
# Initialisation  -----------------------------------------------------------

observed_currencies <- c("NZD", "AUD", "USD", "EUR", "GBP", "JPY", "ZAR", "CNY", "BRL", "INR")

db_con <- nba.dataRub::dh_createCon("cockroach")

# Currency table ----------------------------------------------------------

df_currencies <- quantmod::oanda.currencies |> 
  tibble::rownames_to_column() |> 
  dplyr::rename(symbol = 1, country = 2)

df_dates_from <- nba.dataRub::dh_getQuery(db_con, "dates_from.sql")

# Get rates ---------------------------------------------------------------

get_rates <- purrr::possibly(
  
  function(observed_currency, compare_currency, currencies){  
  
    pair_cur <- paste0(observed_currency, "/", compare_currency)
    from_date <- (dplyr::filter(df_dates_from, base_cur == observed_currency, conversion_cur == compare_currency))$date
   
    base_ix <- match(observed_currency, observed_currencies)-1
    grp_ix <- match(compare_currency, currencies)
    pos_ix <- base_ix * length(currencies) + grp_ix
    cat(paste0(pair_cur, ": ", pos_ix, " of ", length(currencies) * length(observed_currencies), "\n"))
    # Sys.sleep(3)
   
    quantmod::getFX(pair_cur, from = from_date + 1, auto.assign = FALSE) |> 
      as.data.frame() |>
      tibble::rownames_to_column(var = "date") |>
      dplyr::mutate(base_cur = observed_currency, conversion_cur = compare_currency) |>
      dplyr::rename(rate = 2)
    
  }

)


df_rates <- purrr::map(observed_currencies, \(obs_cur){
  
  compare_currencies <- setdiff(df_currencies$symbol, obs_cur)

  purrr::map(compare_currencies, \(x) get_rates(obs_cur, x, compare_currencies)) |> 
    purrr::compact() |> 
    dplyr::bind_rows()
  
}) |> 
  dplyr::bind_rows() |> 
  dplyr::mutate(date = as.Date(date)) |> 
  dplyr::select(base_cur, conversion_cur, date, rate)


# Write to database -------------------------------------------------------

# Rates
nba.dataRub::dh_ingestData(db_con, df_rates, "forex", "rates", append = TRUE)

# Error log
df_error_log <- tibble::tibble(base_cur = observed_currencies) |> 
  dplyr::cross_join(dplyr::select(df_currencies, conversion_cur = symbol)) |> 
  dplyr::filter(base_cur != conversion_cur) |> 
  dplyr::anti_join(df_rates, by = dplyr::join_by(base_cur, conversion_cur)) |> 
  dplyr::arrange(base_cur, conversion_cur) |> 
  dplyr::mutate(update_date = Sys.Date()) |>
  dplyr::left_join(df_dates_from, by = dplyr::join_by(base_cur, conversion_cur)) |> 
  dplyr::rename(max_pair_conversion_date = date)

if(nrow(df_error_log) > 0) nba.dataRub::dh_ingestData(db_con, df_error_log, "forex", "error_log", append = TRUE)


# Update log
df_update_log <- tibble::tibble(update_date = Sys.Date(), max_conversion_date = max(df_rates$date))

nba.dataRub::dh_ingestData(db_con, df_update_log, "forex", "update_log", append = TRUE)
