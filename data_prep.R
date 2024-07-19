
library(tidyverse)
library(quantmod)
library(nba.dataRub)

observed_currencies <- c("NZD", "AUD", "USD")

db_con <- dh_createCon("cockroach")

# Currency table ----------------------------------------------------------

df_currencies <- oanda.currencies |> 
  rownames_to_column() |> 
  rename(symbol = 1, country = 2)

df_dates_from <- dh_getQuery(db_con, "dates_from.sql")


# Get rates ---------------------------------------------------------------

get_rates <- possibly(
  
  function(observed_currency, compare_currency, currencies){  
  
    pair_cur <- paste0(observed_currency, "/", compare_currency)
    from_date <- filter(df_dates_from, base_cur == observed_currency, conversion_cur == compare_currency) |> 
      pull(date)
    
    base_ix <- match(observed_currency, observed_currencies)-1
    grp_ix <- match(compare_currency, currencies)
    pos_ix <- base_ix * length(currencies) + grp_ix
    cat(paste0(pair_cur, ": ", pos_ix, " of ", length(currencies) * length(observed_currencies), "\n"))
    # Sys.sleep(3)
    
    getFX(pair_cur, from = from_date + 1, auto.assign = FALSE) |> 
      as.data.frame() |>
      rownames_to_column(var = "date") |>
      mutate(base_cur = observed_currency, conversion_cur = compare_currency) |>
      rename(rate = 2)
    
  }

)


df_rates <- map(observed_currencies, \(obs_cur){
  
  compare_currencies <- df_currencies |> 
    filter(symbol != obs_cur) |> 
    pull(symbol)
  
  map(compare_currencies, \(x) get_rates(obs_cur, x, compare_currencies)) |> 
    compact() |> 
    bind_rows()
  
}) |> 
  bind_rows() |>
  mutate(date = as.Date(date))


# Write to database -------------------------------------------------------

dh_ingestData(db_con, df_rates, "forex", "rates", append=TRUE)
