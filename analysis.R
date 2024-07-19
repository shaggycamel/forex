
library(tidyverse)
library(slider)
library(plotly)
library(nba.dataRub)

db_con <- dh_createCon("cockroach")


# Data prep ---------------------------------------------------------------

mean_diff_rank_calc <- function(df, interval){
  df |> 
    mutate(
      !!paste0("rate_mean_", interval) := slide_mean(rate, before = interval, complete = TRUE),
      !!paste0("rate_lag_", interval) := lag(rate, interval),
      .by = c(base_cur, conversion_cur)
    ) |> 
    mutate(
      !!paste0("perc_rate_diff_", interval) :=
        (rate - !!sym(paste0("rate_lag_", interval))) /
          ((rate + !!sym(paste0("rate_lag_", interval))) / 2)
    ) |> 
    mutate(
      !!paste0("perc_diff_rank_", interval) := dense_rank(!!sym(paste0("perc_rate_diff_", interval)) * -1),
      .by = c(base_cur, date)
    )
}


df_rates <- dh_getQuery(db_con, "anl_query.sql") |> 
  mean_diff_rank_calc(1) |> 
  mean_diff_rank_calc(7) |> 
  mean_diff_rank_calc(30) |> 
  mean_diff_rank_calc(100)



# Plot --------------------------------------------------------------------

bs_c <- "NZD"
cv_c <- "MMK"

plt_df <- filter(df_rates, base_cur == bs_c, conversion_cur == cv_c)

plot_ly(plt_df, x = ~date, y = ~rate, name = 'daily', type = 'scatter', mode = 'lines+markers') |> 
  add_trace(y = ~rate_mean_7, name = '7 day ma', mode = 'lines') |> 
  add_trace(y = ~rate_mean_30, name = '30 day ma', mode = 'lines') |> 
  add_trace(y = ~rate_mean_100, name = '100 day ma', mode = 'lines') |> 
  layout(
    title = list(text = paste(bs_c, cv_c, sep = "/"), x = 0.08, y = 1.1),
    yaxis = list(title = "Rate"),
    xaxis = list(rangeslider = list(type = "date")),
    hovermode="x unified"
  )


