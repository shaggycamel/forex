
library(tidyverse)
library(slider)
library(plotly)

df_rates <- read_csv("rates.csv") |> 
  arrange(base_cur, conversion_cur, date) |> 
  mutate(
    rate_mean_30 = slide_mean(rate, before = 30, complete = TRUE),
    rate_mean_7 = slide_mean(rate, before = 7, complete = TRUE),
    rate_lag_30 = lag(rate, 30),
    rate_lag_7 = lag(rate, 7),
    rate_lag_1 = lag(rate, 1),
    .by = c(base_cur, conversion_cur)
  ) |> 
  mutate(
    perc_rate_diff_30 = (rate - rate_lag_30) / ((rate + rate_lag_30) / 2),
    perc_rate_diff_7 = (rate - rate_lag_7) / ((rate + rate_lag_7) / 2),
    perc_rate_diff_1 = (rate - rate_lag_1) / ((rate + rate_lag_1) / 2)
  ) |> 
  mutate(
    perc_diff_rank_30 = dense_rank(perc_rate_diff_30 * -1),
    perc_diff_rank_7 = dense_rank(perc_rate_diff_7 * -1),
    perc_diff_rank_1 = dense_rank(perc_rate_diff_1 * -1),
    .by = c(base_cur, date)
  )


# Plot --------------------------------------------------------------------

bs_c <- "NZD"
cv_c <- "MMK"

plt_df <- filter(df_rates, base_cur == bs_c, conversion_cur == cv_c)

ggplot(plt_df, aes(x = date, y = rate)) +
  geom_path() +
  geom_point() +
  geom_path(aes(y = rate_mean_30), colour = "red") +
  geom_path(aes(y = rate_mean_7), colour = "blue") +
  theme_bw()

# plotly slider



# RATE OF INCREASE --------------------------------------------------------


df_rates <- df_rates |> 
  group_by(base_cur, conversion_cur) |> 
  mutate()
