library(rvest)
library(dplyr)
library(tidyr)
library(stringr)

df_elections <- read_html("https://en.wikipedia.org/wiki/List_of_next_general_elections") |> 
  html_nodes(".wikitable") |> 
  html_table(fill = TRUE) |> 
  bind_rows() |> 
  janitor::clean_names() |> 
  filter(country != "Country") |> 
  rename(
    term_legislative_election = legislative_election_2,
    term_presidential_election = presidential_election_5,
  )

# Remove anything that is not a number
df_election_info <- df_elections |> 
  select(country, starts_with("term"), fairness, in_power_now)

df_election_dates <- df_elections |> 
  select(country, starts_with("presidential"), starts_with("legislative")) |> 
  pivot_longer(c(starts_with("presidential"), starts_with("legislative")), names_to = "type", values_to = "date") |> 
  mutate(type = str_remove(type, "_\\d")) |> 
  filter(str_detect(date, "\\d")) |> 
  mutate(new_date = lubridate::parse_date_time(date, orders = c("Y", "dmY", "mY"))) |> 
  mutate(date = if_else(is.na(new_date), str_remove(date, ".*\\d{1}–"), date)) |> 
  mutate(new_date = if_else(is.na(new_date), lubridate::parse_date_time(date, orders = c("Y", "dmY")), new_date))

