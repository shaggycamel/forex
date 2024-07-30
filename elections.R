
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
  select(country, starts_with("term"), fairness, in_power_now) |> 
  mutate(across(starts_with("term"), \(x) str_remove(str_remove(x, "\\(.*\\)"), "\\D"))) |> 
  mutate(across(starts_with("term"), \(x) as.integer(x))) |> 
  mutate(across(where(is.character), \(x) str_squish(x))) |> 
  mutate(across(where(is.character), \(x) na_if(x, "")))


df_election_dates <- df_elections |> 
  select(country, starts_with("presidential"), starts_with("legislative")) |> 
  pivot_longer(c(starts_with("presidential"), starts_with("legislative")), names_to = "type", values_to = "date_string") |> 
  filter(str_detect(date_string, "\\d")) |> 
  mutate(type = str_remove(type, "_\\d")) |> 
  mutate(date_string = str_replace(date_string, "[[:punct:]]", "-")) |> 
  mutate(date_string = str_remove(date_string, ".*\\d{1}-")) |> 
  mutate(date = lubridate::parse_date_time(date_string, orders = c("Y", "dmY", "mY"))) |> 
  mutate(year = lubridate::year(date))


# Eventually create procedure to combine with existing date data and remove duplicates