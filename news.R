
library(newsapi)
library(dplyr)
library(purrr)
library(stringr)

df_news <- filter(news_sources(), language == "en")$id |> 
  map(\(x) every_news(source = x, since = as.Date("2024-07-29"))) |> 
  bind_rows() |> 
  filter(content != "[Removed]", !str_detect(source, "[[:upper:]]")) |> 
  janitor::clean_names()


df_news |> 
  filter(content != "[Removed]", !str_detect(source, "[[:upper:]]")) |> 
  janitor::clean_names() |> 
  write.csv("news_tmp.csv", row.names = FALSE)
