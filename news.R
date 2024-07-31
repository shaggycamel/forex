
library(newsapi)

newsapi::top_headlines(country="AU") |> View()

newsapi::every_news(
  "Olympics",
  since = as.Date("2024-07-20"),
  language = "en"
) |> View()
