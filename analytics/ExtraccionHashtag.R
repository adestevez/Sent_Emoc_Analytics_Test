# Configure variables: number of tweets to download and hashtag search query
num_tweets_to_download <- 200
hashtag_to_search <- "#rstudioconf"
# Make sure to install any packages listed below that you don't already have on your system:
library("rtweet")
library("reactable")
library("glue")
library("stringr")
library("httpuv")
library("dplyr")
library("purrr")
# Code to actually search for tweets
tweet_df <- search_tweets(hashtag_to_search, n = num_tweets_to_download, include_rts = FALSE)
# select a few desired columns and add a clickable link to tweet text for table data
tweet_table_data <- tweet_df %>%
  select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, urls_expanded_url) %>%
  mutate(
    Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>") 
  )%>%
  select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs = urls_expanded_url)






make_url_html <- function(url) {
  if(length(url) < 2) {
    if(!is.na(url)) {
      as.character(glue("<a title = {url} target = '_new' href = '{url}'>{url}</a>") )
    } else {
      ""
    }
  } else {
    paste0(purrr::map_chr(url, ~ paste0("<a title = '", .x, "' target = '_new' href = '", .x, "'>", .x, "</a>", collapse = ", ")), collapse = ", ")
  }
}
tweet_table_data$URLs <- purrr::map_chr(tweet_table_data$URLs, make_url_html)






reactable::reactable(tweet_table_data, 
                     filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                     showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200), 
                     columns = list(
                       DateTime = colDef(defaultSortOrder = "asc"),
                       User = colDef(defaultSortOrder = "asc"),
                       Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                       Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                       RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
                       URLs = colDef(html = TRUE)
                     )
)