###################
# server.R
# 
# For all your server needs 
###################
library("shinydashboard")
library("rtweet")
library("reactable")
library("glue")
library("stringr")
library("httpuv")
library("dplyr")
library("purrr")

library("shiny")



# Define server logic 
server <- function(input, output) {

  source('./functions.R')
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste(input$hashtag_to_search, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(tweet_table_data(), file, row.names = FALSE)
    }
  )
  
  tweet_df <- eventReactive(input$get_data, {
    search_tweets(input$hashtag_to_search, n = input$num_tweets_to_download, include_rts = FALSE)
  })
  
  tweet_table_data <- reactive({
    req(tweet_df())
    tweet_df() %>%
      select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, urls_expanded_url) %>%
      mutate(
        Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
        URLs = purrr::map_chr(urls_expanded_url, make_url_html)
      )%>%
      select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs)
  })
  
  output$tweet_table <- renderReactable({
    reactable::reactable(tweet_table_data(), 
                         filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                         showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 5, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200), 
                         columns = list(
                           DateTime = colDef(defaultSortOrder = "asc"),
                           User = colDef(defaultSortOrder = "asc"),
                           Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                           Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                           RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
                           URLs = colDef(html = TRUE)
                         )
    )
  })
  

  
  output$plot1 <- renderPlot({
    tweets=tweet_df()
    ts_plot(tweets, "minutes")
  })
  
  output$plot2 <- renderPlot({
    tweets=tweet_df()
    tweets %>%
      filter(location != "", !is.na(location)) %>% 
      count(location) %>% 
      top_n(10, n) %>% 
      ggplot() +
      geom_col(aes(x = reorder(location, n), y = n)) + 
      coord_flip() +
      labs(title = "Procedencia de los usuarios",
           x = "ubicacin",
           y = "cantidad")
    
  })
  
  
  
  
  
  tweet_table_data2 <- reactive({
    req(tweet_df())
    tweet_df() %>%
      filter(!is_retweet) %>% 
      filter(retweet_count == max(retweet_count)) %>% 
      select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, urls_expanded_url) %>%
      mutate(
        Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>")
      )%>%
      select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count)
  })
  
  output$tweet_table2 <- renderReactable({
    reactable::reactable(tweet_table_data2(), 
                         filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                         showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200), 
                         columns = list(
                           DateTime = colDef(defaultSortOrder = "asc"),
                           User = colDef(defaultSortOrder = "asc"),
                           Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                           Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                           RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE))
                         )
    )
  })
  
  
  tweet_table_data3 <- reactive({
    req(tweet_df())
    tweet_df() %>%
      top_n(5, followers_count) %>% 
      arrange(desc(followers_count)) %>%  
      select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, urls_expanded_url, followers_count, location) %>%
      mutate(
        Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
        URLs = purrr::map_chr(urls_expanded_url, make_url_html)
      )%>%
      select(DateTime = created_at, User = screen_name, Likes = favorite_count, RTs = retweet_count, Seguidores=followers_count, Locacion=location,)
  })
  
  output$tweet_table3 <- renderReactable({
    reactable::reactable(tweet_table_data3(), 
                         filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                         showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200), 
                         columns = list(
                           User = colDef(defaultSortOrder = "asc"),
                           Seguidores = colDef(defaultSortOrder = "asc"),
                           Locacion = colDef(defaultSortOrder = "asc")
                         )
    )
  })
  
  
 
  
  
  
}