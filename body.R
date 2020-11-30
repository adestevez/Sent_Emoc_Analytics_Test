
library("reactable")

body <- dashboardBody(
  tabItems(
    
    ########################
    # First tab content
    ########################
    tabItem(
      tabName = "search",
      fluidRow(
        
        # Application title
        titlePanel("Search"),
        

          box(
            width = 3,
            numericInput("num_tweets_to_download",
                         "Number of tweets to download:",
                         min = 100,
                         max = 18000,
                         value = 200,
                         step = 100),
            textInput("hashtag_to_search",
                      "Keyword to search:",
                      value = "jotalloretv"),
            dateRangeInput("date_picker", label = "Select dates:", start = "2020-07-01", end = "2020-07-17"),
            actionButton("get_data", "Get data", class = "btn-primary"),
            br(),br(),
            downloadButton("download_data", "Download data")
          ),
          
          # Show a plot of the generated distribution
          box(
            width = 9,
            reactableOutput("tweet_table")
          )
        
      )
    ),
    
    ########################
    # Second tab content
    ########################
    tabItem(
      tabName = "dashboard",
      
      fluidRow(
        
        # Application title
      titlePanel("Dashboard"),
      box(
        title = "Time Line", 
        plotOutput("plot1")
      ),
      box(
        title = "Locación",
        plotOutput("plot2")
      ),
      box(
        title = "Tweet Popular",
        reactableOutput("tweet_table2")
      ),
      box(
        title = "Usuarios Populares",
        reactableOutput("tweet_table3")
      )
      
    )
  )
  )
)