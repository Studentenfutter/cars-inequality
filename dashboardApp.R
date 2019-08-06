library(tm)
library(wordcloud)
library(memoise)
# new libs
library(twitteR)
library(tidyverse)
library(tidytext)
library(shinydashboard)
library(wordcloud2)
library(openssl)
library(httpuv)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("First", tabName = "first", icon = icon("th")),
    menuItem("Map", tabName = "map", icon = icon("dashboard"))
  ),
  
  textInput("selection", "Input your search term:",
            ""),
  actionButton("update", "Change"),
  hr()
)

body <-  dashboardBody(
  tabItems(
  # First Tab
    tabItem(
      tabName = "first",
      h2("General word count statistics"),
      fluidRow(
        infoBoxOutput("countBox"),
        infoBoxOutput("mostFrequentBox"),
        tags$style("#countBox {width:400px;}"),
        tags$style("#mostFrequentBox {width:400px;}"),
        box(
          wordcloud2Output("plot", height = 350),
          sliderInput(
            "freq",
            "Minimum Frequency:",
            min = 1,
            max = 50,
            value = 15
          ),
          sliderInput(
            "max",
            "Maximum Number of Words:",
            min = 1,
            max = 300,
            value = 100
          ),
          width = 12
        )
      )
    ),
  # Second Tab
    tabItem(
      tabName = "map",
      h2("Plot a map"),
      fluidRow(
        infoBoxOutput("countBox"),
        infoBoxOutput("mostFrequentBox"),
        tags$style("#countBox {width:400px;}"),
        tags$style("#mostFrequentBox {width:400px;}"),
        box(
          wordcloud2Output("plot", height = 350),
          sliderInput(
            "freq",
            "Minimum Frequency:",
            min = 1,
            max = 50,
            value = 15
          ),
          sliderInput(
            "max",
            "Maximum Number of Words:",
            min = 1,
            max = 300,
            value = 100
          ),
          width = 12
        )
      )
    )
  )
)


ui <- dashboardPage(# Application title
  dashboardHeader(title = "Cars and regional inequalities"),
  sidebar,
  body)
#Define server logic

api_key <- "Qo7VeCp9vkTttVeJImiL3e729"
api_secret  <- "7bOG4V2wpu5ms2Tqu09BOcuPQ281NvRMSo0GF9eGBscazGYfR7"
access_token <- "1155914249992097794-YtPSjWWiIedQM5hMLj5LMiFQW99OGc"
access_secret <- "bfrP4BSxR0eQxYr7ygU5M3WBuNS0W8kgpDj1FUOcbIlR9"

server <- function(input, output, session) {
  session$onSessionEnded(stopApp) # Stop on Window closing
  tweets_clean <- reactiveValues(df = NULL)
  # Define a reactive expression for the document term matri
  #Here we are creating the "handshake" with Twitter
  
  setup_twitter_oauth(
    access_token = access_token ,
    access_secret = access_secret,
    consumer_key = api_key,
    consumer_secret = api_secret
  )
  
  observeEvent(input$update, {
    tw <-
      searchTwitter(
        input$selection,
        n = input$max,
        lang = 'en',
        resultType = "recent"
      )
    # tweets to df so we could use tidytext
    df <- twListToDF(tw)
    # use dplyr and tidytext to clean your tweets
    data(stop_words)
    tweets_clean$df <- df %>%
      dplyr::select(text) %>%
      # filter(!word %in% stop_words$word) %>%
      tidytext::unnest_tokens(word, text) %>%
      anti_join(get_stopwords()) %>%
      filter(!word %in% c(input$selection, 'https', 'rt')) %>%
      count(word, sort = TRUE)
  })
  
  output$countBox <- renderInfoBox({
    infoBox(
      "Count of tweets scraped", nrow(tweets_clean$df), icon = icon("users"),
      color = "purple", fill = TRUE
    )
  })
  
  output$mostFrequentBox <- renderInfoBox({
    infoBox(
      "Region currently scraped", "59071", icon = icon("list"),
      color = "yellow", fill = TRUE
    )
  })
  
  output$plot <- renderWordcloud2({
    # plot it
    if (is.null(tweets_clean$df)) {
      NULL
    } else{
      wordcloud2(tweets_clean$df)
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
