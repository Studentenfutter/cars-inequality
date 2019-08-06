library(tm)
library(wordcloud)
library(memoise)
# new libs
library(twitteR)
library(tidyverse)
library(tidytext)
library(wordcloud2)


ui <- fluidPage(
  # Application title
  titlePanel("Word Cloud"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      textInput("selection", "Input your search term:",
                ""),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 15),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100)
    ),
    
    # Show Word Cloud
    mainPanel(
      wordcloud2Output("plot", height = "400px")
    )
  )
)
#Define server logic

api_key <- "Qo7VeCp9vkTttVeJImiL3e729"
api_secret  <- "7bOG4V2wpu5ms2Tqu09BOcuPQ281NvRMSo0GF9eGBscazGYfR7"
access_token <- "1155914249992097794-YtPSjWWiIedQM5hMLj5LMiFQW99OGc"
access_secret <- "bfrP4BSxR0eQxYr7ygU5M3WBuNS0W8kgpDj1FUOcbIlR9"

server <- function(input, output, session) {
  tweets_clean <- reactiveValues(df = NULL)
  # Define a reactive expression for the document term matri
  #Here we are creating the "handshake" with Twitter
  setup_twitter_oauth(access_token = access_token ,access_secret = access_secret,
                      consumer_key = api_key,consumer_secret = api_secret )
  
  observeEvent(input$update,{
    
    tw <- searchTwitter(input$selection, n=input$max, lang='en', resultType = "recent")
    # tweets to df so we could use tidytext
    df <- twListToDF(tw)
    # use dplyr and tidytext to clean your tweets
    tweets_clean$df <- df %>%
      dplyr::select(text) %>%
      tidytext::unnest_tokens(word, text) %>%
      count(word, sort = TRUE) 
  })
  output$plot <- renderWordcloud2({
    # plot it
    if(is.null(tweets_clean$df)){
      NULL
    } else{
      wordcloud2(tweets_clean$df)
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)