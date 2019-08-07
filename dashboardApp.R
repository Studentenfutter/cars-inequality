library(tm)
library(memoise)
# new libs
library(tidyverse)
library(tidytext)
library(shinydashboard)
library(wordcloud2)



sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("First", tabName = "first", icon = icon("th")),
    menuItem("Map", tabName = "map", icon = icon("dashboard"))
  ),
  selectInput(inputId = "dropdown", label = "Car specification:", choices = c('VW-Polo','Opel-Corsa',
                                                                              'Ford-Fiesta', 'Mercedes-Benz-E-Klasse',
                                                                              'BMW-Z4', 'Porsche-911',
                                                                              'Opel-Astra', 'Audi-A3',
                                                                              'VW-Golf','VW-Tiguan',
                                                                              'Audi-Q5','Smart-Fortwo',
                                                                              'Fiat-Panda', 'Renault-Twingo',
                                                                              'Mercedes-Benz-C-Klasse', 
                                                                              'BMW-3er', 'VW-Passat',
                                                                              'Mercedes-Benz-E-Klasse', 
                                                                              'BMW-5er', 'Audi-A6',
                                                                              'Mercedes-Benz-S-Klasse', 'BMW-7er',
                                                                              'Audi-A8')),
  
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
          textOutput("word")
      #     ,
      #     sliderInput(
      #       "freq",
      #       "Minimum Frequency:",
      #       min = 1,
      #       max = 50,
      #       value = 15
      #     ),
      #     sliderInput(
      #       "max",
      #       "Maximum Number of Words:",
      #       min = 1,
      #       max = 300,
      #       value = 100
      #     ),
      #     width = 12
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
server <- function(input, output, session) {

  
  
  observeEvent(input$update, {

    load("data/scraped_data/ebay_clean.rda")
    df <- ebay_clean[ebay_clean$model== input$dropdown]
    
    
    
  })
  
  output$countBox <- renderInfoBox({
    infoBox(
      "Count of tweets scraped", nrow(df), icon = icon("users"),
      color = "purple", fill = TRUE
    )
  })
  
  output$mostFrequentBox <- renderInfoBox({
    infoBox(
      "Region currently scraped", "59071", icon = icon("list"),
      color = "yellow", fill = TRUE
    )
  })
  
  # output$plot <- renderWordcloud2({
  #   # plot it
  #   if (is.null(df$text)) {
  #     NULL
  #   } else{
  #     wordcloud2(df$text)
  #   }
  # })
  output$word <- renderText("hello")
}

# Run the application
shinyApp(ui = ui, server = server)
