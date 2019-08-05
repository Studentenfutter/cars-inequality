# Official documentation: https://rstudio.github.io/leaflet/
# plot a map containing the regional info of germany
library(leaflet)

# Add Spatial data - From https://gadm.org/download_country_v3.html
library(raster)
ger <- getData("GADM", country="Germany", level=2) # Change granularity level 1-3, Maybe we get enough detailed data for level 2?
ger$randomData <- rnorm(n=nrow(ger), 150, 30)# Fill with random Data for now

#create a color palette to fill the polygons
pal <- colorQuantile("Greens", NULL, n = 5)

#create a pop up (onClick)
polygon_popup <- paste0("<strong>Name: </strong>", ger$NAME_1, "<br>",
                        "<strong>Indicator: </strong>", round(ger$randomData,2))


# plot map of germany
leaflet(options = leafletOptions(minZoom = 6)) %>% 
  addProviderTiles(provider = "CartoDB") %>%
  setView(lat=51.133333, lng=10.416667, zoom = 6) %>%
  addPolygons(data = ger,
              stroke = T,
              color = "white",
              weight = 2,
              fillColor= ~pal(randomData),
              fillOpacity = 0.5,
              popup = polygon_popup, 
              highlightOptions = highlightOptions(color = "red",
                                                  weight = 3,
                                                  bringToFront = TRUE))

# To plot with shiny - Not working so far
# library(shiny)
# 
# app <- shinyApp(
#   ui <- fluidPage(leafletOutput('myMap', width = "100%", height = 650)),
#   server <- function(input, output) {
#     map <- n
#     output$myMap <- renderLeaflet(map)
#   }
# )
# shinyApp(ui = ui, server = server)
# 
# ## Weiteres Shinybeispiel - Also not Working
# library(shiny)    # for shiny apps
# library(leaflet)  # renderLeaflet function
# library(spData)   # loads the world dataset 
# world <- spData::world
# ui = fluidPage(
#   sliderInput(inputId = "life", "Life expectancy", 49, 84, value = 80),
#   leafletOutput(outputId = "map")
# )
# server = function(input, output) {
#   output$map = renderLeaflet({
#     leaflet() %>% addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
#       addPolygons(data = spData::world[world$lifeExp < input$life, ])})
# }
# shinyApp(ui, server)
