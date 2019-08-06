# Official documentation: https://rstudio.github.io/leaflet/
# plot a map containing the regional info of germany
library(leaflet)

# Add Spatial data - From https://gadm.org/download_country_v3.html
library(raster)
ger <- getData("GADM", country="Germany", level=2) # Change granularity level 1-3, Maybe we get enough detailed data for level 2?

# Match with car theft data from carthefts.R
ger$total_carthefts <- crimes_counties$`erfasste Fälle`
ger$relative_carthefts <- crimes_counties$`HZ nach Zensus`

#create a color palette to fill the polygons
pal <- colorQuantile("Greens", NULL, n = 5)

#create a pop up (onClick)
polygon_popup <- paste0("<strong>Name: </strong>", crimes_counties$`Stadt-/Landkreis`, "<br>",
                        "<strong>Fälle: </strong>", crimes_counties$`erfasste Fälle`)


# plot map of total carthefts per region
leaflet(options = leafletOptions(minZoom = 6)) %>% 
  addProviderTiles(provider = "CartoDB") %>%
  setView(lat=51.133333, lng=10.416667, zoom = 6) %>%
    # Add absolute crime rates
  addPolygons(data = ger,
              group = "Absolute",
              stroke = T,
              color = "white",
              weight = 2,
              fillColor= ~pal(ger$total_carthefts),
              label = ~paste0(ger$TYPE_2, " ", ger$NAME_2, ": ", ger$total_carthefts),
              fillOpacity = 0.5,
              popup = polygon_popup, 
              highlightOptions = highlightOptions(color = "red",
                                                  weight = 3,
                                                  bringToFront = TRUE)) %>%
    # Add relative crime rates
  addPolygons(data = ger,
              group = "Relative",
              stroke = T,
              color = "white",
              weight = 2,
              fillColor= ~pal(ger$relative_carthefts),
              label = ~paste0(ger$TYPE_2, " ", ger$NAME_2, ": ", ger$total_carthefts),
              fillOpacity = 0.5,
              popup = polygon_popup, 
              highlightOptions = highlightOptions(color = "red",
                                                  weight = 3,
                                                  bringToFront = TRUE)) %>%
    # Add interactive controls
  addLayersControl(
    baseGroups = c("Absolute", "Relative"),
    options = layersControlOptions(collapsed = FALSE)
  )

# plot map of relative carthefts per region
relative_carthefts <- leaflet(options = leafletOptions(minZoom = 6)) %>% 
  addProviderTiles(provider = "CartoDB") %>%
  setView(lat=51.133333, lng=10.416667, zoom = 6) %>%
  addPolygons(data = ger,
              stroke = T,
              color = "white",
              weight = 2,
              fillColor= ~pal(ger$relative_carthefts),
              label = ~paste0(ger$TYPE_2, " ", ger$NAME_2, ": ", ger$total_carthefts),
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
