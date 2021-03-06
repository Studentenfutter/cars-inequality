# Official documentation: https://rstudio.github.io/leaflet/
# plot a map containing the regional info of germany
library(leaflet)
library(tidyverse)
# Add Spatial data - From https://gadm.org/download_country_v3.html
library(raster)
ger <- getData("GADM", country="Germany", level=2) # Change granularity level 1-3, Maybe we get enough detailed data for level 2?

# Match with car theft data from carthefts.R
# TODO: Fill and match with left_join and @ operatr

# Map for gpd per county
load("data/other_data/external.data2.Rda")
load("data/scraped_data/ebay_grouped.rda")
load("data/other_data/crimes_counties.rda")

# Change name for Regional ID to CC_2
colnames(ext3)[which(names(ext3) == "RegionalID")] <- "CC_2"
colnames(ebay_location)[which(names(ebay_location) == "RegionalID")] <- "CC_2"
colnames(ebay_ads_per_location)[which(names(ebay_ads_per_location) == "RegionalID")] <- "CC_2"
colnames(ebay_avg_price_per_class)[which(names(ebay_avg_price_per_class) == "RegionalID")] <- "CC_2"
###
#### Join data
###
#crimes_counties_cleaned <- crimes_counties[,c(12,18,19)]
# TODO: Fill and match with left_join and @ operatr
ger$total_carthefts <- crimes_counties$`erfasste Fälle`
ger$relative_carthefts <- crimes_counties$`HZ nach Zensus`

#ger@data <- left_join(ger@data, crimes_counties_cleaned, by = "CC_2") # Join Crime Data
ger@data <- left_join(ger@data, ext3, by = "CC_2") # Join GDP data
ger@data <- left_join(ger@data, ebay_location, by= "CC_2") # Join Car prices per Region
ger@data <- left_join(ger@data, ebay_ads_per_location, by= "CC_2") # Join Titel number of ads per Region
ger@data <- left_join(ger@data, ebay_avg_price_per_class, by= "CC_2") # Join Tital number of ads per Region
ger@data$bip.2016 <- as.numeric(ger@data$bip.2016)
ger@data$bip.2017 <- as.numeric(ger@data$bip.2017)
ger@data$gdppc.2016 <- as.numeric(ger@data$gdppc.2016)
ger@data$gdppc.2017 <- as.numeric(ger@data$gdppc.2017)

# Wieviele Autos kann man sich zum durchschn. Gebrauchtwagenpreis von einem durchschnittlichen pro Kopf einkommen pro Kopf kaufen?
ger@data$avg.price.with.pek <- ger@data$pek.2016 / ger@data$avg.price

# Import PLZ data
load("data/other_data/plz_codes.Rda") # Import codes


#create a color palette to fill the polygons
pal <- colorQuantile("Greens", NULL, n = 5)
hot <- colorQuantile("Reds", ger$avg.price, n = 5, na.color = "#F2F2F2")
gdp <- colorQuantile("Reds", ger$gdppc.2017, n = 5, na.color = "#F2F2F2")

#create a pop up (onClick)
# TODO: Add Table row format!
popup_car <- paste0("<strong><center>", ger$TYPE_2, " ", ger$NAME_2, "</center></strong><br>",
                        "<strong>Average price per car: </strong>", round(as.numeric(ger$avg.price)), "€<br>",
                        "<strong>Standard deviation: </strong>", round(as.numeric(ger$sd.price)), "<br>",
                        "<strong>Average prices for</strong><br>",
                        "<strong>Small cars: </strong>", round(as.numeric(ger$Kleinwagen)), "€<br>",
                        "<strong>SUVs: </strong>", round(as.numeric(ger$`SUV/Geländewagen`)), "€<br>",
                        "<strong>Other categories: </strong>", round(as.numeric(ger$`Uneindeutiges Modell`)), "€<br>",
                        "<strong>GDP per capita 2017: </strong>", round(as.numeric(ger$gdppc.2017)), " million € per year<br>")

polygon_popup  <- paste0("<strong>Name: </strong>", ger$NAME_2, "<br>",
                        "<strong>Absolute Fälle: </strong>", ger$total_carthefts, "<br>",
                        "<strong>Relative Fälle: </strong>", ger$relative_carthefts, "<br>",
                        "<strong>GDP 2016: </strong>", ger$bip.2016, "<br>",
                        "<strong>GDP 2017: </strong>", ger$bip.2017, "<br>")

# Save files for plotting in shiny
save(ger, pal, hot, popup_car, polygon_popup, file = "data/other_data/map.rda")

# plot map of total carthefts per region
my_map <- leaflet(options = leafletOptions(minZoom = 6)) %>% 
  addProviderTiles(provider = "CartoDB") %>%
  setView(lat=51.133333, lng=10.416667, zoom = 6) %>%
    # Add absolute crime rates
  addPolygons(data = ger,
              group = "Absolute car thefts",
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
              group = "Relative car thefts",
              stroke = T,
              color = "white",
              weight = 2,
              fillColor= ~pal(ger$relative_carthefts),
              label = ~paste0(ger$TYPE_2, " ", ger$NAME_2, ": ", ger$relative_carthefts),
              fillOpacity = 0.5,
              popup = polygon_popup, 
              highlightOptions = highlightOptions(color = "red",
                                                  weight = 3,
                                                  bringToFront = TRUE)) %>%
  # Add PEK 2016
  addPolygons(data = ger,
              group = "PEK 2016",
              stroke = T,
              color = "white",
              weight = 2,
              fillColor= ~pal(ger$pek.2016),
              label = ~paste0(ger$TYPE_2, " ", ger$NAME_2, ": ", ger$pek.2016),
              fillOpacity = 0.5,
              popup = polygon_popup,
              highlightOptions = highlightOptions(color = "red",
                                                  weight = 3,
                                                  bringToFront = TRUE)) %>%
  # Add GDP per Capita 2017
  addPolygons(data = ger,
              group = "GDP per capita 2017",
              stroke = T,
              color = "white",
              weight = 2,
              fillColor= ~gdp(ger$gdppc.2017),
              label = ~paste0(ger$gdppc.2017 ,"€ (", ger$NAME_2, ")"),
              fillOpacity = 0.5,
              popup = polygon_popup,
              highlightOptions = highlightOptions(color = "red",
                                                  weight = 3,
                                                  bringToFront = TRUE)) %>%
 # Add avg price data
  addPolygons(data = ger,
              group = "Average car price",
              stroke = T,
              color = "white",
              weight = 2,
              fillColor= ~hot(ger$avg.price),
              label = ~paste0(round(as.numeric(ger$avg.price)) ,"€ (", ger$NAME_2, ")"),
              fillOpacity = 0.5,
              popup = popup_car,
              highlightOptions = highlightOptions(color = "red",
                                                  weight = 3,
                                                  bringToFront = TRUE)) %>%
  addLegend("bottomright", group = "Average car price", pal = hot, values = round(as.numeric(ger$avg.price)),
            title = "Avg. Car Price") %>%
    # Add interactive controls
  addLayersControl(
    baseGroups = c("Average car price", "Absolute car thefts", "Relative car thefts", "PEK 2016", "GDP per capita 2017"),
    options = layersControlOptions(collapsed = FALSE) #%>% 
      #hideGroup("GDP per capita 2017")
  )


# plot map of relative carthefts per region
# relative_carthefts <- leaflet(options = leafletOptions(minZoom = 6)) %>% 
#   addProviderTiles(provider = "CartoDB") %>%
#   setView(lat=51.133333, lng=10.416667, zoom = 6) %>%
#   addPolygons(data = ger,
#               stroke = T,
#               color = "white",
#               weight = 2,
#               fillColor= ~pal(ger$relative_carthefts),
#               label = ~paste0(ger$TYPE_2, " ", ger$NAME_2, ": ", ger$total_carthefts),
#               fillOpacity = 0.5,
#               popup = polygon_popup, 
#               highlightOptions = highlightOptions(color = "red",
#                                                   weight = 3,
#                                                   bringToFront = TRUE))
# 
# 
# ## Add car data!
# leaflet(options = leafletOptions(minZoom = 6)) %>% 
#   addProviderTiles(provider = "CartoDB") %>%
#   setView(lat=51.133333, lng=10.416667, zoom = 6) %>%
#   # Add avg price data
#   addPolygons(data = ger,
#               group = "Absolute",
#               stroke = T,
#               color = "white",
#               weight = 2,
#               fillColor= ~hot(ger$avg.price),
#               label = ~paste0("Ads in ", ger$NAME_2, ": ", ger$n),
#               fillOpacity = 0.5,
#               popup = polygon_popup, 
#               highlightOptions = highlightOptions(color = "red",
#                                                   weight = 3,
#                                                   bringToFront = TRUE)) %>%
#   addLegend("bottomright", pal = hot, values = ger$avg.price,
#             title = "Avg. Car Price")
# 

# To plot with shiny - Not working so far
library(shiny)

app <- shinyApp(
  ui <- fluidPage(leafletOutput('myMap', width = "100%", height = 650)),
  server <- function(input, output) {
    map <- n
    output$myMap <- renderLeaflet(map)
  }
)
shinyApp(ui = ui, server = server)

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
