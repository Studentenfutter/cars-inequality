# Scrape car thefts per Landkreis
theft_url <- "https://www.bka.de/SharedDocs/Downloads/DE/Publikationen/PolizeilicheKriminalstatistik/2018/BKATabellen/FaelleLaenderKreiseStaedte/BKA-LKS-F-04-T01-Kreise-Fallentwicklung_csv.csv"
curl::curl_download(gini_url, "data/theft.csv")
# Files have been manually cleaned

# Import ger-Spatial data-frame from leaflet.R
# load("leaflet.R") - to load ger vector

theft <- read_csv("data/theft.csv")

counties <- as.data.frame(ger)

# Calculate Missings 

theft$`Gemeinde-schlüssel` %in% counties$CC_2
setdiff(theft$`Gemeinde-schlüssel`, counties$CC_2)

# Fix error in Göttingen, from 03152 to 03159 in ger
counties$CC_2[counties$CC_2 == "03152"] <- "03159"
# Change Name of CC2
colnames(counties)[which(names(counties) == "Gemeindeschlüssel")] <- "CC_2"
colnames(theft)[which(names(theft) == "Gemeinde-schlüssel")] <- "CC_2"

# Join row of cases with the countries dataset both datasets
crimes_counties <- left_join(counties, theft, by = "CC_2") #Join matching rows from theft to counties
crimes_counties <- full_join(theft, counties, by = "CC_2")
crimes_counties <- arrange(crimes_counties, CC_2)
