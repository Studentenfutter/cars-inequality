# Cleaning of scraped results
# Import results
library(readr)
library(stringr)
library(tidyverse)
library(readxl)
# Load scraped data
load("data/scraped_data/ebay.rda")

ebay <- df

## PLZ

# # Define correcting function
# correct_zipcodes <- function (x) {
#   if (nchar(x) == 4 ) {
#     return(paste0("0", x))
#   } else if (is.na(x)) {
#     return(NA)
#   } else {
#     print(as.character(x))
#   }
# }
# 
# # Apply correcting function
# ebay$zip_code <- map_chr(ebay$zip_code, correct_zipcodes)

# Delete everything in zip_code thats not a number of 4 or more digits
ebay$zip_code <- as.numeric(stringr::str_extract_all(as.numeric(ebay$zip_code), "(\\d{4,})"))

# read PLZ from place column
ebay$zip_extracted <- as.numeric(stringr::str_extract(ebay$place, "(\\d{5})"))

# Convert Zip Codes to RegionalIDs
load("data/other_data/plz_codes.Rda") # Load codes dataframe

# for (i in ebay$zip_extracted) {
#       x <- match(ebay$zip_extracted[i], codes$PLZ)
#       print(x)
# }

# Join both data frames
codes <- codes %>% rename(zip_extracted = PLZ)
ebay <- ebay %>% left_join(codes, by = 'zip_extracted')

# Clean and Prepare values
ebay$price <- gsub("€", "", ebay$price) # Remove € and whitespace
ebay$price <- gsub("\\.", "", ebay$price) # Remove .

ebay$kilometer <- gsub("\\.", "", ebay$kilometer) # Remove . from kilometer

# Rename unclear car categories
ebay$model <- gsub("(\\d{2,})", "Uneindeutiges Modell (Beziner)", ebay$model)
ebay$model <- gsub("(\\d{2,})", "Uneindeutiges Modell", ebay$model)
ebay$model <- gsub("2/3", "Uneindeutiges Modell", ebay$model)
ebay$model <- gsub("Schiebedach", "Uneindeutiges Modell", ebay$model)

# Extract Car Names from URLs

name_list <- list( list('VW-Polo', 'kleinwagen'), list('Opel-Corsa','kleinwagen'),
                   list('Ford-Fiesta', 'kleinwagen'), list('Mercedes-Benz-E-Klasse','Sportwagen'),
                   list('BMW-Z4','Sportwagen'), list('Porsche-911','Sportwagen'),
                   list('Opel-Astra', 'kompaktklasse'), list('Audi-A3', 'kompaktklasse'),
                   list('VW-Golf', 'kompaktklasse'), list('VW-Tiguan', 'gelaendewagen'),
                   list('BMW-X1', 'gelaendewagen'), list('Audi-Q5', 'gelaendewagen'),
                   list('Smart-Fortwo', 'minis'), list('Fiat-Panda', 'minis'),
                   list('Renault-Twingo', 'minis'), list('Mercedes-Benz-C-Klasse', 'mittelklassse'),
                   list('BMW-3er', 'mittelklassse'), list('VW-Passat', 'mittelklassse'),
                   list('Mercedes-Benz-E-Klasse', 'obere_mittelklasse'), list('BMW-5er', 'obere_mittelklasse'),
                   list('Audi-A6', 'obere_mittelklasse'),
                   list('Mercedes-Benz-S-Klasse', 'oberklasse'), list('BMW-7er', 'oberklasse'),
                   list('Audi-A8', 'oberklasse'))
ebay$car <- NA
for (i in 1:length(name_list)){
  ebay$car <- ifelse(grepl(tolower(toString(name_list[[i]][1])), ebay$url), tolower(name_list[[i]][1]), ebay$car)
}

## Data Filtering
# Remove everything for 1€ or "Zu verschenken" - also below value x?
ebay <- ebay[!is.na(ebay$kilometer), ]
ebay <- ebay[!(ebay$price == ""),]
ebay <- distinct(ebay,price,zip_code,model, .keep_all= TRUE) 

ebay$price <- str_sub(ebay$price,1,str_length(ebay$price)-1)
ebay$price <- as.numeric(ebay$price)
ebay <- ebay[!is.na(ebay$price), ]
ebay <- ebay[!is.na(ebay$RegionalID), ]

ebay$price[1] <=105000
# Filter for price above 300 and below 105000
ebay <- ebay %>% 
  filter(price != "1", price != "Zu verschenken") %>% 
  filter(price >= 300, price <= 105000) %>% 
  filter(registration_date >= 1950, registration_date <= 2019) %>% 
  filter(as.numeric(kilometer) >= 500)

ebay$age <- 2019 - as.numeric(ebay$registration_date) # Calcuate age of car
ebay <- ebay %>%  distinct(text, .keep_all= TRUE)

ebay$kilometer <- ebay$kilometer %>% as.numeric()


# data merging
load("data/other_data/external.data2.Rda")
ebay <- left_join(ebay, ext3, by = "RegionalID")
ebay$gdppc.2017 <- ebay$gdppc.2017 %>% as.numeric()

names(theft[3])
theft <- read.csv("data/other_data/theft.csv")
theft$new_key <- paste0("0", stringr::str_extract(theft$Gemeinde.schl�.ssel, "(\\d{4})"))
ebay <- left_join(ebay, theft, by = c("RegionalID" = "new_key"))

cities <- read_excel("data/other_data/plz_five_cities.xlsx", sheet = 2)

ebay$zip_extracted <- ebay$zip_extracted %>% 
  as.numeric()

cities$zip <- cities$zip %>% 
  as.numeric()

cities <- cities %>% 
  na.omit()

ebay <- left_join(ebay, cities, by = c("zip_extracted" = "zip")) %>% distinct(text, .keep_all= TRUE)


#outlier detection with boxplots
outlier_values <- boxplot.stats(ebay$price)$out  # outlier values.
boxplot(ebay$price, main="Price", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

boxplot(price ~ RegionalID, data=ebay, main="Prices across regions")

mod <- lm(price ~ RegionalID, data=ebay)
cooksd <- cooks.distance(mod)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 6*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>6*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

influential <- as.numeric(names(cooksd)[(cooksd > 6*mean(cooksd, na.rm=T))])
influential <- na.omit(influential)
influential

ebay <- ebay[-influential,]


# Save final results as ebay_clean
ebay_clean <- ebay
save(ebay_clean, file = "data/scraped_data/ebay_clean.rda")
