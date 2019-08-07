# Cleaning of scraped results
# Import results
library(readr)
library(stringr)
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
ebay <- distinct(ebay,price,zip_code,model, .keep_all= TRUE) %>% 
  filter(price != "1", price != "Zu verschenken") 


ebay$price <- str_sub(ebay$price,1,str_length(ebay$price)-1)
ebay$price <- as.numeric(ebay$price)
ebay <- ebay[!is.na(ebay$price), ]

ebay$price[1] <=105000
# Filter for price above 300 and below 105000
ebay <- ebay %>% filter(price >= 300, price <= 105000)


# Save final results as ebay_clean
ebay_clean <- ebay
save(ebay_clean, file = "data/scraped_data/ebay_clean.rda")
