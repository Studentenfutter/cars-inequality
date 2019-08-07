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
load("data/plz_codes.Rda") # Load codes dataframe

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

# Extract Car Names from URLs
# ['VW-Golf', 'kompaktklasse'],
# ['Opel-Astra', 'kompaktklasse'],
# ['Audi-A3', 'kompaktklasse'],
# ['BMW-1er', 'kompaktklasse'],
# ['Ford-Focus', 'kompaktklasse'],
# ['Mercedes-Benz-A-Klasse', 'kompaktklasse'],
# ['VW-Tiguan', 'gelaendewagen'],
# ['BMW-X1', 'gelaendewagen'],
# ['Audi-Q5', 'gelaendewagen'],
# ['Ford-Kuga', 'gelaendewagen'],
# ['Skoda-Yeti', 'gelaendewagen'],
# ['Mercedes-Benz-GLK', 'gelaendewagen'],
# ['Smart-Fortwo', 'minis'],
# ['Fiat-Panda', 'minis'],
# ['Renault-Twingo', 'minis'],
# ['Fiat-500', 'minis'],
# ['Hyundai-i10', 'minis'],
# ['Ford-Ka', 'minis'],
# ['Mercedes-Benz-C-Klasse', 'mittelklassse'],
# ['BMW-3er', 'mittelklassse'],
# ['VW-Passat', 'mittelklassse'],
# ['Audi-A4', 'mittelklassse'],
# ['Opel-Insignia', 'mittelklassse'],
# ['Audi-A5', 'mittelklassse'],
# ['Mercedes-Benz-E-Klasse', 'obere_mittelklasse'],
# ['BMW-5er', 'obere_mittelklasse'],
# ['Audi-A6', 'obere_mittelklasse'],
# ['Volvo-S70', 'obere_mittelklasse'],
# ['Jaguar-XF', 'obere_mittelklasse'],
# ['Chrysler-300C', 'obere_mittelklasse'],
# ['Mercedes-Benz-S-Klasse', 'oberklasse'],
# ['BMW-7er', 'oberklasse'],
# ['Audi-A8', 'oberklasse'],
# ['Porsche-Panamera', 'oberklasse'],
# ['VW-Phaeton', 'oberklasse'],
# ['Mercedes-Benz-CLS', 'oberklasse']


## Data Filtering
# Remove everything for 1€ or "Zu verschenken" - also below value x?
ebay <- ebay[!is.na(ebay$kilometer), ]
ebay <- ebay[!(ebay$price == ""),]
ebay <- distinct(ebay,price,zip_code,model, .keep_all= TRUE) %>% 
  filter(price != "1", price != "Zu verschenken") 

ebay$price <- as.numeric(ebay$price)
ebay <- ebay %>% filter(price >= 300)


# Save final results as ebay_clean
ebay_clean <- ebay
save(ebay_clean, file = "data/scraped_data/ebay_clean.rda")
