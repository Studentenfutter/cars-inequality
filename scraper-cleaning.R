# Cleaning of scraped results
# Import results
library(readr)
library(stringr)
# ebay <- read_csv("data/ebay_v01.csv")
ebay <- df


## PLZ

# Define correcting function
correct_zipcodes <- function (x) {
  if (nchar(x) == 4 ) {
    return(paste0("0", x))
  } else if (is.na(x)) {
    return(NA)
  } else {
    print(as.character(x))
  }
}
# Delete everything in zip_code thats not a number of 4 or more digits
ebay$zip_code <- as.numeric(stringr::str_extract_all(as.numeric(ebay$zip_code), "(\\d{4,})"))

# Apply correcting function
ebay$zip_code <- map_chr(ebay$zip_code, correct_zipcodes)

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
ebay$model <- gsub("(\\d{2,})", "Uneindeutiges Modell", ebay$model)


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
