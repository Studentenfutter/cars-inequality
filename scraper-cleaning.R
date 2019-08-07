# Cleaning of scraped results
# Import results
library(readr)
ebay <- read_csv("data/ebay_v01.csv")

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
# Apply correcting function
ebay$zip_code <- map_chr(ebay$zip_code, correct_zipcodes)

# Convert Zip Codes to RegionalIDs
load("data/plz_codes.Rda") # Load codes dataframe
codes

for (i in ebay$zip_code) {
      x <- match(ebay$zip_code[i], codes$plz)
      print(x)
}

# Join both data frames
codes <- codes %>% rename(zip_code = PLZ)
ebay$zip_code <- as.numeric(ebay$zip_code)
ebay <- ebay %>% left_join(codes, by = 'zip_code')
