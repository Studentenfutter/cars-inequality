# Cleaning of scraped results
# Import results
library(readr)
ebay <- read_csv("data/ebay_v01.csv")

# Define correcting function
correct_zipcodes <- function (x) {
  if (nchar(x) == 4 ) {
    return(paste0("0", x))
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

match(vector1, vector2, nomatch = NA_integer_, incomparables = NULL)

