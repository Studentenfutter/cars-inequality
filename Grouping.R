
rm(list = ls())
#####
library(janitor)
library(tidyverse)
##### Import ebay_clean from scraper-cleaning.R
load("data/scraped_data/ebay_clean.rda")


ebay_grouped <- ebay_clean %>%
  group_by(RegionalID, model) %>% 
  summarize(
    "avg.price" = mean(price),
    "median.price" = median(price),
    "sd.price" = sd(price)
  ) 

# Convert to wide format to join columns with leaflet ger
ebay_avg_price_per_class <- ebay_grouped[,1:3] %>% spread(model, avg.price)

ebay_location <- ebay_clean %>%
  group_by(RegionalID) %>% 
  summarize(
    "avg.price" = mean(price),
    "median.price" = median(price),
    "sd.price" = sd(price),
  ) 

ebay_ads_per_location <- ebay_clean %>% group_by(RegionalID) %>% tally()


ebay_ads_per_location <- ebay_clean %>%
  group_by(RegionalID, model) %>%
  summarize(
    "avg.price" = mean(price),
    "median.price" = median(price),
    "sd.price" = sd(price)
  )

# Save Output 
save(ebay_grouped, ebay_avg_price_per_class, ebay_location, ebay_ads_per_location, ebay_ads_per_location, file = "data/scraped_data/ebay_grouped.rda")

##### Sample Data

df <- read_excel("data/sample.xls", sheet = 2)
df <- clean_names(df)

#####

pr_class <-
  df %>%
  group_by(model_group, model_name) %>%
  summarize(
    "avg.price" = mean(price), # mean
    "median.price" = median(price),
    "sd.price" = sd(price) # sd
  )

pr_location <-
  df %>%
  group_by(county_id) %>%
  summarize(
    "avg.price" = mean(price),
    "median.price" = median(price),
    "sd.price" = sd(price)
  )


pr_location_class <-
  df %>%
  group_by(county_id, model_group, model_name) %>%
  summarize(
    "avg.price" = mean(price),
    "median.price" = median(price),
    "sd.price" = sd(price)
  )

# Delete everything thats above 2 times the mean or median sd?


