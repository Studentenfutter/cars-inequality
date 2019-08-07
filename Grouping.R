
rm(list = ls())
#####
library(janitor)
library(tidyverse)
##### Import real df ebay from scraper-cleaning.R


ebay_grouped <- ebay_clean %>%
  group_by(RegionalID, model) %>% 
  summarize(
    "avg.price" = mean(price),
    "median.price" = median(price),
    "sd.price" = sd(price)
  ) 

ebay_location <- ebay_clean %>%
  group_by(RegionalID) %>% 
  summarize(
    "avg.price" = mean(price),
    "median.price" = median(price),
    "sd.price" = sd(price),
  ) 

ebay_ads_per_location <- ebay_clean %>% group_by(RegionalID) %>% tally()


classes_region <- ebay %>%
  group_by(RegionalID, model_group) %>%
  summarize(
    "avg.price" = mean(price),
    "median.price" = median(price),
    "sd.price" = sd(price)
  )


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


