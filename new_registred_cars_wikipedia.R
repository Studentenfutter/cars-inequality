# Scrape Wikidata of most popular cars in Germany

library(rvest)
library(tidyverse)

url <- 'https://de.wikipedia.org/w/index.php?title=Liste_der_Neuzulassungen_von_Personenkraftwagen_in_Deutschland_nach_Segmenten_und_Modellreihen'
wikipedia_page <- read_html(url) # get html

cars_overview <- html_node(wikipedia_page, 
                          xpath = '/html/body/div[3]/div[3]/div[4]/div/table[1]')
cars_percentage <- html_node(wikipedia_page,xpath = '/html/body/div[3]/div[3]/div[4]/div/table[2]')

## Convert to DataFrame
cars_overview <- html_table(cars_overview)
cars_percentage <- html_table(cars_percentage)

# Tidy data
# cars_overview
cars_overview <- cars_overview[1:16,1:19]
names(cars_overview)[names(cars_overview) == "Segment"] <- "Name"

cars_overview <- gather(cars_overview, "1999":"2016",
       key = "year", value = "Total New Registrations", convert = TRUE) 

## Remove dot in number name and convert to numeric
cars_overview[3] <- sapply(cars_overview[3], function(x) as.numeric(gsub("\\.", "", as.character(x))))

# cars_percentage
cars_percentage <- cars_percentage[1:16,] %>%
  gather("1999":"2015",
         key = "year", value = "Total New Registrations", convert = TRUE)

# Plot car overview
ggplot(cars_overview) +
  aes(x = year, y = `Total New Registrations`) +
  geom_line(size = 1L, colour = "#35b779") +
  theme_minimal() +
  facet_wrap(vars(Name))
