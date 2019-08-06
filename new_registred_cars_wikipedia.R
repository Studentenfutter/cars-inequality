# Scrape Wikidata of most popular cars in Germany

library(rvest)
library(tidyverse)
library(ggplot2)

url <- 'https://de.wikipedia.org/w/index.php?title=Liste_der_Neuzulassungen_von_Personenkraftwagen_in_Deutschland_nach_Segmenten_und_Modellreihen'
wikipedia_page <- read_html(url) # get html

cars_overview <- html_node(wikipedia_page, 
                          xpath = '/html/body/div[3]/div[3]/div[4]/div/table[1]')
cars_percentage <- html_node(wikipedia_page,xpath = '/html/body/div[3]/div[3]/div[4]/div/table[2]')

minis <- html_node(wikipedia_page, xpath= '/html/body/div[3]/div[3]/div[4]/div/table[3]')
kleinwagen <- html_node(wikipedia_page, xpath='/html/body/div[3]/div[3]/div[4]/div/table[4]')
kompaktklasse <- html_node(wikipedia_page, xpath='/html/body/div[3]/div[3]/div[4]/div/table[5]')
Mittelklasse <- html_node(wikipedia_page, xpath='/html/body/div[3]/div[3]/div[4]/div/table[6]')
Obere_Mittelklasse <- html_node(wikipedia_page, xpath='/html/body/div[3]/div[3]/div[4]/div/table[7]')
Oberklasse <- html_node(wikipedia_page, xpath='/html/body/div[3]/div[3]/div[4]/div/table[8]')
Geländewagen <- html_node(wikipedia_page, xpath='/html/body/div[3]/div[3]/div[4]/div/table[10]')
Sportwagen <- html_node(wikipedia_page, xpath='/html/body/div[3]/div[3]/div[4]/div/table[12]')
Utilities <- html_node(wikipedia_page, xpath='/html/body/div[3]/div[3]/div[4]/div/table[16]')
Wohnmobile <- html_node(wikipedia_page, xpath='/html/body/div[3]/div[3]/div[4]/div/table[17]')

## Convert to DataFrame
cars_overview <- html_table(cars_overview)
cars_percentage <- html_table(cars_percentage)
minis <- html_table(minis)
kleinwagen <- html_table(kleinwagen)
kompaktklasse <- html_table(kompaktklasse)
Mittelklasse <- html_table(Mittelklasse)
Obere_Mittelklasse <- html_table(Obere_Mittelklasse)
Oberklasse <- html_table(Oberklasse)
Geländewagen <- html_table(Geländewagen)
Sportwagen <- html_table(Sportwagen)
Utilities <- html_table(Utilities)
Wohnmobile <- html_table(Wohnmobile)

# Tidy data
# cars_overview
cars_overview <- cars_overview[1:16,1:19]
names(cars_overview)[names(cars_overview) == "Segment"] <- "Name"

cars_overview <- gather(cars_overview, "1999":"2016",
       key = "year", value = "total_new_registrations", convert = TRUE) 

## Remove dot in number name and convert to numeric
cars_overview[3] <- sapply(cars_overview[3], function(x) as.numeric(gsub("\\.", "", as.character(x))))

# cars_percentage
cars_percentage <- cars_percentage[1:16,] %>%
  gather("1999":"2015",
         key = "year", value = "total_new_registrations", convert = TRUE)

# Minis
minis <- slice(minis, 1:(n()-5) )%>%
  gather("1999":"2018",
         key = "year", value = "total_new_registrations", convert = TRUE)

minis[3] <- sapply(minis[3], function(x) as.numeric(gsub("\\.", "", as.character(x))))

# Kleinwagen
kleinwagen <- slice(kleinwagen, 1:(n()-5) )%>%
  gather("1999":"2018",
         key = "year", value = "total_new_registrations", convert = TRUE)

kleinwagen[3] <- sapply(kleinwagen[3], function(x) as.numeric(gsub("\\.", "", as.character(x))))

# kompaktklasse
kompaktklasse <- slice(kompaktklasse, 1:(n()-5) )%>%
  gather("1999":"2018",
         key = "year", value = "total_new_registrations", convert = TRUE)

kompaktklasse[3] <- sapply(kompaktklasse[3], function(x) as.numeric(gsub("\\.", "", as.character(x))))


# Mittelklasse
Mittelklasse <- slice(Mittelklasse, 1:(n()-5) )%>%
  gather("1999":"2018",
         key = "year", value = "total_new_registrations", convert = TRUE)

Mittelklasse[3] <- sapply(Mittelklasse[3], function(x) as.numeric(gsub("\\.", "", as.character(x))))

# Obere Mittelklasse
Obere_Mittelklasse <- slice(Obere_Mittelklasse, 1:(n()-5) )%>%
  gather("1999":"2018",
         key = "year", value = "total_new_registrations", convert = TRUE)

Obere_Mittelklasse[3] <- sapply(Obere_Mittelklasse[3], function(x) as.numeric(gsub("\\.", "", as.character(x))))

# Oberklasse
Oberklasse <- slice(Oberklasse, 1:(n()-4) )%>% # Only 4, Other is missing
  gather("1999":"2018",
         key = "year", value = "total_new_registrations", convert = TRUE)

Oberklasse[3] <- sapply(Oberklasse[3], function(x) as.numeric(gsub("\\.", "", as.character(x))))

# Sportwagen
Sportwagen <- slice(Sportwagen, 1:(n()-4) )%>% # Only 4, Other is missing
  gather("2007":"2018",
         key = "year", value = "total_new_registrations", convert = TRUE)

Sportwagen[3] <- sapply(Sportwagen[3], function(x) as.numeric(gsub("\\.", "", as.character(x))))

# Wohnmobile
Wohnmobile <- slice(Wohnmobile , 1:(n()-5) )%>% 
  gather("2006":"2018",
         key = "year", value = "total_new_registrations", convert = TRUE)

Wohnmobile[3] <- sapply(Wohnmobile[3], function(x) as.numeric(gsub("\\.", "", as.character(x))))

# Geländewagen
Geländewagen <- slice(Geländewagen , 1:(n()-5) )%>% 
  gather("1999":"2018",
         key = "year", value = "total_new_registrations", convert = TRUE)

Geländewagen[3] <- sapply(Geländewagen[3], function(x) as.numeric(gsub("\\.", "", as.character(x))))

# Utilities
Utilities <- slice(Utilities , 1:(n()-5) )%>% 
  gather("1999":"2018",
         key = "year", value = "total_new_registrations", convert = TRUE)

Utilities[3] <- sapply(Utilities[3], function(x) as.numeric(gsub("\\.", "", as.character(x))))

# Putting all frames in a list 
all_cars <- list(minis, kleinwagen, kompaktklasse, Mittelklasse, Obere_Mittelklasse, Oberklasse,
                 Geländewagen, Sportwagen, Utilities, Wohnmobile)
names(all_cars) <- c("minis", "kleinwagen", "kompaktklasse", "Mittelklasse", "Obere_Mittelklasse", "Oberklasse",
                     "Geländewagen", "Sportwagen", "Utilities", "Wohnmobile")


# Filter for year 2010
all_cars <- lapply(all_cars, function (x) {filter(x, year == 2010) })

## Calculating Top 5 of 2010
all_cars <- lapply(all_cars, function(df){df[order(-df$total_new_registrations),] })
all_cars_top5 <- lapply(all_cars, head)

# save each new data frame as an individual .csv file based on its name

lapply(1:length(all_cars_top5), function(i) write.csv(all_cars_top5[[i]], 
                                                file = paste0("data/", names(all_cars_top5[i]), ".csv"),
                                                row.names = FALSE))
# Plot table nicely with kable
library(knitr)
library(kableExtra)

all_cars_top5$minis %>% kable() %>% kable_styling() # and so on...

# Plot car overview
ggplot(cars_overview) +
  aes(x = year, y = `total_new_registrations`) +
  geom_line(size = 1L, colour = "#35b779") +
  theme_minimal() +
  facet_wrap(vars(Name))
