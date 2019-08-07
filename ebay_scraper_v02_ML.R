library(tidyverse)
library(rvest)
library(stringr)
library(plyr)

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
                   list('Audi-A8', 'oberklasse')
)

city_list <- list( list('s-stuttgart', 'k0l9280r50'), 
                   list("s-jena", "k0l3770r50")
)

pages_to_scrape = 5



# base URL for Ebay Kleinanzeigen 
url1 <- "https://www.ebay-kleinanzeigen.de/s-autos/"

#helper function to get links from car specifications and locations
make_urls <- function(names, cities){
  # List of urls created
  urls <- list()
  model_list <- list()
  group_list <- list()
  location_list <- list()
  
  for (name in 1:length(name_list)){
    for (city in 1:length(cities)){
      for (i in 1:pages_to_scrape){

        url <- paste(url1,cities[[city]][[1]],"/anzeige:angebote/","seite:",toString(i),"/",names[[name]][[1]],"/",cities[[city]][[2]], sep = "")
        urls <- c(urls, url)
      }
    }
  }


# Returns the list of completed urls
  result <- c(urls, model_list, group_list, location_list)

  return(result)
}

urls <- make_urls(name_list,city_list)
urls


# Returns the list of individual car links to scrape
css_nodes <- list()
for (i in 1:length(urls)/4)
   css_nodes <- c(css_nodes,html_nodes(read_html(toString(urls[[i]][1])),css = '.ellipsis'))

hrefs <- list()
for(i in 1:length(css_nodes))
  hrefs <- c(hrefs,html_attr(css_nodes[[i]], 'href'))
hrefs
length(hrefs)
for (i in 1:30){
  duplicate_check<- list()
  for (i in 1:length(hrefs)){
    duplicate_check <- c(duplicate_check,str_sub(hrefs[i],-18,-1))
    if(duplicated(duplicate_check)[i]) {
      hrefs <- hrefs[-i]
    }
  }
}
length(hrefs)

# scraper function given a url and a dataframe to scrape all entries and return attribute information
ebay_scraper <- function(url, df){
  ebay_html <- read_html(url)
  
  attr_keys <- ebay_html %>% 
    html_nodes('.attributelist-striped') %>% 
    html_nodes('.attributelist--key') %>% 
    html_text
  
  attr_values <- ebay_html %>% 
    html_nodes('.attributelist-striped') %>% 
    html_nodes('.attributelist--value') %>% 
    html_text %>% 
    gsub("[\r\n]", "",.) %>% 
    gsub(" ", "",.)
  
  result <- list(attr_keys,attr_values)
  
  
  
  price <- ebay_html %>%
    html_nodes('#viewad-price') %>%
    html_text %>% 
    gsub("[\r\n]", "",.) %>% 
    gsub(" ", "",.) %>% 
    gsub("VB", "",.)
  price
  
  text <- ebay_html %>%
    html_nodes('#viewad-description-text') %>% 
    html_text %>% 
    gsub("[\r\n]", "",.)
  attr_values[3]
  attr_values[1]
  
  df[1,"id"] <- attr_values[3]
  df[1,'model'] <- attr_values[9]
  df[1,"price"] <- str_sub(price,7,)
  df[1,'registration_date'] <- attr_values[7]
  df[1,'condition'] <- attr_values[13]
  df[1,'kilometer'] <- attr_values[6]
  df[1,'place'] <- attr_values[1]
  df[1,'zip_code'] <- str_sub(attr_values[1],1,5)
  df[1,'text'] <- text
  df[1, 'url'] <- url
  return(df)
  
}


#
base_url <- 'https://www.ebay-kleinanzeigen.de'



# model set up
df <- data.frame(id=NA,model=NA,price=NA,kilometer=NA, registration_date=NA, condition = NA, zip_code= NA, text = NA, place = NA, url = NA)


df0 <- data.frame(id=NA,model=NA,price=NA,kilometer=NA, registration_date=NA, condition = NA, zip_code= NA, text = NA, place = NA, url = NA)

# loop to actually scrape data
for (i in 1:length(hrefs)) {
  temp <- ebay_scraper(paste(base_url,hrefs[i],sep=""), df0)
  df <- rbind(df, temp)
  rm("temp")
  Sys.sleep(1)
}


