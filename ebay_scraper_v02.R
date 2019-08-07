library(tidyverse)
library(rvest)
library(stringr)

name_list <- list( list('VW-Polo', 'kleinwagen'), list('Opel-Corsa','kleinwagen'),
                   list('Ford-Fiesta', 'kleinwagen'), list('Mini-6','kleinwagen'),
                   list('Peugeout-207','kleinwagen'), list('Fiat-Ducato','Wohnmobil'),
                   list('Ford-Transit','Wohnmobil'), list('VW-Transporter','Wohnmobil'),
                   list('Iveco-Daily','Wohnmobil'), list('Mercedes-Benz-Sprinter','Wohnmobil'),
                   list('Citroen-Jumper','Wohnmobil'), list('VW-Transporter','Utilities'),
                   list('VW-Caddy','Utilities'), list('Citroen-Berlingo','Utilities'),
                   list('Renault-Kangoo','Utilities'), list('Ford-Transit-Turneo','Utilities'),
                   list('Mercedes-Benz-Vito','Utilities'), list('Mercedes-Benz-E-Klasse','Sportwagen'),
                   list('BMW-Z4','Sportwagen'), list('Porsche-911','Sportwagen'),
                   list('Audi-TT','Sportwagen'), list('Mercedes-Benz-SLK','Sportwagen'),
                   list('Porsche-Boxter','Sportwagen'), list('Opel-Astra', 'kompaktklasse'),
                   list('Audi-A3', 'kompaktklasse'), list('BMW-1er', 'kompaktklasse'),
                   list('Ford-Focus', 'kompaktklasse'), list('Mercedes-Benz-A-Klasse', 'kompaktklasse'),
                   list('VW-Tiguan', 'gelaendewagen'), list('BMW-X1', 'gelaendewagen'),
                   list('Audi-Q5', 'gelaendewagen'), list('Ford-Kuga', 'gelaendewagen'),
                   list('Skoda-Yeti', 'gelaendewagen'), list('Mercedes-Benz-GLK', 'gelaendewagen'),
                   list('Smart-Fortwo', 'minis'), list('Fiat-Panda', 'minis'),
                   list('Renault-Twingo', 'minis'), list('Fiat-500', 'minis'),
                   list('Hyundai-i10', 'minis'), list('Ford-Ka', 'minis'),
                   list('Mercedes-Benz-C-Klasse', 'mittelklassse'), list('BMW-3er', 'mittelklassse'),
                   list('VW-Passat', 'mittelklassse'), list('Audi-A4', 'mittelklassse'),
                   list('Opel-Insignia', 'mittelklassse'), list('Audi-A5', 'mittelklassse'),
                   list('Mercedes-Benz-E-Klasse', 'obere_mittelklasse'), list('BMW-5er', 'obere_mittelklasse'),
                   list('Audi-A6', 'obere_mittelklasse'), list('Volvo-S70', 'obere_mittelklasse'),
                   list('Jaguar-XF', 'obere_mittelklasse'), list('Chrysler-300C', 'obere_mittelklasse'),
                   list('Mercedes-Benz-S-Klasse', 'oberklasse'), list('BMW-7er', 'oberklasse'),
                   list('Audi-A8', 'oberklasse'), list('Porsche-Panamera', 'oberklasse'),
                   list('VW-Phaeton', 'oberklasse'), list('Mercedes-Benz-CLS', 'oberklasse')
)

city_list <- list( list('s-stuttgart', 'k0l9280r50'), 
                   list("s-jena", "k0l3770r50"),
                   list("s-hamburg","k0l9440r50"),
                   list('s-muenchen','k0l6411r50'),
                   list('s-magdeburg','k0l2227r50'),
                   list('s-deggendorf','k0l5985r50'),
                   list('s-duesseldorf','k0l2068r50'),
                   list('s-cottbus','k0l7743r50')
)

pages_to_scrape = 3



# base URL for Ebay Kleinanzeigen 
url1 <- "https://www.ebay-kleinanzeigen.de/s-autos"

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
        model_list <- c(model_list,toString(names[[name]][[1]]))
        group_list <- c(group_list,toString(cities[[name]][[2]]))
        location_list <- c(location_list,toString(cities[[city]][[1]]))
      }
    }
  }


# Returns the list of completed urls
  result <- c(urls, model_list, group_list, location_list)

  return(result)
}

urls <- make_urls(name_list,city_list)


# Returns the list of individual car links to scrape
css_nodes <- list()
for (i in 1:length(urls)/4)
   css_nodes <- c(css_nodes,html_nodes(read_html(toString(urls[[i]][1])),css = '.ellipsis'))

hrefs <- list()
for(i in 1:length(css_nodes))
  hrefs <- c(hrefs,html_attr(css_nodes[[i]], 'href'))
hrefs

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
  return(df)
  
}


#
base_url <- 'https://www.ebay-kleinanzeigen.de'



# model set up
df <- data.frame(id=NA,model=NA,price=NA,kilometer=NA, registration_date=NA, condition = NA, zip_code= NA, text = NA, place = NA)


df0 <- data.frame(id=NA,model=NA,price=NA,kilometer=NA, registration_date=NA, condition = NA, zip_code= NA, text = NA, place = NA)

# loop to actually scrape data
for (i in 1:length(hrefs)) {
  temp <- ebay_scraper(paste(base_url,hrefs[i],sep=""), df0)
  df <- rbind(df, temp)
  rm("temp")
  Sys.sleep(1)
}


df <- distinct(df,price,zip_code,model, .keep_all= TRUE)
df <- df[!is.na(df$kilometer), ]
df

