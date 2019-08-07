library(tidyverse)
library(rvest)
library(stringr)

name_list = list(list('fiat-panda','kleinwagen'))

city_list = list(list('s-muenchen', 'k0l6411'))
pages_to_scrape = 2




#helper function to get links from car specifications and locations
make_urls <- function(names, cities){
  # base URL for Ebay Kleinanzeigen 
  url1 <- "https://www.ebay-kleinanzeigen.de/s-autos/"
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

for (i in 1:10){
  duplicate_check<- list()
  for (i in 1:length(hrefs)){
    duplicate_check <- c(duplicate_check,str_sub(hrefs[i],-18,-1))
    if(duplicated(duplicate_check)[i]) {
      hrefs <- hrefs[-i]
    }
  }
}
hrefs









# test
url <- toString(urls[[1]][1])
url <- 'https://www.ebay-kleinanzeigen.de/s-autos/s-muenchen/anzeige:angebote/seite:1/fiat-panda/k0l6411'
html <- read_html(url)
html
css_nodes <- html_nodes(html, css = '.ellipsis')
css_nodes
urls <-  html_attr(css_nodes, 'href')
urls[1:20]

