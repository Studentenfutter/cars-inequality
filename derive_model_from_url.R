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
                   list('Audi-A8', 'oberklasse'))


df$car <- NA

for (i in 1:length(name_list)){
  df$car <- ifelse(grepl(tolower(toString(name_list[[i]][1])), df$url), tolower(name_list[[i]][1]), df$car)
  print(tolower(toString(name_list[[i]][1])))
}


