############################################
# GET AMERICA COUNTRIES LIST ---------------
############################################
# This script scrapes names from America's countries.
# Author: Henrique Gomide

require(XML)
library(RCurl)

theurl <- getURL("https://en.wikipedia.org/wiki/List_of_countries_in_the_Americas_by_population")
tables <- readHTMLTable(theurl)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
info <- tables[[which.max(n.rows)]]
country_list <- as.character(info[,2])
country_list <- gsub("\\(.*$","" , country_list)
country_list <- substring(country_list, 2)
country_list <- country_list[1:55]
country_list <- paste0("(",country_list, ") OR")
writeLines(country_list, "countryList")

