# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library("tidyverse");
library("readxl");
library("lubridate")
library("writexl")
library("RSQLite")
library("DBI")
library("httr")
library("glue")
library("jsonlite")
library("rvest")
library("purrr")

# 2.0 Importing Files ----
#connect RSQLite database

con <- RSQLite::dbConnect(drv = SQLite(),
                          dbname ="00_Data/02_chinook/Chinook_Sqlite.sqlite")
#Get lists from Database to variable con
dbListTables(con)

album_tbl <- tbl(con,"Album") %>% collect()

x <- dbGetQuery(con, 'SELECT * From Artist')

glimpse(x)

#Nach fertiger Akquise 
dbDisconnect(con)

Luke_Skywalker <- GET("https://swapi.dev/api/people/1/")

#einigermaßen anzeigen, wie die raw daten in der aus dem Web abgerufenen Variable aussehen
rawToChar(Luke_Skywalker$content)


#Die speicherung vorher ist überflüssig
#Get scheint ebenfalls überflüssig zu sein
Luke <- fromJSON("https://swapi.dev/api/people/1/")


'test1 <- content(Luke_Skywalker, as = "text")'


test2 <- content(Luke_Skywalker, as = "parsed")
test3 <- content(Luke_Skywalker)

alphavantage_key<- Sys.getenv('token') 


resp <- GET('https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE&apikey={alphavantage_key}')
resp

#WEBSRAPING

#select wikipedia as scource



wikipedia_URL <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
movie_URL <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"

resp <- GET(url = "https://www.imdb.com/chart/top/?ref_=nv_mv_250",  
            add_headers('Accept-Language' = "en-US, en;q=0.5")) 
html <- content(resp)


wikipedia <- wikipedia_URL %>%
  read_html() %>%
  html_nodes(css= "#constituents")%>%
  html_table() %>%
  .[[1]]%>%
  as_tibble()



rank2 <- html %>% 
  
  html_nodes(".titleColumn ") %>% 
  html_text()%>%
  stringr::str_extract("(?<= )[0-9]*(?=\\.\\n)")%>% 
  as.numeric()

title2 <- html %>% 
  
  html_nodes(".titleColumn >a") %>% 
  html_text()#%>%
#stringr::str_extract("(?<= )[0-9]*(?=\\.\\n)")%>% 
#as.numeric()


year2 <- html %>% 
  
  html_nodes(".titleColumn .secondaryInfo") %>% 
  html_text()%>%
  stringr::str_extract(pattern = "[0-9]+") %>% 
  as.numeric()

# wichtig an dieser Stelle sind in der Aufgabenstellung code und Klasse Falsch bzw die Klasse kann auf unterschiedliche weise referenziert werden
rating2 <- html %>% 
  
  html_nodes(css = ".ratingColumn > strong")%>%
  html_text()%>%
  as.numeric()


num_ratings2 <- html %>% 
  
  html_nodes(css = ".imdbRating > strong") %>% 
  html_attr('title') %>% 
  # Extract the numbers and remove the comma to make it numeric values
  stringr::str_extract("(?<=based on ).*(?=\ user ratings)" ) %>% 
  stringr::str_replace_all(pattern = ",", replacement = "") %>% 
  as.numeric()


imdb_tbl2 <- tibble(rank2, title2, year2,  rating2, num_ratings2)
glimpse(imdb_tbl2)

#purrr package stuff

bike_data_lst <- fromJSON("00_Data/bikedata.json")
# Open the data by clicking on it in the environment or by running View()
#View(bike_data_lst)
display_values<-bike_data_lst[["productDetail"]][["variationAttributes"]][["values"]][[1]][["displayValue"]]


# canyon analysis

canyon_URL <- "https://www.canyon.com/en-de/"
canyon_fam_URL <- "https://www.canyon.com/en-de/road-bikes/"
canyon_cat_URL <- "https://www.canyon.com/en-de/road-bikes/endurance-bikes/endurace/"

resp_ca <- GET(url = canyon_URL,  
            add_headers('Accept-Language' = "en-US, en;q=0.5")) 
html_ca <- content(resp_ca)


#gotten canyon url stuff and ready for analysis



