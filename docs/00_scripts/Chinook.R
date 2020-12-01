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




