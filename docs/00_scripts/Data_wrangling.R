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
library("xopen")
library("stringi")


#read saved bike data from xlsx

bikedata<- read_excel("00_Data/01_bike_sales/01_raw_data/bikes.xlsx")

#select columns in different ways

biketbl_1 <- bikedata%>%
  select(bike.id,model,model.year)

biketbl_2 <- bikedata%>%
  select(1:3)

biketbl_3 <- bikedata%>%
  select(1,contains("model"))

# selected sccessfully

model_and_price<- bikedata%>%
  select("model","price")


bikedata%>%
  select(contains("category"),everything())

mean_price<-bikedata%>%
  pull(price)%>%
  
  mean()

char <- bikedata%>%
  select(where(is.character))

numerical <- bikedata%>%
  select(where(is.numeric))

nonnumerical <- bikedata%>%
  select(!where(is.numeric))

bikedata <- bikedata%>%
  separate(col =category, into=c("category_1", "category_2", "category_3"), sep= " - ")

bikedata1_7<- bikedata%>%
  select(model, contains("category"), price)%>%
  rename(
    Model = model,
    'Bike Familiy' = category_1,
    'Ride Style'= category_2,
    'Bike Category' = category_3,
    'Price in Euro' = price
    
  
  )

bikedata1_8<- bikedata%>%
  select(model, contains("category"), price)%>%
  set_names(c("Model", "Bike Family", "Bike Ridestyle","Bike Category","Price in Euro"))


 #2

bikedata%>%
  select(model, price)%>%
  arrange(desc(price))%>%
  glimpse()

bikedata%>%
  select(model, price)%>%
  arrange(desc(price))%>%
  filter(price<mean(price))%>%
  glimpse()

#Dieser Befehl benötigt ein Und und funktioniert wie im script angegeben mit oder nicht

bikedata%>%
  select(model, price)%>%
  arrange(desc(price))%>%
  filter((price >5000)&(price< 10000))%>%
  glimpse()


bikedata%>%
  select(model, price)%>%
  arrange(desc(price))%>%
  filter((price >5000)&(model%>%str_detect("Endurace")))
  #view()

  #aus den nächste beiden nehme ich mit, dass der default wert von arrange ascending ist
bikedata %>%
  arrange(desc(price)) %>%
  slice(1:5)%>%
  glimpse()

bikedata %>%
  arrange(price) %>%
  slice(1:5)%>%
  glimpse()

#3 

bike_orderlines_tbl <- read_excel("00_Data/01_bike_sales/01_raw_data/orderlines.xlsx")


