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
Bikeshops_tbl<- read_excel("00_Data/01_bike_sales/01_raw_data/bikeshops.xlsx")
bike_orderlines_tbl <- left_join(bike_orderlines_tbl,bikedata,by=c("product.id"="bike.id"))
bike_orderlines_tbl <- left_join(bike_orderlines_tbl,Bikeshops_tbl,by=c("customer.id"="bikeshop.id"))
#3.1
bike_orderlines_tbl <-bike_orderlines_tbl%>%
  mutate(freight_costs = weight*2 )%>%
  mutate(total_price=price*quantity)%>%
  rename(bikeshop = name)

#3.2
#bike_orderlines_tbl%>%
#  mutate(total_price = log(total_price) )%>%
#  View()

#3.3 strive
#bike_orderlines_tbl%>%
  #mutate(is_strive = model %>% str_to_lower() %>% str_detect("strive")) %>%
  #filter(is_strive)%>% #weird
#View()  

bike_orderlines_tbl %>%
  mutate(price_binned = ntile(total_price, 3)) %>% 
  select(total_price, price_binned, everything())%>%
  arrange(desc(price_binned))%>%
  filter(price_binned >1)%>%
  View()

#4

#5

bikeshop_revenue_tbl <- bike_orderlines_tbl %>%
  select(bikeshop, category_1, total_price) %>%
  
  group_by(bikeshop, category_1) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  arrange(desc(sales))

bikeshop_revenue_formatted_tbl <- bikeshop_revenue_tbl %>%
  pivot_wider(names_from  = category_1,
              values_from = sales) %>%
  mutate(
    Mountain = scales::dollar(Mountain, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    Gravel = scales::dollar(Gravel, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    Road     = scales::dollar(Road, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    `Hybrid / City` = scales::dollar(`Hybrid / City`, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    `E-Bikes` = scales::dollar(`E-Bikes`, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €")
  )

bikeshop_revenue_formatted_tbl_long<-bikeshop_revenue_formatted_tbl %>%
  pivot_longer(cols           = c(names(.)[2:6]),
               names_to       = "category_1",
               values_to      = "sales",
               values_drop_na = T) %>%
  mutate(sales =  sales %>% str_remove_all("€|\\.") %>% as.double())



