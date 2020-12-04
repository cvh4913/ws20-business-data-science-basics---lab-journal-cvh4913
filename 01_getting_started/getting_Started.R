# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library("tidyverse");
library("readxl");
library("lubridate")
library("writexl")

# 2.0 Importing Files ----
bike_data<-read_excel("00_Data/01_bike_sales/01_raw_data/bikes.xlsx");
shop_data<-read_excel("00_Data/01_bike_sales/01_raw_data/bikeshops.xlsx");
order_data<-read_excel("00_Data/01_bike_sales/01_raw_data/orderlines.xlsx");

# 3.0 Examining Data ----
#order_data

# 4.0 Joining Data ----
glimpse(order_data)
whole_data<-left_join(order_data,bike_data,by =c("product.id"="bike.id"));
whole_data<-left_join(whole_data,shop_data,by =c("customer.id"="bikeshop.id"));

# 5.0 Wrangling Data ---- 
whole_data <-whole_data %>% 
  mutate(total.price=price*quantity)%>%
  select(order.id, contains("order"), contains("model"), contains("category"),price, quantity, total.price,everything())%>%
  
  separate(col="category", into=c("C1", "C2", "C3"), sep = " - ")%>%
  separate(col="location", into=c("City","State"),sep =", ")


whole_data<-whole_data%>%select(-...1,-gender,-url) %>%

  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))
glimpse(whole_data)


# 6.0 Business Insights ----
#rearrange data
#goal -> have the data plottable

sb_year<-whole_data %>%
    select(order_date,total_price);


  
# 6.1 Sales by Year ----


# Step 1 - Manipulate
sb_year<-sb_year %>% mutate(year=year(order_date))%>%
  select(-order_date)%>%
  group_by(year)%>%
  summarize(sales=sum(total_price))%>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"));
# Step 2 - Visualize


# 6.2 Sales by Year and Category 2 ----

sb_cat<-whole_data %>%
  select(C1,total_price,order_date);

# Step 1 - Manipulate

sb_cat<-sb_cat%>% 
  mutate(year=year(order_date))%>%
  select(-order_date)%>%
  
  
 
  group_by(year,C1)%>%
  summarize(sales=sum(total_price))%>%
  ungroup()%>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"));





# 6.3 Challenge Assignement Cities and States ----


sales_by_Cities<-whole_data %>%
  select(City,total_price,State,order_date)%>%
  mutate(year=year(order_date))%>%
  select(-order_date)%>%
  group_by(City,State,year)%>%
  summarize(sales=sum(total_price))%>%
  ungroup()%>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"));

sales_by_State<-whole_data%>%
  select(total_price,State,order_date)%>%
  mutate(year=year(order_date))%>%
  select(-order_date)%>%
  group_by(State,year)%>%
  summarize(sales=sum(total_price))%>%
  ungroup()%>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"));







# Step 2 - Visualize



# Visualize the challenge task ----




sales_by_Cities %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = City)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  
  # Facet
  facet_wrap(~ City) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  
  labs(
    title = "Revenue by year and cities",
    subtitle = "The Trends in different cities are very different",
    fill = "Main category" # Changes the legend name
    
  )

# Visualize the challenge task ----




sales_by_State %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = State)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  
  # Facet
  facet_wrap(~ State) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  
  labs(
    title = "Revenue by year and state",
    subtitle = "The Trends in different states with North-Rhine-Westphalia leading ahead of Bremen",
    fill = "Main category" # Changes the legend name
    
  )


# 7.0 Writing Files ----




# 7.1 Excel ----
sb_year %>%
  write_xlsx("00_data/01_bike_sales/02_wrangled_data/sales_by_year.xlsx")
sb_cat %>%
  write_xlsx("00_data/01_bike_sales/02_wrangled_data/sales_by_year_and_Category_1.xlsx")

# 7.2 CSV ----
sb_year %>% 
  write_csv("00_data/01_bike_sales/02_wrangled_data/sales_by_year.csv")
sb_cat %>%
  write_csv("00_data/01_bike_sales/02_wrangled_data/sales_by_year_and_Category_1.csv")

# 7.3 RDS ----
sb_year %>% 
  write_rds("00_data/01_bike_sales/02_wrangled_data/sales_by_year.rds")
sb_cat %>%
  write_rds("00_data/01_bike_sales/02_wrangled_data/sales_by_year_and_Category_1.rds")

