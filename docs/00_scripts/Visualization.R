# load Librarys

library("tidyverse");
library("readxl");
library("lubridate")
library("writexl")
library("ggthemes")

#load data from file 

data<-readRDS("00_data/01_bike_sales/02_wrangled_data/bikedata.rds")

#Exercise 1, plot sales over year

sales_data<-data%>%
  select(price,order_date,quantity)%>%
  mutate(total_price=price*quantity)%>%
  #get the year from the order date
  mutate(year=year(order_date))%>%
  select(-order_date)%>%
  group_by(year)%>%
  summarize(sales=sum(total_price))%>%
  ungroup()%>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
# Step 2: Plot ----

sales_data %>%
  
  #2.1 Canvas
  ggplot(aes(x = year, y = sales, color = sales))

# Without piping 
ggplot(data = sales_data, 
       aes(x     = year, 
           y     = sales, 
           color = sales))+
#This is the first step of a graphi. Tha canvas is setup, axes are defined and labeled
#2.2 Adding geometries

geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE)+

geom_label(label =  "Major Demand This Year",
           vjust = -0.5, 
           size  = 5,
           fill  = "#1f78b4",
           color = "white",
           fontface = "italic",
           data = sales_data %>%
             filter(year %in% c(2019))) +
  
  expand_limits(y=1.6e7)+
  expand_limits(x=2020)


# second example

# Data Manipulation
revenue_by_month_tbl <- data %>%
  
  select(order_date, total_price) %>%
  
  mutate(year_month = floor_date(order_date, "months") %>% ymd()) %>%
  
  group_by(year_month) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

# Line Plot
revenue_by_month_tbl %>%
  
  ggplot(aes(year_month, revenue)) +
  
  geom_line(size = 0.5, linetype = 1) +
  geom_smooth(method = "loess", span = 0.2)


#third example
sales_data %>%
  
  # Canvas
  ggplot(aes(x = year, y = sales, color = sales)) +
  
  # Geometries 
  geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE) +
  
  # same as above, with explicit scales
  scale_x_continuous() +
  scale_y_continuous() +
 
  scale_color_continuous(low    = "red", high = "black", 
                         labels = scales::dollar_format(scale  = 1/1e6, 
                                                        prefix = "", 
                                                        suffix = "M €")) +
  scale_y_continuous(labels = scales::dollar_format(scale  = 1/1e6, 
                                                    prefix = "", 
                                                    suffix = "M €")) +
  
  # Formatting
  expand_limits(y = 0) +
  # You can also type "red", "black" etc. for the colors
  
  labs(
    title = "Revenue",
    subtitle = "Sales are trending up and to the right!",
    x = "",
    y = "Sales (Millions)",
    color = "Rev (M €)",
    caption = "What's happening?\nSales numbers showing year-over-year growth."
  )+
  
  theme_economist()+
  theme(legend.position = "right", legend.direction = "vertical")+
  #scale_x_continuous(breaks=...,labels = ...)+
  theme(axis.text.x = element_text(angle = 45))



