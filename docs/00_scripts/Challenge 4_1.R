# load Librarys
toDate <- function(year, month, day) {
  ISOdate(year, month, day)
}

toNumerics <- function(Date) {
  stopifnot(inherits(Date, c("Date", "POSIXt")))
  day <- as.numeric(strftime(Date, format = "%d"))
  month <- as.numeric(strftime(Date, format = "%m"))
  year <- as.numeric(strftime(Date, format = "%Y"))
  list(year = year, month = month, day = day)
}


library("tidyverse");
library("readxl");
library("lubridate")
library("writexl")
library("ggthemes")

#Get covid data

url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)%>%
  as_tibble

relevant_locations<-c("Germany","United_Kingdom","France","Spain","United_States_of_America")

covid_data_dt<-covid_data_dt%>%
  rename(location=countriesAndTerritories)%>%
  filter((location %in% relevant_locations)&(year==2020))%>%
  mutate(Report_date = toDate(year,month,day)) %>%
  select(Report_date,cases,location,month)%>%
  group_by(location)%>%
  arrange(Report_date)%>%
  mutate(cumulative_cases=cumsum(cases))%>%
  ungroup()


#Objective: Plot covid cummulative cases over time 
#Objective: customize plot 

dates<-covid_data_dt%>%
  pull(Report_date)%>%
  as_tibble%>%
  unique()

months<-covid_data_dt%>%
  pull(month)%>%
  as_tibble%>%
  unique()#%>%
  #mutate(Date=toDate(2020,month,1))

xmin <- first(dates)
xmax <- last(dates)

#Testplot

covid_data_dt %>%
  
  # Put the aes color mapping here, to apply it to geom_line and geom_point
  ggplot(aes(Report_date, cumulative_cases, color = location)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Confirmed COVID-19 Cases Worlwide",
    subtitle = "Europe was optional",
    x = "Year 2020",
    y = "Cumulative Cases"
    
    
  )+
  theme(
    panel.background = element_rect(fill = "transparent",
                                    colour = "transparent",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "black"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "grey")
      
  )+
  scale_x_continuous(breaks = toDate(2020,unique(covid_data_dt$month),1),labels= c("January","Februrary","March","April","May","June","July","August","September","October","November","December")
                     
                                        ) +
  geom_label(label =  covid_data_dt%>%
               filter((location=="United_States_of_America")&(Report_date==xmax))%>%
               select(cumulative_cases)%>%
               as.numeric(),
             vjust = "inward",
             hjust = "inward",
             size  = 3,
             fill  = 'green',
             color = "black",
             fontface = "italic",
             data = covid_data_dt %>%
               filter((location %in% c("United_States_of_America"))&Report_date==last(dates))) +
  
  # Or you could do it locally in each geom 
  # (aes mapping only necessary if you map it to a column)
  geom_line(size = 1)+scale_colour_manual(values=c("blue","black","yellow","red","green")) +
  scale_y_continuous(breaks= seq(0, 15e6, by = 25e5),labels = scales::dollar_format(scale  = 1/1e6, 
                                                    prefix = "", 
                                                    suffix = "M")) 
  
coord_cartesian(ylim = c(0, 1.5e7), xlim = c(xmin, xmax)) 


  
  
