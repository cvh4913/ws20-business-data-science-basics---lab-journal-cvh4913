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
library("stringr")
library("data.table")


world <- map_data("world")
url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_tbl <- fread(url)%>%
  as_tibble

date<-covid_data_tbl%>%
  select(day,month,year)%>%
  mutate(Date=toDate(year,month,day))%>%
  arrange(desc(Date))%>%
  slice(1:1)




covid_data_tbl<-covid_data_tbl%>%
  
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
    
  ))%>%
  rename(location=countriesAndTerritories)%>%
  mutate(Report_date = toDate(year,month,day)) %>%
  select(Report_date,deaths,location,month,popData2019,cases)%>%
  group_by(location)%>%
  arrange(Report_date)%>%
  mutate(total_deaths=sum(deaths))%>%
  mutate(total_cases=sum(cases))%>%
  mutate(deaths_per_capita=total_deaths/popData2019)%>%
  mutate(mortality_rate=total_deaths/total_cases)%>%
  filter(Report_date==date$Date)%>%
  select(-deaths,-month,-popData2019)%>%
  arrange(desc(deaths_per_capita))
  
  
  
 
world<- left_join(world,covid_data_tbl,by=c("region"="location"))









world %>% ggplot() +
  geom_map(aes(long,lat,fill=deaths_per_capita, map_id = region ), map = world,
           color="#2b2b2b", size=0.15)+
  
  scale_fill_viridis_c(option = "C")+
  labs(
    title = "Map of Deaths per capita",
    subtitle = "By December 12th of 2020",
    x = "Longitude/°",
    y = "Latitude/°",
    caption = "Date: 12/05/2020"
    
    
  )+
theme(
  panel.background = element_rect(fill = "transparent",
                                  colour = "transparent",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "gray"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
  
)

world %>% ggplot() +
  geom_map(aes(long,lat,fill=mortality_rate, map_id = region ), map = world,
           color="#2b2b2b", size=0.15)+
  
  scale_fill_viridis_c(option = "C")+
  labs(
    title = "Map of Mortality Rates",
    subtitle = "By December 12th of 2020",
    x = "Longitude/°",
    y = "Latitude/°",
    caption = "Date: 12/05/2020"
    
    
  )+
  theme(
    panel.background = element_rect(fill = "transparent",
                                    colour = "transparent",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
    
  )
