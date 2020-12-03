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
library("data.table")


url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)

class(covid_data_dt)

'test_dt <- data.table(ID = c("b","b","b","a","a","c"),
                      a  = 1:6,
                      b  = 7:12,
                      c  = 13:18)'
#this gives an empty set
covid_data_dt[countriesAndTerritories == "Germany" & 
                lubridate::month(dateRep, label = T, abbr = F) == "June"]

#this does actually work like intended
covid_data_dt[order(year, month, day, -countriesAndTerritories)]

#try to select columns
select_cols = c("cases", "deaths")
covid_data_dt[, ..select_cols]

#how many days have more than 1000 deaths in a country?
covid_data_dt[,sum(deaths > 1000)]

# to list the observations put it in i
covid_data_dt[deaths > 1000]


#verstanden: neue Spalten einbringen
covid_data_dt[, `:=`(deaths_per_capita = deaths / popData2019,
                     cases_per_capita = cases / popData2019,
                     deaths_per_cases = deaths / cases)]
#Ersetzt NaN durch 0
covid_data_dt[, deaths_per_cases := ifelse(cases <1,0,deaths_per_cases)]


'copy'
'covid_data_dt[countriesAndTerritories == "Germany" & month == 4, 
              .(m_cases = mean(cases), 
                m_death = mean(deaths)
              )
]'


covid_data_dt[deaths_per_cases > 2, .N, by = countriesAndTerritories]


covid_data_dt[, .(
  m_cases  = round(mean(cases),  digits = 1), 
  m_deaths = round(mean(deaths), digits = 1)
), 
by = .(countriesAndTerritories)][order(-m_cases)]

# Create a new data.table
covid_data_EUR_dt <- covid_data_dt[ continentExp == "Europe", 
                                    lapply(.SD, function(x) {
                                      x %>% 
                                        mean() %>% 
                                        round(1)
                                    }
                                    ), 
                                    by = .(countriesAndTerritories), 
                                    .SDcols = c("cases", "deaths")
]

# Set key
setkey(covid_data_EUR_dt, countriesAndTerritories)
key(covid_data_EUR_dt)

# Create two data.tables from that
cd_dt1 <- covid_data_EUR_dt[, .(countriesAndTerritories, cases)]
cd_dt2 <- covid_data_EUR_dt[1:20, .(countriesAndTerritories, deaths)]

# Join them
cd_dt1[cd_dt2]
