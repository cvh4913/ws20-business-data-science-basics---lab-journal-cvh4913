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
library("magrittr")


# List all internal data sets
data()

# Load specified data sets
data("airquality")
aq_dt <- data.table(airquality)
aq_dt[!is.na(Ozone), .(Solar.R, Wind, Temp)]


data("mtcars") # step not absolutely necessary
mtcars$carname <- rownames(mtcars)
mtcars_dt <- as.data.table(mtcars)
mtcars_dt[, mileage_type := ifelse(mpg > 20, 'high', 'low')]

mtcars_dt[, .(.N, mileage = mean(mpg) %>% round(2)), by=gear]
