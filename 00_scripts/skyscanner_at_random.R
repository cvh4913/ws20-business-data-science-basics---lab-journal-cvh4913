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
library("furrr")
library("xopen")
library("stringi")

get_destination_data<-function(url){
  
  html_destinations<-read_html(url)
  destinations_tbl<-html_destinations%>%
    html_nodes(css = "result-list nav-content")%>% 
    html_attr('destinations')%>%
    as_tibble()
  
  
}
  
  


#URL führt zu den niedrigstpreisigen flügen ab Hamburg in x Länder

url<-"https://www.skyscanner.de/transport/fluge-von/ham/201207/201214/?adults=1&adultsv2=1&cabinclass=economy&children=0&childrenv2=&inboundaltsenabled=true&infants=0&originentityid=27536295&outboundaltsenabled=false&preferdirects=false&preferflexible=false&ref=home&rtn=1"

#xopen(url)

destinations<-get_destination_data(url)



