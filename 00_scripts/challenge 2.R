# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

bikeclasses_tbl<-function(url,integer,family,family_id_url){
  
  bike_name_tbl <- url %>%
    read_html()%>%
    html_nodes(css = ".catalog-category-bikes__title-text") %>% 
    
    html_text() %>%
    str_remove(pattern = "\n") %>%
    str_remove(pattern = "\n") %>%
    # Convert vector to tibble
    enframe(name = "position", value = "subdirectory") %>%
    mutate(category = family[integer])%>%
    mutate(class_url = family_id_url[integer])
    
  
}


# 1.0 Load libraries ----
library("tidyverse")
library("readxl")
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




#gotten canyon url stuff and ready for analysis

# 1.1 COLLECT PRODUCT FAMILIES ----

url_home          <- "https://www.rosebikes.de/fahrräder"
#xopen(url_home) # Open links directly from RStudio to inspect them

# Read in the HTML for the entire webpage
html_home         <- read_html(url_home)

# Web scrape the ids for the families
bike_family_tbl <- html_home %>%
  
  # Get the nodes for the families ...
  html_nodes(css = ".catalog-navigation__link") %>%
  # ...and extract the information of the id attribute
  html_attr('href') %>%
  
  discard(.p = ~stringr::str_detect(.x,"Sale|Finder|Kinder")) %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "family_class")%>%
  
  # build individual urls for each category page
  mutate(
    family_url = str_glue("https://www.rosebikes.de{family_class}")
  )

bike_family_tbl

# 1.2 COLLECT PRODUCT CATEGORIES ----

# Combine all Ids to one string so that we will get all nodes at once
# (seperated by the OR operator ",")
family_id_url <- bike_family_tbl %>%
  pull(family_url)

bike_family<-bike_family_tbl%>%
  pull(family_url)%>%
  str_remove(pattern = "https://www.rosebikes.de/fahrräder/")
  
family_id_url
url<-family_id_url[1]

bikes_tbl<-bikeclasses_tbl(family_id_url[1],1,bike_family,family_id_url)
for(i in 2:12){
  
  add<-bikeclasses_tbl(family_id_url[i],i,bike_family,family_id_url)
  bikes_tbl<-add_row(bikes_tbl,add)
  
}

#===================================================================
#versuch mit mutate eine weitere Spalte zu bikes_tbl hinzuzufügen
for(i in 1:nrow(bike_family_tbl)){
  add<-family_id_url[i]%>%
    read_html()%>%
    html_nodes(css = ".catalog-category-bikes__content > a") %>%
    html_attr("href") %>%
    as_tibble%>%
    mutate(Name= family_id_url[i]%>%
             read_html()%>%
             html_nodes(css = ".catalog-category-bikes__title-text") %>%
             html_text() %>%
             str_remove(pattern = "\n") %>%
             str_remove(pattern = "\n") 
    
  )%>%
  mutate(price=family_id_url[i]%>%
           read_html()%>%
           html_nodes(css = ".catalog-category-bikes__price-title") %>%
           html_text()%>%
           str_remove(pattern = "\nab ")%>%
           str_remove(pattern = "\n ")
           
           )
    
    if (i==1) {
      misc_bikedata<-add
    } else {
      misc_bikedata<-add_row(misc_bikedata,add)
    }
  
}

bikes_tbl<-left_join(bikes_tbl,misc_bikedata,by=c("subdirectory"="Name"))

bikes_tbl %>% 
  write_rds("00_Data/01_bike_challenge/bikes_tbl.rds")

test<-readRDS("00_Data/01_bike_challenge/bikes_tbl.rds")%>%head(10)
test
