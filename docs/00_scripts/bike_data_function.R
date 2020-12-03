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

#List of needed functions


get_bike_data <- function(url) {
  
  html_bike_category <- read_html(url)
  
  # Get the URLs
  bike_url_tbl  <- html_bike_category %>%
    html_nodes(css = ".productTile__contentWrapper > a") %>%
    html_attr("href") %>%
    str_remove(pattern = "\\?.*") %>%
    enframe(name = "position", value = "url")
  
  # Get the descriptions
  bike_desc_tbl <- html_bike_category %>%
    html_nodes(css = '.productTile__productSummaryLeft > 
                      meta[itemprop="description"]') %>%
    html_attr("content") %>%
    enframe(name = "position", value = "description")
  
  # Get JSON data
  bike_json_tbl <- html_bike_category %>%
    html_nodes(css = '.productGrid__listItem.xlt-producttile > div') %>%
    html_attr("data-gtm-impression") %>%
    map(fromJSON) %>% # need JSON ### need lists
    map(purrr::pluck, 2, "impressions") %>% 
    map(na_if, "not defined") %>%
    map(na_if, "") %>%
    map(~mutate(., across(c("dimension56","price"), as.numeric))) %>%
    bind_rows() %>%
    as_tibble() %>%
    rowid_to_column(var='position') %>%
    left_join(bike_desc_tbl) %>%
    left_join(bike_url_tbl)
}

get_colors <- function(url) {
  
  url %>%
    
    read_html() %>%
    
    # Get all 'script nodes' and convert to char
    html_nodes(css = "script") %>%
    as.character() %>%
    
    # Select the node, that contains 'window.deptsfra'
    str_subset(pattern = "window.deptsfra") %>%
    
    # remove the chars that do not belong to the json
    # 1. replace at the beginning everything until the first "{" with ""
    str_replace("^[^\\{]+", "") %>%
    # 2. replace at the end everything after the last "}" with ""
    str_replace("[^\\}]+$", "") %>%
    
    # Convert from json to an r object and pick the relevant values
    fromJSON() %>%
    purrr::pluck("productDetail", "variationAttributes", "values", 1, "value")
}

get_sizes <- function(url) {
  
  json <- url %>%
    
    read_html() %>%
    
    # Get all 'script nodes' and convert to char
    html_nodes(css = "script") %>%
    as.character() %>%
    
    # Select the node, that contains 'window.deptsfra'
    str_subset(pattern = "window.deptsfra") %>%
    
    # remove the chars that do not belong to the json
    # 1. replace at the beginning everything until the first "{" with ""
    str_replace("^[^\\{]+", "") %>%
    # 2. replace at the end everything after the last "}" with ""
    str_replace("[^\\}]+$", "") %>%
    
    # Convert from json to an r object and pick the relevant values
    fromJSON(flatten = T) %>%
    purrr::pluck("productDetail", "variationAttributes", "values", 2) %>%
    
    # select(id, value, available, availability)# %>%
    select(id, value, availability.onlyXLeftNumber) %>%
    
    # Rename
    rename(id_size = id) %>%
    rename(size = value) %>%
    rename(stock_availability = availability.onlyXLeftNumber) %>%
    
    # Conver to tibble
    as_tibble()
  
}


##Build Bike Data Tibble with URLs 

bike_category_tbl <- html_home %>%
  
  # Select nodes by the ids
  html_nodes(css = family_id_css) %>% # comma separated different ids
  
  # Going further down the tree and select nodes by class
  # Selecting two classes makes it specific enough
  html_nodes(css = ".navigationListSecondary__listItem .js-ridestyles") %>%
  html_attr('href') %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "subdirectory") %>%
  
  # Add the domain, because we will get only the subdirectories
  mutate(
    url = glue("https://www.canyon.com{subdirectory}")
  ) %>%
  
  # Some categories are listed multiple times.
  # We only need unique values
  distinct(url)

#create a URL vector which can be handeled by the function step by step

bike_category_url_vec <- bike_category_tbl %>% 
  pull(url)

bike_data_lst <- map(bike_category_url_vec, get_bike_data)

# Merge the list into a tibble
bike_data_tbl <- bind_rows(bike_data_lst)
saveRDS(bike_data_tbl, "bike_data_tbl.rds")

bike_data_tbl %>%
  group_by(id) %>%
  filter(n()>1) %>%
  arrange(id) 
  #View()

bike_data_cleaned_tbl <- bike_data_tbl %>%
  
  # Filter for bikes. Only unique ones
  filter(nchar(.$id) == 4) %>%
  filter(!(name %>% str_detect("Frameset"))) %>%
  distinct(id, .keep_all = T) %>%
  
  # Split categories (Speedmax had to be treated individually)
  mutate(category = replace(category, 
                            name == "Speedmax CF SLX 8.0 SL", "Road/Triathlon Bike/Speedmax")) %>%
  separate(col = category, into = c("category_1",
                                    "category_2",
                                    "category_3"),
           sep = "(?<!\\s)/(?!\\s)") %>%
  
  # Renaming
  rename("year"       = "dimension50") %>%
  rename("model"      = "name") %>%
  rename("gender"     = "dimension63") %>%
  rename("price_euro" = "metric4") %>%
  
  # Fix years manually (have checked the website)
  mutate(year = replace_na(year, 2021)) %>%
  
  # Add frame material
  mutate(frame_material = case_when(
    model %>% str_detect(" CF ") ~ "carbon",
    model %>% str_detect(" CFR ") ~ "carbon",
    TRUE ~ "aluminium"
  )
  ) %>%
  
  # Select and order columns
  select(-c(position, brand, variant, starts_with("dim"), 
            quantity, feedProductId, price, metric5)) %>%
  select(id, model, year, frame_material, price_euro, everything())

saveRDS(bike_data_cleaned_tbl, "bike_data_cleaned_tbl.rds")

bike_url_vec<-bike_data_cleaned_tbl %>% 
  pull(url)

plan("multiprocess")
bike_data_colors_tbl <- bike_data_cleaned_tbl %>% 
  mutate(colors = future_map(bike_url_vec, get_colors))

saveRDS(bike_data_colors_tbl,"bike_data_colors_tbl.rds")

# 3.2 Create the urls for each variation

bike_data_colors_tbl <- bike_data_colors_tbl %>%
  
  # Create entry for each color variation
  unnest(colors) %>%
  
  # Merge url and query parameters for the colors
  mutate(url_color = glue("{url}?dwvar_{id}_pv_rahmenfarbe={colors}")) %>%
  select(-url) %>%
  
  # Use stringi to replace the last dash with the HTLM format of a dash (%2F)
  # Only if there is a dash in the color column
  mutate(url_color = ifelse(str_detect(colors, pattern = "/"),
                            
                            # if TRUE --> replace      
                            stringi::stri_replace_last_fixed(url_color, "/", "%2F"),
                            
                            # ELSE --> take the original url
                            url_color))

bike_data_colors_tbl %>% glimpse()

bike_url_color_vec <- bike_data_colors_tbl %>% 
  pull(url_color)

# Map
bike_data_sizes_tbl <- bike_data_colors_tbl %>% 
  mutate(size = future_map(bike_url_color_vec, get_sizes))

# Unnest
bike_data_sizes_tbl <- bike_data_sizes_tbl %>% 
  unnest(size)

saveRDS(bike_data_sizes_tbl, "bike_data_sizes_tbl.rds")
