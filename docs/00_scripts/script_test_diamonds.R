#script to analyse test data diamonds.rds


library (tidyverse)

diamonds<- readRDS("00_Data/diamonds5.rds")

diamonds %>% head(15)

diamonds %>% 
  unite(clarity, clarity_prefix, clarity_suffix, sep = '')

diamonds %>% filter(cut =="Ideal" | cut =="Premium", x >= 4)

diamonds %>% head(15)