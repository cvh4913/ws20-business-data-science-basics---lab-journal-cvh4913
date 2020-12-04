#librarys


library(vroom)
library(tidyverse)
library(data.table)


col_types <- list(
  id = col_integer(),
  type = col_skip(),
  number = col_integer(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_skip(),
  title = col_skip(),
  kind = col_skip(),
  num_claims = col_double(),
  filename = col_skip(),
  withdrawn = col_character()
)

patent_tbl <- vroom(
  file       = "00_Data/03_patents/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

#glimpse(patent_tbl)'

col_types <- list(
  patent_id = col_integer(),
  assignee_id = col_character(),
  location_id = col_character()
  )

patent_assignee_tbl <- vroom(
  file       = "00_Data/03_patents/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

col_types <- list(
  id = col_character(),
  type = col_integer(),
  name_first = col_skip(),
  name_last = col_skip(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "00_Data/03_patents/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

'col_types <- list(
  uuid = col_character(),
  patent_id = integer(),
  mainclass_id = col_integer(),
  subclass_id = col_character(),
  sequence = col_integer()
)

uspc_tbl <- vroom(
  file       = "00_Data/03_patents/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)'
glimpse(patent_assignee_tbl)
glimpse(assignee_tbl)
#1. Which US Company has the most Patents
#aufbereitung der Daten 

patent_tbl<-patent_tbl%>%
  filter(country == "US")%>%
  filter(!is.na(id))%>%
  mutate(year=year(date))%>%
  filter(year==2019)
  
assignee_tbl<-assignee_tbl%>%
  filter(type==2)


data<-left_join(patent_assignee_tbl,assignee_tbl,by=c("assignee_id"="id"))
data<-left_join(data,patent_tbl,by=c("patent_id"="id"))
data<-data%>%
  filter((!is.na(year))&!is.na(organization))

res<-data%>%
  count(organization)%>%
  arrange(desc(n))%>%
  slice(1:10)

## Visualize

res %>%
  
  # Setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x = reorder(organization,-n),n, y = n)) +
  theme(axis.text.x = element_text(angle = 90))+
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  
  
  
  # Formatting
  # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
  # Again, we have to adjust it for euro values
  labs(
    title    = "Number of Patents for different US Companies",
    
    x = "", # Override defaults for x and y
    y = "Number of Assigned Patents"
  )


res %>% 
  write_rds("00_Data/03_wrangle_challenge/3_2_corporations.rds")







