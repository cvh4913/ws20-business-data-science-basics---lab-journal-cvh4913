library(vroom)
library(tidyverse)
library(data.table)


'col_types <- list(
  id = col_integer(),
  type = col_character(),
  number = col_integer(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
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
  name_first = col_character(),
  name_last = col_character(),
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
#join data 
wrangeled<-left_join(patent_assignee_tbl,assignee_tbl,by = c("assignee_id"="id"))
#only patents od US Companies shall be counted e.g. the type has to be exactly 2


wrangeled<-wrangeled%>%
  filter(type == 2)%>%
  select(organization, patent_id, everything())%>%
  select(-name_first, -name_last, -location_id, -assignee_id)%>%
  arrange(desc(organization))


Patentnumber<-wrangeled%>%
  filter(!is.na(patent_id))%>%
  count(organization)%>%
  arrange(desc(n))

head(Patentnumber,10)

Picture_table<-Patentnumber%>%
  slice(1:10)
## Visualize

Picture_table %>%
  
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



