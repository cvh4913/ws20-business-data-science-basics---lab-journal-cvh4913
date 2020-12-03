#librarys


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
  location_id = col_skip()
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

col_types <- list(
  uuid = col_skip(),
  patent_id = col_integer(),
  mainclass_id = col_integer(),
  subclass_id = col_skip(),
  sequence = col_skip()
)

uspc_tbl <- vroom(
  file       = "00_Data/03_patents/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

uspc_tbl_res <- vroom(
  file       = "00_Data/03_patents/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

#cleaning the data befor joining
#only companies are important, therefore everything except companies is cleaned from the assignee_tbl which would mean only type 2 and 3 are considered

assignee_tbl<-assignee_tbl%>%
  filter((type==2)|(type==3))

uspc_tbl<-uspc_tbl%>%
  filter(!is.na(mainclass_id))%>%
  mutate(number=1)%>%
  group_by(mainclass_id)%>%
  summarize(number_patents=sum(number))%>%
  arrange(desc(number_patents))



uspc_bit<-uspc_tbl%>%
  slice(1:100)
#there are 3 things considered here. companies, number of patents per tech class
#uspc and patent assignee can be joined via patent id
#the assignee is considered with the assignee ID

data<-left_join(uspc_tbl,patent_assignee_tbl,by=c("patent_id"="patent_id"))
#data<-left_join(data,assignee_tbl,by_c("assignee_id"="id"))

  




