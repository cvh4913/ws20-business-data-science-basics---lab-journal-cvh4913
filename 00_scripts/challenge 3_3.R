#librarys
get_patent_classes <- function(tbl,companie){
  
  tbl%>%
    filter(organization %in% companie)%>%
    count(mainclass_id)%>%
    arrange(desc(n))%>%
    slice(1:5)  
}

build_figure <- function(list,i,j){
  
  as_tibble(list[c(i,i+1)]) %>%
    
    # Set up x, y, fill
    ggplot(aes(x = reorder(mainclass_id,-n),n, y = n, fill = mainclass_id)) +
    theme(axis.text.x = element_text(angle = 90))+
    
    # Geometries
    geom_col() + # Run up to here to get a stacked bar plot
    geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
    
    
    
    # Formatting
    
    
    labs(
      title = relevant_companies[j],
      
      fill = "Main category" # Changes the legend name
      
    )
  
  
  
}
  
  



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



#cleaning the data befor joining
#only companies are important, therefore everything except companies is cleaned from the assignee_tbl which would mean only type 2 and 3 are considered

#derive best 2 companies world wide

assignee_tbl<-assignee_tbl%>%
  filter((type==2)|(type==3))

relevant_patents_tbl<-left_join(assignee_tbl,patent_assignee_tbl,by=c("id"="assignee_id"))

relevant_patents_tbl<-relevant_patents_tbl%>%
  filter(!is.na(patent_id))

#get all patent ids which are relevant for this process because they are left in the relevant dataset
patent_vec<-pull(relevant_patents_tbl,patent_id)


#filter the uspc tibble to reduce its size
uspc_tbl<-uspc_tbl%>%
  filter(patent_id %in% (patent_vec))%>%
  distinct() # the patents are still saved multiple times because of the sub classes which are not nessecary here. Distinct to get rid of thart problem


relevant_patents_tbl<-left_join(relevant_patents_tbl,uspc_tbl,by=c("patent_id"="patent_id"))
#filter to get rid of each patent which does not have a main class ID
relevant_patents_tbl<-relevant_patents_tbl%>%
  filter((!is.na(mainclass_id))&(!is.na(organization)))

#find the most innovative tech sector
tech<-relevant_patents_tbl%>%
  count(mainclass_id)%>%
  arrange(desc(n))%>%
  slice(1:10)
#found most innovative tech sectors

#find the top ten most innovative countries worldwide
number_of_patents<-  relevant_patents_tbl%>%
  group_by(organization)%>%
  summarize(
    count=n()
  )%>%
  ungroup()%>%
  arrange(desc(count))%>%
  slice(1:10)

relevant_companies<-pull(number_of_patents,organization) 

c1<-get_patent_classes(relevant_patents_tbl,relevant_companies[1])

c2<-get_patent_classes(relevant_patents_tbl,relevant_companies[2])

c3<-get_patent_classes(relevant_patents_tbl,relevant_companies[3])

c4<-get_patent_classes(relevant_patents_tbl,relevant_companies[4])

c5<-get_patent_classes(relevant_patents_tbl,relevant_companies[5])

c6<-get_patent_classes(relevant_patents_tbl,relevant_companies[6])

c7<-get_patent_classes(relevant_patents_tbl,relevant_companies[7])

c8<-get_patent_classes(relevant_patents_tbl,relevant_companies[8])

c9<-get_patent_classes(relevant_patents_tbl,relevant_companies[9])

c10<-get_patent_classes(relevant_patents_tbl,relevant_companies[10])

c_vec<-c(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10)


build_figure(c_vec,1,1)
build_figure(c_vec,3,2)
build_figure(c_vec,5,3)
build_figure(c_vec,7,4)  
build_figure(c_vec,9,5)
build_figure(c_vec,11,6)
build_figure(c_vec,13,7)
build_figure(c_vec,15,8)  
build_figure(c_vec,17,9)
build_figure(c_vec,19,10)

tech %>%
  
  # Set up x, y, fill
  ggplot(aes(x = reorder(mainclass_id,-n),n, y = n, fill = mainclass_id)) +
  theme(axis.text.x = element_text(angle = 90))+
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  
  
  # Formatting
  
  
  labs(
    title = "most innovative Tech Sectors for the top 10 Corporations worldwide",
    
    fill = "Main category" # Changes the legend name
    
  )


