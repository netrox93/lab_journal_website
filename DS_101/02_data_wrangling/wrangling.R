library(vroom)
library(tidyverse)
library(lubridate)
library(data.table)

#import assignee Table
col_types_assignee <- list(
  id = col_character(),
  type = col_double(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_assignee,
  na         = c("", "NA", "NULL")
)
#import patent_assignee table
col_types_patent_assignee <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent_assignee,
  na         = c("", "NA", "NULL")
)

#import patent table
col_types_patents <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)

patent_tbl <- vroom(
  file       = "patent.tsv", 
  delim      = "\t", 
  col_types  = col_types_patents,
  na         = c("", "NA", "NULL")
)


# first attend with dplyr
#sort the patent table by the assignees with the most patents
patent_summary_tbl <- patent_assignee_tbl %>% select(patent_id,assignee_id)

most_patents <- patent_summary_tbl %>% 
  group_by(assignee_id) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  arrange(desc(count))

assignee_summary_tbl<-assignee_tbl %>% select(id,organization)

#save only the first 5000 assignees with the most patents ant join the Companies
#to the assignee IDs
most_patents <- most_patents %>%
  slice(1:(nrow(.)/100)) %>%
  left_join(y=assignee_summary_tbl,  by = c("assignee_id"="id"))
glimpse(most_patents)



###########################

patent_summary_2019 <- patent_tbl %>% 
  select(id,date) %>%
  mutate(year=year(date)) %>%
  mutate(amount=1)%>%
  left_join(y=patent_summary_tbl,  by = c("id"="patent_id")) %>%
  left_join(y=assignee_summary_tbl,  by = c("assignee_id"="id"))
glimpse(patent_summary_2019)

class(patent_summary_2019)
setDT(patent_summary_2019)
list_2019 <- patent_summary_2019[year == 2019,sum(amount),organization] 
glimpse(most_patents)

########### second attend with data.table ############

