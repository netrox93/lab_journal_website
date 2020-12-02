library(tidyverse)
library(lubridate)
library(data.table)
library(scales)
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
  

data_tbl <- covid_data_tbl %>% 
  distinct(cases, dateRep, countriesAndTerritories) %>% 
  filter(countriesAndTerritories == 'Germany' | 
           countriesAndTerritories == 'United_Kingdom' | 
           countriesAndTerritories == 'Spain' | 
           countriesAndTerritories == 'France') %>%
  mutate(date       = lubridate::dmy(dateRep)) %>% 
  arrange(date) %>%
  group_by(countriesAndTerritories) %>%
  mutate(sum_cases = cumsum(cases)) %>%
  ungroup


p <- ggplot(data=data_tbl,aes(x=date,y=sum_cases,color=countriesAndTerritories))+
  geom_line(size=2) +
  scale_color_manual(values=c('#0072BD','#D95319','#EDB120','#7E2F8E'))+
  scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks("1 months"))

p + labs(title = "Cummulative Covid cases in european countries",
         caption = "Number of Covid Cases is still growing fast",
         x = "Date",
         y = "Cummulative Covid Cases per country",
         color = "Country" )+
  annotate("text",
           x=date(c('2020-11-20','2020-12-01','2020-12-01','2020-11-01')),
           y=(c(400000,1300000,1700000,2200000)),
           label=(c("Germany","Spain","UK","France")),
           color=c('#D95319','#7E2F8E','#EDB120','#0072BD'))+

theme(
     axis.text.x = element_text(
       angle = 45,
       hjust = 1
     ),
     strip.text = element_text(
       face  = "bold",
       color = '#0072BD'
     )
   )


world_map <- map_data("world")

covid_worldwide_tbl <- covid_data_tbl %>%
  select(countriesAndTerritories,deaths,popData2019,dateRep)%>%
  mutate(across(countriesAndTerritories, str_replace_all, "_", " "))%>%
  mutate(countriesAndTerritories = case_when(
  countriesAndTerritories == "United Kingdom" ~ "UK",
  countriesAndTerritories == "United States of America" ~ "USA",
  countriesAndTerritories == "Czechia" ~ "Czech Republic",
  TRUE ~ countriesAndTerritories
)) %>%
  mutate(date = lubridate::dmy(dateRep)) %>% 
  arrange(date) %>%
  group_by(countriesAndTerritories) %>%
  mutate(toal_deaths = cumsum(deaths)) %>%
  filter(date=="2020-12-01") %>%
  mutate(mortality=toal_deaths/popData2019) %>%
  left_join(y=world_map, by =c("countriesAndTerritories"="region"))
 
theme_set(
  theme_void()
)

ggplot(covid_worldwide_tbl, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=mortality), colour = "grey")
  #scale_fill_viridis_c(option = "C")
#scale_fill_brewer(type = "seq",palette = 1,direction = 1,aesthetics = "colour")
