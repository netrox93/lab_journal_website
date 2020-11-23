library(httr)
library(jsonlite)
library(tidyverse)

resp <- GET("https://www.quandl.com/api/v3/datasets/BP/ELEC_GEN_DEU.json?api_key=3fzuzvof5E_4V2rs-WWm")

resp %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()