library(tidyverse)
diamonds2 <- readRDS("diamonds2.rds")
diamonds2
diamonds2 %>% 
  pivot_longer(cols      = c("2008", "2009"), 
               names_to  = 'year', 
               values_to = 'price') %>% 
  head(n = 5)
diamonds3 <- readRDS("diamonds3.rds")
diamonds3 %>%
pivot_wider(names_from="dimension",
            values_from="measurement") %>%
  head(n=5)
diamonds4 <- readRDS("diamonds4.rds")

diamonds4 %>% 
  separate(col = dim,
           into = c("x", "y", "z"),
           sep = "/",
           convert = T)

diamonds5 <- readRDS("diamonds5.rds")

diamonds5 %>% 
  unite(clarity, clarity_prefix, clarity_suffix, sep = '')

library(ggplot2) # To load the diamonds dataset
library(dplyr)
diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  head(5)

diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  slice(3:4)
diamonds %>% 
  arrange(cut, carat, desc(price))