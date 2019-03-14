library(tidyverse)
library(dplyr)

x <- read_csv("https://pollofpolls.eu/get/polls/ES-parliament/format/csv")
# Make a list of firms with at least 40 polls

good_firms <- x %>% 
  group_by(firm) %>% 
  summarize(number = n()) %>% 
  ungroup() %>% 
  filter(number >= 40) %>% 
  pull(firm)

# Figure out date a year ago

# Filter out data that we want to use and send it to the plot.


data <-  
  x %>% 
  filter(firm %in% good_firms) %>% 
  filter(date > Sys.Date() - 365) %>%
  select(-c(source, sd)) %>%
  gather(key = party, value = value, PP, PSOE, BNG, CC, ERC, PNVEAJ, Cs, VOX, Podemos,
         EHBildu, PACMA, PDeCAT) 
  