library(tidyverse)
library(dplyr)
library(readxl)
library(plotly)

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

# Here is the ggplot

x %>% 
  filter(firm %in% good_firms) %>% 
  filter(date > Sys.Date() - 365) %>%
  select(-c(source, sd)) %>%
  gather(key = party, value = value, PP, PSOE, Cs, VOX, Podemos) %>%
  ggplot(aes(x = date, y = value, color = party)) +
  geom_point() + 
  geom_smooth(se = FALSE) +
  labs(x = NULL, y = "Percentage Support", title = "Polling in Advance of Spanish Election on April 28",
       subtitle = "Popular support for VOX surges over last year")
  
#Here is the interactive plot


# data <-
#   x %>% 
#   filter(firm %in% good_firms) %>% 
#   filter(date > Sys.Date() - 365) %>%
#   select(-c(source, sd)) %>%
#   gather(key = party, value = value, PP, PSOE, Cs, VOX, Podemos) %>%
# group_by(date, party) %>% 
  # summarize(avg_poll = mean(value)) %>%
  # spread(key = party, value = avg_poll) %>% 
  # ungroup()
 
# plot <- plot_ly(data, x = ~date, y = ~PP, name = 'PP', type = 'scatter', mode = 'lines+markers') %>%
#   add_trace(y = ~PSOE, name = 'PSOE', mode = 'lines+markers') %>%
#   add_trace(y = ~BNG, name = 'BNG', mode = 'lines+markers') %>%
#   add_trace(y = ~CC, name = 'CC', mode = 'lines+markers') %>%
#   add_trace(y = ~ERC, name = 'ERC', mode = 'lines+markers') %>% 
#   add_trace(y = ~Cs, name = 'CS', mode = 'lines+markers') %>%
#   add_trace(y = ~VOX, name = 'VOX', mode = 'lines+markers') %>%
#   add_trace(y = ~Podemos, name = 'PODEMOS', mode = 'lines+markers') %>%
#   add_trace(y = ~EHBildu, name = 'EH Bildu', mode = 'lines+markers') %>%
#   add_trace(y = ~PDeCAT, name = 'PDeCat', mode = 'lines+markers') %>% 
#   
#   layout(title = "1 Year Election Polls, Spain", 
#          yaxis = list(title = "Seats"), 
#          xaxis = list(title = "Date"))
# 
# 
# plot



  