library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# -------------------------------------------------------------------------

dat = tibble(path = list.files(here("workshop_dataviz","data","raw"))) %>% 
  separate(path, c("code","country"), "__", remove = F) %>% 
  mutate(country = str_remove(country, ".csv")) %>% 
  rowwise() %>% 
  mutate(data = read.csv(here("workshop_dataviz","data","raw",path)) %>% 
           tibble() %>% 
           list()) %>% 
  ungroup() %>% 
  select(-path, -code) %>% 
  unnest(data) %>% 
  relocate(-country)

datAverages = dat %>% 
  mutate(theYear = ifelse(year(ymd(date)) %in% 2015:2019, "2015 - 2019", "2020")) %>% 
  select(-date) %>% 
  group_by(theYear, country, code, name) %>% 
  summarise(mean = mean(mean, na.rm = T),
            median = median(median, na.rm = T),
            .groups = "drop")

write.csv(dat, here("workshop_dataviz","data", "timeSeriesData.csv"), row.names = F)

write.csv(datAverages, here("workshop_dataviz","data", "historicalAverageData.csv"), row.names = F)