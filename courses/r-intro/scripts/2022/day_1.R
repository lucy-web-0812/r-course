library(dplyr)
library(lubridate)

no = read.csv("data/data_day_1/MY1_no_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

no2 = read.csv("data/data_day_1/MY1_no2_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

o3 = read.csv("data/data_day_1/MY1_o3_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

met = read.csv("data/data_day_1/MY1_met_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(date, wind_direction, wind_speed)

mydata = no %>% 
  left_join(no2,by = "date") %>% 
  left_join(o3, by = "date") %>% 
  left_join(met, by = "date")

