# day_2.R
# ~~~~~~~

# Enter your code in this script as you follow along with the second day's session.

library(dplyr)
library(lubridate)
library(stringr)
library(tidyverse)

files = list.files("courses/r-intro/data/taught/part_2/", full.names = TRUE)

site_names <- basename(files) |> # Useful functions just to give the end of the files 
  word(1,1, sep = "_") |> 
  tolower()

# Loops -------------------------------------------------------------------


siteList <- list()

for(i in 1:length(files)){
  siteList[[i]] <- read.csv(files[i]) |> 
    tibble() |> 
    select(-X) |> 
    mutate(site = site_names[i]) 
  
  
  if(site_names[i] == "lon6"){
    siteList[[i]] <- siteList[[i]] |> 
    mutate(date = as_datetime(date)) 
  }else{
    siteList[[i]] <- siteList[[i]] |> 
    mutate(date = ymd_hms(date)) 
  }
  
  if(site_names[i] == "kc1"){
    siteList[[i]] <- siteList[[i]] |> 
      mutate(no = ifelse(no == "missing", NA, no), no = as.numeric(no)) 
  }
}

sites_middle <- bind_rows(siteList)
sites_middle |> count(site)


sites_middle |> 
  group_by(site) |> 
  summarise(mean_no2 = mean(no2, na.rm = T), sd = sd(no2, na.rm = T)) |> 
  arrange(mean_no2)

# select, rename, group_by, 

library(tidyr)
# pivot_wider and pivot_longer

sites_wide <- sites_middle |> 
  pivot_wider(names_from = site, values_from = c(no2,no,o3))

sites_long <- sites_middle |> 
  pivot_longer(c(no,no2,o3), names_to = "species", values_to = "conc")

sites_long |> 
  group_by(site, species) |> 
  summarise(mean_conc = mean(conc, na.rm = T), sd = sd(conc, na.rm = T)) |> 
  arrange(mean_conc) |> 
  ungroup()


# Pipeline to flag values greater than 95% 

sites_long |> 
  group_by(site,species) |> 
  mutate(p_95 = quantile(conc, 0.95, na.rm = T),
         flag = conc > p_95) |> 
  ungroup()

# Filter by any of the row values, e.g just getting certain dates,certain pollutants, certain sites
# Note that can use the between function but this is inclusion boundaries 

sites_long |> 
  filter(site %in% c('cll2', 'kc1'), species == "no2", conc > 100) # Can also use the != function

sites_long |> 
  group_by(site,species) |> 
  filter(conc >= quantile(conc, 0.95, na.rm = T)) |> 
  ungroup()


# More time data funcitons - lubridate! 

daily_data <- sites_long |> 
  mutate(date = floor_date(date, "day")) |> 
  group_by(date,site,species) |> 
  summarise(conc = mean(conc, na.rm = T)) |> 
  ungroup() 

# hour of day 
sites_long |> 
  mutate(hour_of_day = hour(date)) |> 
  group_by(hour_of_day, site, species) |> 
  summarise(conc = mean(conc, na.rm = T)) |> 
  ungroup() |> 
  group_by(site,species) |> 
  filter(conc == max(conc, na.rm = TRUE)) |> 
  ungroup()


sites_long |> 
  mutate(week_day = wday(date, label = TRUE)) |> 
  group_by(week_day, site, species) |> 
  summarise(conc = mean(conc, na.rm = T)) |> 
  ungroup() |> 
  group_by(site,species) |> 
  filter(conc == max(conc, na.rm = TRUE)) |> 
  ungroup()

# TO find the difference between two values in time can use lag funciton 

sites_long |> 
  group_by(site,species) |> 
  arrange(date) |> 
  mutate(prev_conc = lag(conc), conc_diff = conc - prev_conc) |> 
  ungroup()

