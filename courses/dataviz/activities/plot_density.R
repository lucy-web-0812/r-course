
library(readr)
library(dplyr)
library(ggplot2)

# Read and prep data ------------------------------------------------------

density_data_raw = read_csv("~/historicalAverageData.csv")

density_data = density_data_raw %>%
  # filter for just the UK, France & Switzerland
  filter(country %in% c("united_kingdom", "france", "switzerland"))

# Create plot -------------------------------------------------------------

density_data %>% 
  filter(country == "united_kingdom",
         name == "no2") %>% 
  ggplot(aes())



