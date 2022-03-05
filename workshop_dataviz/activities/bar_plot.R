
library(readr)
library(dplyr)
library(ggplot2)

# Read and prep data ------------------------------------------------------

bar_data_raw = read_csv("~/historicalAverageData.csv")

bar_data = bar_data_raw %>% 
  # group by country, year & pollutant
  group_by(country, theYear, name) %>%
  # calculate means for these categories
  summarise(country_avg = mean(mean))

# Create plot -------------------------------------------------------------

bar_data %>% 
  filter(name == "no2", country == "united_kingdom") %>% 
  ggplot(aes())
