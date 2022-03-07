
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read and prep data ------------------------------------------------------

codes = c("lu0101a", "lu0102a", "ch0010a", "ch0011a", "MY1", "KC1")

scatter_data_raw = read_csv("~/timeSeriesData.csv")

scatter_data = scatter_data_raw %>%
  # filter just for the codes we're interested in
  filter(code %in% codes) %>%
  # drop the median column - we don't need it
  select(-median) %>%
  # reshape so the pollutants are in their own columns
  pivot_wider(names_from = name, values_from = mean)

# Create plot -------------------------------------------------------------

scatter_data %>% 
  filter(code == "MY1") %>% 
  ggplot(aes())
