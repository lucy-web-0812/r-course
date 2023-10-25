library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(ggplot2)
library(openair)

setwd("C:/Users/jd6th/Documents/course_data/data_taught/")

# Read All Data -----------------------------------------------------------

files = list.files("data_day_2/", full.names = TRUE, pattern = ".csv")

filesShort = list.files("data_day_2/", full.names = FALSE, pattern = ".csv")

siteNames = word(filesShort,sep = "_", start = 1)

datList = list()

for(index in 1:length(files)){
  
  datList[[index]] = read.csv(files[index], na.strings = c("NA","missing")) %>% 
    tibble() %>% 
    select(-X) %>% 
    mutate(date = ymd_hms(date),
           site = siteNames[index])
  
}

dat = bind_rows(datList)

# Data Manipulation -------------------------------------------------------

dat_long = dat %>% 
  pivot_longer(-c(date,site), 
               names_to = "poll", 
               values_to = "conc")

dat_long %>%
  group_by(site, poll) %>% 
  summarise(mean = mean(conc, na.rm = T),
            sd = sd(conc, na.rm = T),
            early_date = min(date)) %>%
  mutate(upper = mean + (2*sd),
         lower = mean - (2*sd))

dat_long %>%
  group_by(site, poll) %>%
  mutate(p95 = quantile(conc, .95, na.rm = T),
         flag = conc > p95)

dat_long %>%
  filter(site %in% c("MY1","CLL2"),
         conc > 100)

dat_long %>% 
  group_by(site, poll) %>% 
  filter(conc == max(conc, na.rm = T))

dat %>%
  pivot_wider(names_from = site,
              values_from = no:o3)

dat_long


# Visualisations ----------------------------------------------------------

dat_daily = timeAverage(dat, "day", type = "site")

ggplot(data = dat_daily, aes(x = date, y = no)) +
  geom_line(aes(color = site, linetype = site)) +
  geom_point(aes(shape = site)) +
  facet_wrap(~site, ncol = 1, scales = "free_y") +
  labs(x = "Measurement Date", y = "NO (ppb)")

dat_daily_long = dat_daily %>% 
  pivot_longer(no:o3, names_to = "poll", values_to = "conc")

ggplot(data = dat_daily_long, aes(x = date, y = conc)) +
  geom_line(aes(color = site)) +
  facet_wrap(~poll, ncol = 1, scales = "free_y") +
  labs(x = "Measurement Date", y = "NO (ppb)", color = "Site Name") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("#000000", "cadetblue", "hotpink", "goldenrod", "lightsalmon"))
