
## Exercise Doc

library(openair)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# Day 1 -------------------------------------------------------------------

# Reading Data

met = read.csv("data_exercises/aberdeen_met.csv")) %>%
  mutate(date = lubridate::mdy_hms(date))

no  = read.csv(here::here("data/exercises/aberdeen_no.csv")) %>%
  mutate(date = lubridate::mdy_hms(date))

no2 = read.csv(here::here("data/exercises/aberdeen_no2.csv")) %>%
  mutate(date = lubridate::mdy_hms(date))

all = left_join(no, no2, by = "date") %>%
  left_join(met, by = "date") %>%
  mutate(nox = no + no2,
         no2_ratio = no2 / nox) %>% 
  tibble()

plot(x = all$date, y = all$no2_ratio)

# Statistics

mean(all$no2_ratio, na.rm = T)
median(all$no2_ratio, na.rm = T)
sd(all$no2_ratio, na.rm = T)

mod = lm(formula = nox ~ no2, data = all)
summary(mod)

plot(all$no2, all$nox)
abline(reg = mod, col = "cadetblue")

# openair

# this is open-ended, so just have a play!

openair::summaryPlot(all)

openair::calendarPlot(all, year = 2019)

openair::polarPlot(all, pollutant = "nox", type = "year")

# Day 2 -------------------------------------------------------------------

# Reading Data

files = list.files(here::here("data/exercises"), full.names = T)

file_list = list()

for (i in 1:length(files)) {
  
  if (i == 1) {
    dat = read.csv(files[[i]]) %>%
      mutate(date = lubridate::mdy_hms(date))
  } else {
    temp = read.csv(files[[i]]) %>%
      mutate(date = lubridate::mdy_hms(date))
    dat = left_join(dat, temp, by = "date")
    
  }
  
}

dat = tibble(dat)

# Tidyverse

# No need to pivot twice - save your grouped long data

long_dat = dat %>%
  pivot_longer(c(no, ws, no2, air_temp)) %>%
  group_by(name)

mean_sd = long_dat %>%
  summarise(mean = mean(value, na.rm = T),
            sd   = sd(value, na.rm = T))

max_val = long_dat %>%
  filter(value == max(value, na.rm = T)) %>% 
  rename(date_at_max = date, max_val = value) %>% 
  select(-wd)


left_join(mean_sd, max_val)

# ggplot2

# some creative freedom, but here is a rough skeleton:

all %>%
  timeAverage(avg.time = "week") %>%
  mutate(nox = no + no2) %>%
  pivot_longer(c(no, no2, nox)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line(aes(color = name)) +
  facet_wrap(~name, ncol = 1) +
  theme_light() + 
  labs(x = NULL, y = "Concentration (ppb)") +
  theme(legend.position = "none")

# Cape Verde --------------------------------------------------------------

# Challenge 0 (reading)

cape_verde_raw <- read.csv("data/cape_verde.csv") %>% 
  tibble()

cape_verde <- cape_verde_raw %>% 
  rename(ws = ws_m.s,
         wd = wd_deg,
         date = date_time) %>% 
  mutate(date = ymd_hms(date))

# Challenge 1 (diurnals)

# easy: openair
oz_timevar = openair::timeVariation(cape_verde, pollutant = "O3_ppbV", type = "Flag_name")

# medium: pull from openair
dat = oz_timevar$data$hour

dat %>% 
  ggplot(aes(x = hour, y = Mean, ymax = Upper, ymin = Lower)) +
  geom_ribbon(alpha = .5) +
  geom_line(size = 1) +
  theme_light() +
  labs(x = "Hour of Day", y = openair::quickText("Mean O3 (ppb)")) +
  facet_grid(Flag_name~., scales = "free_y")

# more challenging: all in tidyverse! 
# NB: openair gives 95% CI, this is standard error...

ggdat = cape_verde %>% 
  drop_na(O3_ppbV) %>% 
  mutate(hour = hour(date)) %>% 
  group_by(hour, Flag_name) %>% 
  summarise(avg_o3 = mean(O3_ppbV),
            se_o3 = sd(O3_ppbV)/sqrt(n()))

ggplot(ggdat, aes(x = hour, y = avg_o3, ymax = avg_o3 + se_o3, ymin = avg_o3 - se_o3)) +
  geom_ribbon(alpha = .5) +
  geom_line(size = 1) +
  theme_light() +
  labs(x = "Hour of Day", y = openair::quickText("Mean O3 (ppb)")) +
  facet_grid(Flag_name~., scales = "free_y")

ggplot(ggdat, aes(x = hour, y = avg_o3, ymax = avg_o3 + se_o3, ymin = avg_o3 - se_o3)) +
  geom_ribbon(alpha = .5, aes(fill = Flag_name)) +
  geom_line(size = 1, aes(color = Flag_name)) +
  theme_light() +
  labs(x = "Hour of Day", y = openair::quickText("Mean O3 (ppb)")) # or colour instead!

# Challenge 2 (data flags)

voc = cape_verde %>% 
  select(date, contains("pp")) %>% 
  pivot_longer(-date, names_to = "species", values_to = "conc") %>%
  drop_na()

# check to see if anything looks visually strange - some do! (takes a while to plot...)
ggplot(voc, aes(x = date, y = conc)) +
  geom_line() +
  facet_wrap(~species, scales = "free")

# try filtering out the highest quantiles of data - looks better! (takes a while to plot...)
voc %>% 
  group_by(species) %>% 
  filter(conc <= quantile(conc, .995)) %>% 
  ggplot(aes(x = date, y = conc)) +
  geom_line() +
  facet_wrap(~species, scales = "free")

# Challenge 3 (CO2 correlations)

voc_co2 = cape_verde %>% 
  select(contains("pp")) %>%
  janitor::remove_constant() 

# janitor is a helpful package/function to remove constant columns
# you could have also used pivot_longer and drop_na

vocs = voc_co2 %>% select(-CO2_ppmV) %>% names()

for (i in 1:length(vocs)) {
  
  if (i == 1) {
    
    name = vocs[i]
    
    mod = lm(voc_co2$CO2_ppmV ~ voc_co2[[name]])
    
    rsq = summary(mod)$r.squared
    
    df = data.frame(compound = name,
                    rsq = rsq)
    
  } else {
    
    name = vocs[i]
    
    mod = lm(voc_co2$CO2_ppmV ~ voc_co2[[name]])
    
    rsq = summary(mod)
    
    rsq = summary(mod)$r.squared
    
    temp = data.frame(compound = name, rsq = rsq)
    
    df = bind_rows(df, temp)
    
  }
  
}

df

# Visualise 
# (including bonus trick using forcats to put them in order!)

df %>% 
  mutate(compound = forcats::fct_reorder(compound, rsq)) %>% 
  ggplot(aes(y = compound, x = rsq)) +
  geom_col() +
  scale_x_continuous(limits = c(0,1))
