
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(openair)

# Reading data ------------------------------------------------------------

# bad

CLL2 = read.csv("data/part_2/CLL2_2018.csv") %>% tibble()
HORS = read.csv("data/part_2/HORS_2018.csv") %>% tibble()
KC1 = read.csv("data/part_2/KC1_2018.csv") %>% tibble()
LON6 = read.csv("data/part_2/LON6_2018.csv") %>% tibble()
MY1 = read.csv("data/part_2/MY1_2018.csv") %>% tibble()


# better

sites = list.files(path = "data/part_2/", pattern = ".csv") %>% 
  str_remove("_2018.csv")
  
files = list.files(path = "data/part_2/", pattern = ".csv",full.names = T)

df_raw = data.frame()

for(i in 1:length(sites)){
  
  j = read.csv(file = files[[i]]) %>% 
    tibble() %>%
    mutate(site = sites[[i]])
  
  df_raw = rbind(df_raw, j)
  
}

# Fix data type

class(df_raw$no2)
class(df_raw$no)

KC1 = read.csv("data/part_2/KC1_2018.csv") %>% tibble()
KC1
class(KC1$no)

KC1 = read.csv("data/part_2/KC1_2018.csv", na.strings = c("NA", "missing")) %>% tibble()
KC1
class(KC1$no)


# best (for now)

sites = list.files(path = "data/part_2/", pattern = ".csv") %>% 
  str_remove("_2018.csv")

files = list.files(path = "data/part_2/", pattern = ".csv",full.names = T)

df_raw = data.frame()

for(i in 1:length(sites)){
  
  j = read.csv(file = files[[i]], na.strings = c("NA", "missing")) %>% 
    tibble() %>%
    mutate(site = sites[[i]])
  
  df_raw = rbind(df_raw, j)
  
}

df_raw

df = df_raw %>%
  select(-X) %>%
  mutate(date = lubridate::ymd_hms(date),
         nox = no + no2)


# Data manipulation -------------------------------------------------------

## summaries ---

# recall the use of mean
mean(df$nox, na.rm = T)

# we can stay in dataframes by using "summarise"
df %>% summarise(nox = mean(nox, na.rm = T)) # note it is still a dataframe

# can take many arguments
df %>% summarise(mean_nox = mean(nox, na.rm = T),
                 sd_nox   = sd(nox, na.rm = T))

# summarise is best combined with group_by()!
df %>% 
  group_by(site)

df %>%
  group_by(site) %>%
  summarise(mean_nox = mean(nox, na.rm = T),
            sd_nox   = sd(nox, na.rm = T))

df %>%
  group_by(site) %>%
  summarise(mean_nox = mean(nox, na.rm = T),
            sd_nox   = sd(nox, na.rm = T)) %>%
  arrange(mean_nox)

## reshaping ---

df
# talk about e.g. what if you wanted to...
# - calculate summaries for many columns?
# - assign a flag if a pollutant was above a threshold?
# - drop high o3 values but retain no/no2?
# - drop other pollutants... but had a hundred of them?
# answer: reshaping!

pivot_longer(df, no:o3) # notice diference

df %>% pivot_longer(no:o3, names_to = "species", values_to = "conc")

df %>% pivot_longer(-c(site, date), names_to = "species", values_to = "conc") # another way of doing it

df_long = df %>% pivot_longer(-c(site, date), names_to = "species", values_to = "conc")

df_long %>%
  group_by(site, species) %>%
  summarise(mean = mean(conc, na.rm = T),
            sd   =   sd(conc, na.rm = T)) # the same thing we just did above but in a new way!

df_long %>% 
  group_by(site, species) %>% 
  mutate(p95 = quantile(conc, .95, na.rm = T), # can use group_by with mutate!
         flag = conc > p95)

df_long %>% 
  filter(site == "CLL2") # can also filter data (filter for rows, select for columns)

df_long %>%
  group_by(site, species) %>%
  filter(conc == max(conc, na.rm = T)) # group_by works with filter too!

df_long %>% 
  filter(site == "CLL2") %>% 
  select(-site) %>% 
  pivot_wider(names_from = species, values_from = conc) # can go back using pivot_wider

# Data viz -------------------------------------------------------

df # what might we want to plot?

df_daily = timeAverage(df, "day")

ggplot(df_daily) # nothing happens

ggplot(df_daily) + 
  aes(x = date) # we have an axis!

ggplot(df_daily) + 
  aes(x = date, y = nox) # both axes

ggplot(df_daily) + 
  aes(x = date, y = nox) +
  geom_line() # a plot!

ggplot(df_daily) + 
  aes(x = date, y = nox) +
  geom_line() +
  labs(x = "Date", y = "NOx (ppb)") # is that the right unit?

ggplot(df_daily) + 
  aes(x = date, y = nox) +
  geom_line(color = "cadetblue") +
  labs(x = "Date", y = "NOx (ppb)") +
  theme_bw() # finesse 

ggplot(df_daily) + 
  aes(x = date, y = nox) +
  geom_line(alpha = .5, color = "cadetblue") +
  geom_point(color = "cadetblue") +
  geom_smooth(color = "black", lty = 2) +
  labs(x = "Date", y = "NOx (ppb)") +
  theme_bw() # layer geometries

# something about saving??

# bring back in pivoting data

df_daily_long = df %>%
  timeAverage(avg.time = "day", type = "site") %>%
  pivot_longer(-c(site, date), names_to = "species", values_to = "conc")

df_daily_long %>%
  filter(species == "no2") %>%
  ggplot() +
  aes(x = date, y = conc) +
  geom_line() # mixed up!

df_daily_long %>%
  filter(species == "no2") %>%
  ggplot() +
  aes(x = date, y = conc, color = site) +
  geom_line() #that's better!

df_daily_long %>%
  ggplot() +
  aes(x = date, y = conc, color = site) +
  geom_line() +
  facet_wrap(~species) # faceting

df_daily_long %>%
  ggplot() +
  aes(x = date, y = conc, color = site) +
  geom_line() +
  facet_wrap(~species, scales = "free_y", ncol = 1) # free scales, ncol, etc.

df_daily_long %>%
  ggplot() +
  aes(x = date, y = conc, color = site) +
  geom_line() +
  facet_grid(site~species, scales = "free") # or use grid!

df_daily_long %>%
  ggplot() +
  aes(x = date, y = conc, color = site) +
  geom_line() +
  facet_grid(species~., scales = "free_y") +
  theme_bw() +
  theme(legend.position = "top") +
  labs(x = "Date", y = "Concentration (ppb)", color = "Site") # good theming

df_daily_long %>%
  mutate(species = case_when(
    species == "no" ~ "NO",
    species == "no2" ~ "NO[2]",
    species == "nox" ~ "NO[x]",
    species == "o3"  ~ "O[3]"
  )) |> 
  ggplot() +
  aes(x = date, y = conc, color = site) +
  geom_line() +
  facet_grid(species~., scales = "free_y", labeller = label_parsed) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(x = "Date", y = "Concentration (ppb)", color = "Site") 
# this might be far too weird and advanced, but parse the labels

# other plot types...

# bar chart with error bars
df_long %>%
  mutate(species = case_when(
    species == "no" ~ "NO",
    species == "no2" ~ "NO[2]",
    species == "nox" ~ "NO[x]",
    species == "o3"  ~ "O[3]"
  )) |> 
  group_by(site, species) %>%
  summarise(mean = mean(conc, na.rm = T),
            sd   = sd(conc, na.rm = T)) %>%
  ggplot(aes(x = site, y = mean)) +
  geom_col(aes(fill = site), position = position_dodge2()) +
  geom_pointrange(aes(ymax = mean+sd, ymin = mean-sd)) +
  theme_bw() +
  facet_wrap(~species, scales = "free_y", labeller = label_parsed) +
  labs(x = "Site Code", y = "Mean (+/- SD) Concentration (ppb)")

# scatter graph with trendline

coefs = lm(nox ~ no2, df) %>%
  coef() %>%
  signif(3)

equation = paste0("NOX = ", coefs[1], " + ", coefs[2], " NO2")

df %>%
  ggplot(aes(x = no2, y = nox)) +
  geom_point(alpha = .05) +
  geom_smooth(method = "lm", color = "red") +
  geom_smooth(color = "blue") +
  annotate(y = 200, x = 150, geom = "text", label = equation, color = "red") +
  labs(x = quickText("NO2 (ppb)"), y = quickText("NOx")) +
  theme_bw()

# density plots on a log scale with means
df %>%
  group_by(site) %>%
  mutate(mean = mean(nox, na.rm = T)) %>%
  ggplot(aes(color = site)) +
  geom_density(aes(x = nox)) +
  geom_vline(aes(xintercept = mean, color = site), lty = 3, size = 1) +
  scale_x_log10() +
  theme_bw() +
  labs(x = quickText("NOx (ppb)"), color = "Site Code") +
  theme(legend.position = "top")

# Other tidyverse packages --------------------------------------------------------

# forcats

library(forcats)

sizes = c("XS", "S", "M", "L", "XL")
sort(sizes)

sizes_fct = factor(x = sizes, levels = sizes)
sort(sizes_fct)

df %>%
  group_by(site) %>%
  summarise(mean = mean(nox, na.rm = T)) %>%
  ggplot(aes(x = site, y = mean)) +
  geom_col() # not in order!

df %>%
  group_by(site) %>%
  summarise(mean = mean(nox, na.rm = T)) %>%
  ungroup() %>%
  mutate(site = fct_reorder(site, mean)) %>%
  ggplot(aes(x = site, y = mean)) +
  geom_col() # now in the right order!

# broom

library(broom)

lm(nox ~ no2, df) %>% summary() # useful info, hard to access

lm(nox ~ no2, df) %>% glance()  # stats (e.g. r.squared)

lm(nox ~ no2, df) %>% tidy()    # coefficient info as a data frame!

lm(nox ~ no2, df) %>% 
  tidy() %>%
  select(term, estimate) %>%
  mutate(estimate = signif(estimate,3)) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  mutate(equation = paste0("y = ", `(Intercept)`, " + ", no2, " NO2")) # write equation

lm(nox ~ no2, df) %>% augment() # predicted values on original data

lm(nox ~ no2, df) %>% 
  augment() %>%
  ggplot(aes(x = nox, y = .fitted)) +
  geom_point(alpha = .05) +
  geom_abline(color = "green", size = 1)

# purrr

library(purrr)

map(1:5, sqrt)

map_dbl(1:5, sqrt)

tibble(x = 1:5) %>%
  mutate(y = map_dbl(x, sqrt))

# readr
# provides read_csv, similar to read.csv but tries to do the hard work for you in guessing column types and
# is a bit quicker, though is more picky with column names/encoding/etc...

# readxl
# reads excel files - can specify range of cells and the sheet, but basically the same as readr