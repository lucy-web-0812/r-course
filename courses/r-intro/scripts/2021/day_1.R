library(dplyr)
library(lubridate)
library(openair)

setwd("C:/Users/jd6th/Documents/course_data/data_taught/")

# Read in data ------------------------------------------------------------

no = read.csv(file = "data_day_1/MY1_no_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

no2 = read.csv(file = "data_day_1/MY1_no2_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

o3 = read.csv(file = "data_day_1/MY1_o3_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

met = read.csv(file = "data_day_1/MY1_met_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(-X)


# Join data ---------------------------------------------------------------

dat = left_join(no, no2, by = "date") %>% 
  left_join(o3, by = "date") %>% 
  left_join(met, by = "date") %>% 
  mutate(nox = no+no2) %>% 
  rename(ws = wind_speed,
         wd = wind_direction)


# Data Exploration --------------------------------------------------------

# density function
na.omit(dat$no) %>%
  density() %>%
  plot()

# linear model
lmod = lm(formula = nox ~ no, data = dat)

lmod_summary = summary(lmod)

rsq = lmod_summary$r.squared

int = coef(lmod)["(Intercept)"]
grad = coef(lmod)["no"]

plot(dat$no, dat$nox)
abline(a = int, b = grad, col = "red")


# openair -----------------------------------------------------------------

timeAverage(dat, avg.time = "1 day")

summaryPlot(dat)

timePlot(dat, pollutant = c("no","no2"), y.relation = "free", avg.time = "day")

timeVariation(dat, pollutant = c("nox","o3"), type = "season")

windRose(dat, paddle = F)

polarPlot(dat, pollutant = "nox")

corPlot(dat,dendrogram = T)
