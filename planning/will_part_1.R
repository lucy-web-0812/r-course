

setwd("C:/Users/Will/Google Drive/r_projects/wacl_r_intro/") # the course is an example of why this is terrible practise

no = read.csv("data/part_1/MY1_no_2018.csv")

no

library(dplyr)

glimpse(no)
View(no)

tibble(no)

no = tibble(no)
# equivalence example for pipe. 
no = read.csv("data/part_1/MY1_no_2018.csv") %>% 
  tibble()

# discuss data types a little, chr, dbl, date

class(no$date)

library(lubridate)

no$date = ymd_hms(no$date)

no

class(no$date)

# What I'd do is maintain a script and a scratch file
# Then tidy the script as we introduce new concepts

library(dplyr)
library(lubridate)

setwd("C:/Users/Will/Google Drive/r_projects/wacl_r_intro/")

no = read.csv("data/part_1/MY1_no_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) 


no2 = read.csv("data/part_1/MY1_no2_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

o3 = read.csv("data/part_1/MY1_o3_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

met = read.csv("data/part_1/MY1_met_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

met %>% 
  select(-X)

met = read.csv("data/part_1/MY1_met_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(-X)

plot(no$date, no$no)

plot(no$date, no$no, type = "l")

plot(no$date, no$no, type = "l", col = "red")

lines(no2$date, no2$no2, type = "l")


# merging -----------------------------------------------------------------

no_no2 = left_join(no, no2, by = "date")

plot(no_no2$no, no_no2$no2)


no_no2_o3 = left_join(no_no2, o3, by = "date") # tedious!


dat = no %>% 
  left_join(no2, by = "date") %>% 
  left_join(o3, by = "date") %>% 
  left_join(met, by = "date")




# script update -----------------------------------------------------------

library(dplyr)
library(lubridate)

setwd("C:/Users/Will/Google Drive/r_projects/wacl_r_intro/")

no = read.csv("data/part_1/MY1_no_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) 


no2 = read.csv("data/part_1/MY1_no2_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

o3 = read.csv("data/part_1/MY1_o3_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

met = read.csv("data/part_1/MY1_met_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(-X)

dat = no %>% 
  left_join(no2, by = "date") %>% 
  left_join(o3, by = "date") %>% 
  left_join(met, by = "date") %>%
  mutate(nox = no+no2)


# "Standard" Stats --------------------------------------------------------

mean(dat$no)

mean(dat$no, na.rm = T)

mean(dat$wind_direction, na.rm = T) # averaging wind direction warning

median(dat$o3, na.rm = T)

sd(dat$wind_speed, na.rm = T)

range(dat$nox, na.rm = T)

hist(dat$no)

density(dat$no)

na.omit(dat$no) %>% 
  density() 

na.omit(dat$no) %>% 
  density() %>% 
  plot()


plot(dat$no,dat$nox)

# good oppertuinty to introduce help files

?lm

lm(dat$nox~dat$no) # linear model is not the best choice here becuase of error in x and y
# but not the important part of this excerise

lm(dat$nox~dat$no) %>% 
  summary()

linearModel = lm(dat$nox~dat$no)

summary(linearModel)

coef(linearModel)

# how much square bracket notation is worth it here?
coef(linearModel)[1]

coef(linearModel)[2]


plot(dat$no,dat$nox)

?abline

abline(a = coef(linearModel)[1], b = coef(linearModel)[2], col = "red")
# stop base plotting now Will, this is wayyyy more than enough.


# script update -----------------------------------------------------------

library(dplyr)
library(lubridate)

setwd("C:/Users/Will/Google Drive/r_projects/wacl_r_intro/")

no = read.csv("data/part_1/MY1_no_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) 


no2 = read.csv("data/part_1/MY1_no2_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

o3 = read.csv("data/part_1/MY1_o3_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

met = read.csv("data/part_1/MY1_met_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(-X)

dat = no %>% 
  left_join(no2, by = "date") %>% 
  left_join(o3, by = "date") %>% 
  left_join(met, by = "date") %>%
  mutate(nox = no+no2)

# plot nox~no linear model
linearModel = lm(dat$nox~dat$no)

plot(dat$no,dat$nox)
abline(a = coef(linearModel)[1], b = coef(linearModel)[2], col = "red")


# -------------------------------------------------------------------------

library(openair)

# the wind direction averaging point could be linked here, but it might be a bit subtle? could circle back later
summaryPlot(dat)

timePlot(dat, pollutant = "no")

timePlot(dat, pollutant = c("no","no2")) # making the assumption that c() has been taught already

timePlot(dat, pollutant = c("no","no2"), y.relation = "free")

timePlot(dat, pollutant = c("no","no2"), y.relation = "free", avg.time = "1 day")


dat_daily = timeAverage(dat,"1 day")

# time to bring up renaming ws and wd

dat_daily$wind_direction %>% 
  hist(breaks = 100)

dat = dat %>% 
  rename(ws = wind_speed, 
         wd = wind_direction)

dat_daily2 = timeAverage(dat,"1 day")

dat_daily2$wd %>% 
  hist(breaks = 100,add = T,
       col = "red")

# script update -----------------------------------------------------------

library(dplyr)
library(lubridate)
library(openair)

setwd("C:/Users/Will/Google Drive/r_projects/wacl_r_intro/")

no = read.csv("data/part_1/MY1_no_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) 


no2 = read.csv("data/part_1/MY1_no2_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

o3 = read.csv("data/part_1/MY1_o3_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

met = read.csv("data/part_1/MY1_met_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(-X)

dat = no %>% 
  left_join(no2, by = "date") %>% 
  left_join(o3, by = "date") %>% 
  left_join(met, by = "date") %>%
  mutate(nox = no+no2) %>% 
  rename(ws = wind_speed,
         wd = wind_direction)

# plot nox~no linear model
linearModel = lm(dat$nox~dat$no)

plot(dat$no,dat$nox)
abline(a = coef(linearModel)[1], b = coef(linearModel)[2], col = "red")

# plot daily average no and no2

timePlot(dat, pollutant = c("no","no2"), y.relation = "free", avg.time = "1 day")

# -------------------------------------------------------------------------


timeVariation(dat, pollutant = c("no","no2","o3"))

timeVariation(dat, pollutant = c("no","no2","o3"), type = "season")

dat_tv = timeVariation(dat, pollutant = c("no","no2","o3"))

dat_tv$data$hour # make them aware of this? its long data though so a day 2 thing to do in detail?

windRose(dat) # ws and wd already formatted :)

windRose(dat,paddle = F)

polarPlot(dat, pol = "no") # go to google maps for a quick chat about what it means

corPlot(dat) #?

# script update -----------------------------------------------------------

library(dplyr)
library(lubridate)
library(openair)

setwd("C:/Users/Will/Google Drive/r_projects/wacl_r_intro/")

no = read.csv("data/part_1/MY1_no_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) 


no2 = read.csv("data/part_1/MY1_no2_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

o3 = read.csv("data/part_1/MY1_o3_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

met = read.csv("data/part_1/MY1_met_2018.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(-X)

dat = no %>% 
  left_join(no2, by = "date") %>% 
  left_join(o3, by = "date") %>% 
  left_join(met, by = "date") %>%
  mutate(nox = no+no2) %>% 
  rename(ws = wind_speed,
         wd = wind_direction)

# plot nox~no linear model
linearModel = lm(dat$nox~dat$no)

plot(dat$no,dat$nox)
abline(a = coef(linearModel)[1], b = coef(linearModel)[2], col = "red")

# plot daily average no and no2

timePlot(dat, pollutant = c("no","no2"), y.relation = "free", avg.time = "1 day")

# plot diurnals

timeVariation(dat, pollutant = c("no","no2","o3"))

# plot polar

polarPlot(dat, pol = "no")