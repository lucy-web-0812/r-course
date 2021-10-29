library(dplyr)
library(lubridate)
library(here)
library(stringr)

files = list.files(here("data","part_2"))

# instead of writing the path each time, lets be lazy!
files = list.files(here("data","part_2"), full.names = T)

cll2 = read.csv(files[1]) %>% 
  tibble()

cll2 = read.csv(files[1]) %>% 
  tibble() %>% 
  select(-X) %>% 
  mutate(date = ymd_hms(date))

hors = read.csv(files[2]) %>% 
  tibble() %>% 
  select(-X) %>% 
  mutate(date = ymd_hms(date))

kc1 = read.csv(files[3]) %>% 
  tibble() %>% 
  select(-X) %>% 
  mutate(date = ymd_hms(date))

lon6 = read.csv(files[4]) %>% 
  tibble() %>% 
  select(-X) %>% 
  mutate(date = ymd_hms(date))

my1 = read.csv(files[5]) %>% 
  tibble() %>% 
  select(-X) %>% 
  mutate(date = ymd_hms(date))


# if we want to join these, we need to be able to tell them apart....

cll2 = read.csv(files[1]) %>% 
  tibble() %>% 
  select(-X) %>% 
  mutate(date = ymd_hms(date),
         site = "cll2")

hors = read.csv(files[2]) %>% 
  tibble() %>% 
  select(-X) %>% 
  mutate(date = ymd_hms(date),
         site = "hors")

kc1 = read.csv(files[3]) %>% 
  tibble() %>% 
  select(-X) %>% 
  mutate(date = ymd_hms(date),
         site = "kc1")

lon6 = read.csv(files[4]) %>% 
  tibble() %>% 
  select(-X) %>% 
  mutate(date = ymd_hms(date),
         site = "lon6")

my1 = read.csv(files[5]) %>% 
  tibble() %>% 
  select(-X) %>% 
  mutate(date = ymd_hms(date),
         site = "my1")


dat = bind_rows(cll2, hors, kc1, lon6, my1)
# errs on kc1
cll2

hors

kc1 # ahah!

# lets fix kc1s read

kc1 = read.csv(files[3]) %>% 
  tibble() %>% 
  select(-X) %>% 
  mutate(date = ymd_hms(date),
         no = as.numeric(no),
         site = "kc1")

# hmmm, warnings are ok, but you can end up with side effects
# can we be better?

kc1 = read.csv(files[3]) %>% 
  tibble() %>% 
  select(-X) %>% 
  mutate(date = ymd_hms(date),
         no = ifelse(no == "missing", NA, no) %>% 
           as.numeric(),
         site = "kc1")

dat = bind_rows(cll2, hors, kc1, lon6, my1) #yay


# Flow Control ------------------------------------------------------------
# can we be even lazier?!?!?

for(i in 1:length(files)){ # talk through what a loop is doing
  
  # two problems, how do we stop overwriting the previous data?
  dat = read.csv(files[i]) %>% 
    tibble() %>% 
    select(-X) %>% 
    mutate(date = ymd_hms(date),
           no = ifelse(no == "missing", NA, no) %>% 
             as.numeric(),
           site = "kc1") # how do we address the site issue?
}


# The storage issue -------------------------------------------------------

datList = list()
for(i in 1:length(files)){ # talk through what a loop is doing
  
  datList[[i]] = read.csv(files[i]) %>% 
    tibble() %>% 
    select(-X) %>% 
    mutate(date = ymd_hms(date),
           no = ifelse(no == "missing", NA, no) %>% 
             as.numeric(),
           site = "kc1") 
}

dat = bind_rows(list)
# OR

for(i in 1:length(files)){ # talk through what a loop is doing
  
  if(i == 1){
    dat = read.csv(files[i]) %>% 
      tibble() %>% 
      select(-X) %>% 
      mutate(date = ymd_hms(date),
             no = ifelse(no == "missing", NA, no) %>% 
               as.numeric(),
             site = "kc1") 
  }else{
    temp = read.csv(files[i]) %>% 
      tibble() %>% 
      select(-X) %>% 
      mutate(date = ymd_hms(date),
             no = ifelse(no == "missing", NA, no) %>% 
               as.numeric(),
             site = "kc1") 
    
    dat = bind_rows(dat,temp)
  }
  
}


# The site problem --------------------------------------------------------

someText = "The quick brown fox jumped over the lazy dog"

word(someText,start = 2)

someMoreText = "The_quick_brown_underscore_separated_dog"

word(someMoreText)

word(someMoreText,sep = "_")

word(someMoreText,start = 3,sep = "_")

list.files(here("data","part_2"))

sites = list.files(here("data","part_2")) %>% 
  word(1,sep = "_")

# compact loop from earlier

datList = list()

for(i in 1:length(files)){
  
  datList[[i]] = read.csv(files[i]) %>% 
    tibble() %>% 
    select(-X) %>% 
    mutate(date = ymd_hms(date),
           no = ifelse(no == "missing", NA, no) %>% 
             as.numeric(),
           site = sites[i]) 
}

dat = bind_rows(datList)



# Pivoting... -------------------------------------------------------------

# Tidy data...

dat # look at dat
# tidy ethos chat

library(tidyr) # add to top of script

dat %>% 
  pivot_longer(cols = c(no, no2, o3))

# make the point about key columns
# Database-ing "The key, the whole key, and nothing but the key"
# in our bound data, the date value was repeted for each site
# "long"

dat$date %>% plot

# true wide data would actually look like

dat %>% 
  pivot_wider(values_from = c("no","no2","o3"),names_from = "site")

dat %>% 
  pivot_longer(cols = c(no, no2, o3), names_to = "species")

datLong = dat %>% 
  pivot_longer(cols = c(no, no2, o3), names_to = "species")


# Filter

datLong %>% 
  filter(site == "MY1", species == "no")

datLong %>% 
  filter(site == "MY1", species %in% c("no","no2")) # can be found in

datLong %>% 
  filter(site == "MY1", species %in% c("no","no2"),
         between(date,ymd_hm("2018-03-01 00:00"),ymd_hm("2018-03-31 23:59"))) # can be found in

datLong %>% 
  mutate(h = hour(date)) %>% 
  filter(site == "MY1", species %in% c("no","no2"),
         h %in% c(8:20))

datLong %>% 
  filter(site == "MY1", species %in% c("no","no2"),
         hour(date) %in% c(8:20))


# ggplot ------------------------------------------------------------------
# the other benefit of long data

## dear god this could go on for hours

library(ggplot2) # put at top

plottingData = datLong %>% 
  filter(species == "no", site == "MY1")

#talk througth WTF is happening here

plot(plottingData$date, plottingData$value)

ggplot(plottingData)+
  geom_point(aes(x = date, y = value))

plottingData = datLong %>% 
  filter(site == "MY1")

ggplot(plottingData)+
  geom_point(aes(date, value, colour = species))

ggplot(plottingData)+
  geom_point(aes(date, value, shape = species))

ggplot(plottingData)+
  geom_point(aes(date, value, colour = species, size = value))

ggplot(datLong)+
  geom_point(aes(date, value, colour = species))+
  facet_wrap(~site)

# Make a diurnal ----------------------------------------------------------









