
## AFTER - basics on using/installing R/RStudio

library(dplyr) # CONCEPT: packages - what is one? (incl. installing)
library(lubridate)
library(openair)

## CONCEPT: Working directories/filepaths
#setwd(...) 

# Reading in data ---------------------------------------------------------

met = read.csv(file = "data/part_1/MY1_met_2018.csv") |> # CONCEPT: Read CSV
  select(-X) # CONCEPT: Selecting Columns

no = read.csv(file = "data/part_1/MY1_no_2018.csv")

no2 = read.csv(file = "data/part_1/MY1_no2_2018.csv")

o3 = read.csv(file = "data/part_1/MY1_o3_2018.csv")

# Examine Data ------------------------------------------------------------

head(met) # CONCEPT: Examining data in R (different from Excel!)
tail(met)
glimpse(met)
View(met)

# Joining data ------------------------------------------------------------

all_data = left_join(met, no) # CONCEPT: Joining Data

all_data = left_join(all_data, no2)

all_data = left_join(all_data, o3)

head(all_data)


# data tidying ------------------------------------------------------------

head(all_data)

all_data = rename(all_data, # CONCEPT: Renaming Data
                  ws = wind_speed,
                  wd = wind_direction) 

typeof(all_data$date) # CONCEPT: data types (may be discussed earlier?)

tail(all_data)

all_data = mutate(all_data,
                  date = ymd_hms(date)) # CONCEPT: Mutating columns & Lubridate

# IDEA - add another mutate step by separating date into date and time?

typeof(all_data$date)

# Incorporate pipes? If so - CONCEPT: Pipes

# analysis with openair ---------------------------------------------------

timeAverage(all_data) # CONCEPT: openair for analysis... what are we doing with it exactly?

## NB: openair uses tibble rather than data.frame - may need to talk about this...