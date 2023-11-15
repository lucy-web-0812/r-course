library(dplyr)
library(lubridate)


# And this is the way using the pipe
my1_no <- read.csv("courses/r-intro/data/taught/part_1/MY1_no_2018.csv") |> 
  tibble() |> 
  mutate(date = ymd_hms(date, tz = "GMT"))

my1_no2 <- read.csv("courses/r-intro/data/taught/part_1/MY1_no2_2018.csv") |> 
  tibble() |> 
  mutate(date = ymd_hms(date, tz = "GMT"))

my1_o3 <- read.csv("courses/r-intro/data/taught/part_1/MY1_o3_2018.csv") |> 
  tibble() |> 
  mutate(date = ymd_hms(date, tz = "GMT"))

my1_met <- read.csv("courses/r-intro/data/taught/part_1/MY1_met_2018.csv") |> 
  tibble() |> 
  mutate(date = ymd_hms(date, tz = "GMT"))

my1 <- my1_no |> 
  left_join(my1_no2, by = "date") |> 
  left_join(my1_o3, by = "date" ) |> 
  left_join(my1_met, by = "date" )

# ---- Statistics ---- 

# Summary of data of first the columns and then the whole dataset
summary(my1$no)
summary(my1)

hist(my1$no)
density(my1$no2, na.rm = TRUE) |> plot()


mod <- lm(no2 ~ no, data = my1) # Creating a linear model 
mod_sum <- summary(mod)
coefficients <- coef(mod)

# Have to save the summary to get the R2 value 
mod_sum$r.squared

plot(my1$no, my1$no2)
abline(a = coefficients[1], b = coefficients[2], col = "violet")

# ---- Afternoon Session ---- 


