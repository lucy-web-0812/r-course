library(here)
library(magrittr)
library(dplyr)
library(openair)
library(tidyr)
library(purrr)


# -------------------------------------------------------------------------

add_bad_flag = function(df){
  bad_rows = floor(runif(nrow(df)*0.25,1,nrow(df)))
  df[["value"]][bad_rows] = -99999
  
  df
}

# -------------------------------------------------------------------------


if(!dir.exists(here("data"))){ # if dirs haven't been set up, do that
  source(here("aaa_intialise.R"))
}

set.seed(10)

# Part 1 ------------------------------------------------------------------

spec = c("no","no2","o3")

my1 = importAURN(site = "my1", year = 2018, pol = c(spec,"ws","wd")) %>% 
  select(-site,-code)

my1_pol_list = my1 %>% 
  select(date,no:o3) %>% 
  pivot_longer(-date) %>% 
  nest_by(name) %$% # a legit reason to use the explode pipe!
  map2(data,name,
       ~.x %>% 
         rename(!!.y := value) %>% 
         return(.))

my1_met = my1 %>% 
  select(date,
         wind_speed = ws,
         wind_direction = wd)


walk2(my1_pol_list,spec,~write.csv(.x,here("data","part_1",paste0("MY1_",.y,"_2018.csv")),row.names = F))

write.csv(my1_met,here("data","part_1","MY1_met_2018.csv"))


# Part 2 ------------------------------------------------------------------

sites = c("my1","kc1","cll2","HORS","LON6")

dat = expand.grid(spec,sites,stringsAsFactors = F) %>% 
  tibble() %>% 
  set_names(c("specs", "sites")) %>% 
  rowwise() %>% 
  mutate(data = tryCatch({importAURN(site = sites,
                           year = 2018,
                           pollutant = specs) %>% 
      rename(value = specs)},
                         error = function(e){
                           NULL
                         }) %>% 
           list()
  ) %>% 
  ungroup()

dat$data[[4]] = add_bad_flag(dat$data[[4]])

dat_final = dat %>% 
  unnest(data) %>% 
  pivot_wider(names_from = specs) %>% 
  select(-site,-sites) %>% 
  nest_by(code) %>%
  rowwise() %>% 
  mutate(data = mutate(data,no = ifelse(no == -99999,"missing",no)) %>% list())

walk2(dat_final$data,dat_final$code,~write.csv(.x,here("data","part_2",paste0(.y,"_2018.csv"))))


# Cape Verde --------------------------------------------------------------

cape_verde = read.csv("data/cape_verde_raw.csv") |> 
  tibble() |> 
  filter(year != 2006) |> 
  mutate(Flag_name = if_else(Flag_name == "", NA_character_, Flag_name) |> 
           zoo::na.locf()) |> 
  rename(date_time = 1)

write.csv(cape_verde, file = "data/cape_verde.csv",row.names = F)


# exercise data ----------------------------------------------------------------

aberdeen_raw = importAURN("ABD8", year = 1990:2020)

aberdeen = aberdeen_raw %>%
  filter(lubridate::year(date) >= 2019) %>%
  select(-site, -code) %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date)) %>%
  separate(date, c("date", "time"), sep = " ") %>%
  mutate(date = glue::glue("{month}-{day}-{year} {time}")) %>%
  select(-time, -month, -day, -year, -nox)

aberdeen_no2 = select(aberdeen, date, no2)
aberdeen_no = select(aberdeen, date, no)
aberdeen_met = select(aberdeen, -(no2:no))

write_csv(aberdeen_no2, "data/exercises/aberdeen_no2.csv")
write_csv(aberdeen_no, "data/exercises/aberdeen_no.csv")
write_csv(aberdeen_met, "data/exercises/aberdeen_met.csv")

