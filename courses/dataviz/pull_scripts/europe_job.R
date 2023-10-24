library(here)
library(saqgetr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)

# Europe ------------------------------------------------------------------

eeaMeta = get_saq_sites() %>% 
  filter(date_start <= ymd_hm("2015-01-01 00:00"),
         date_end >= ymd_hm("2020-12-31 23:59"),
         site_area == "urban",
         site_type %in% c("traffic","background"),
         country %in% c("france", "germany", "spain", "italy",
                        "switzerland","belgium","netherlands","luxembourg"))

pollutants = c("no2","o3","pm2.5")

for(i in 1:nrow(eeaMeta)){
  
  datRaw = tryCatch({
    saqgetr::get_saq_observations(site = eeaMeta$site[i],
                                  variable = pollutants,
                                  start = 2015,
                                  end = 2020,
                                  tz = "UTC",
                                  valid_only = T)},
    error = function(e){
      NULL
    }
  )
  
  if(is.null(datRaw) | nrow(datRaw) == 0){
    next
  }
  
  datParsed = datRaw %>%
    mutate(site_name = eeaMeta$site_name[i],
           variable = ifelse(variable == "pm2.5","pm25",variable))  %>% 
    select(date,
           code = site,
           name = variable,
           value) %>% 
    mutate(date = floor_date(date,"1 month")) %>% 
    group_by(date, name, code) %>%
    summarise_all(c("mean","median"),na.rm = T)
  
  
  fileName = here("workshop_dataviz","data","raw",paste0(eeaMeta$site[i],"__",eeaMeta$country[i],".csv"))
  
  write.csv(datParsed,fileName,row.names = F)
  
}