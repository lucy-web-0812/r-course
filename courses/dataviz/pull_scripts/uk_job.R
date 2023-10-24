library(here)
library(openair)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(lubridate)

aurnMeta = importMeta(source = "aurn") %>% 
  mutate(city = word(site,1)) %>% 
  filter(site_type %in% c("Urban Background","Urban Traffic"))

datMeta = list()

pollutants = c("no2","o3","pm2.5")

for(i in 1:nrow(aurnMeta)){
  
  datRawList = list()
  
  for(j in 1:length(pollutants)){
    
    datRawList[[j]] = tryCatch({
      importAURN(site = aurnMeta$code[i],
                 pollutant = pollutants[j],
                 year = 2015:2020)},
      error = function(e){
        NULL
      }
    )
    
  }
  
  datRaw = purrr::discard(datRawList,function(x){is.null(x)})
  
  if(is.null(datRaw) | length(datRaw) == 0){
    next
  }
  
  datParsed = datRaw %>%
    map_df(~.x %>% 
             pivot_longer(-c(date,site,code))) %>% 
    mutate(name = ifelse(name == "pm2.5","pm25",name),
           date = floor_date(date,"1 month")) %>% 
    select(-site) %>% 
    group_by(date, name, code) %>%
    summarise_all(c("mean","median"), na.rm = T)
  
  fileName = here("workshop_dataviz","data","raw",paste0(aurnMeta$code[i],"__united_kingdom.csv"))
  
  write.csv(datParsed,fileName,row.names = F)
  
}

