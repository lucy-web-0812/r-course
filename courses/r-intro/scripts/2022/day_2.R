library(dplyr)
library(lubridate)
library(stringr)

filePaths = list.files("data/data_day_2/",full.names = TRUE)

fileNames = list.files("data/data_day_2/")

site_names = word(fileNames, 1, sep = "_")

myList = list()

for(i in 1:length(filePaths)){
  
  if(site_names[i] == "LON6"){
    myList[[i]] = read.csv(filePaths[i]) %>% 
      tibble() %>% 
      select(-X) %>% 
      mutate(date = as_datetime(date),
             no = ifelse(no == "missing", NA, no) %>% 
               as.numeric(),
             site_name = site_names[i])
    
  }else{
    myList[[i]] = read.csv(filePaths[i]) %>% 
      tibble() %>% 
      select(-X) %>% 
      mutate(date = ymd_hms(date),
             no = ifelse(no == "missing", NA, no) %>% 
               as.numeric(),
             site_name = site_names[i])

  }
  
}

mydata = bind_rows(myList)
