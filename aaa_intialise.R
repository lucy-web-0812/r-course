library(here)

for(dir in c("data","plots","scripts")){
  if(!dir.exists(here(dir,"part_1"))){
    dir.create(here(dir,"part_1"), recursive = T)
  }
  
  if(!dir.exists(here(dir,"part_2"))){
    dir.create(here(dir,"part_2"), recursive = T)
  }

}

