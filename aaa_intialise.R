library(here)

# Data --------------------------------------------------------------------

if(!dir.exists(here("data","taught", "part_1"))){
  dir.create(here("data","taught", "part_1"), recursive = T)
}

if(!dir.exists(here("data","taught", "part_1"))){
  dir.create(here("data","taught", "part_2"), recursive = T)
}

if(!dir.exists(here("data","exercise"))){
  dir.create(here("data","exercise"), recursive = T)
}

# Other Folders -----------------------------------------------------------

for (dir in c("planning", "learning_materials", "reference")) {
  if(!dir.exists(here(dir))){
    dir.create(here(dir), recursive = T)
  }
}

