library(here)

# Data --------------------------------------------------------------------

if(!dir.exists(here("course_r-intro", "data","taught", "part_1"))){
  dir.create(here("course_r-intro", "data","taught", "part_1"), recursive = T)
}

if(!dir.exists(here("course_r-intro", "data","taught", "part_1"))){
  dir.create(here("course_r-intro", "data","taught", "part_2"), recursive = T)
}

if(!dir.exists(here("course_r-intro", "data","exercise"))){
  dir.create(here("course_r-intro", "data","exercise"), recursive = T)
}

# Other Folders -----------------------------------------------------------

for (dir in c("planning", "learning_materials", "reference")) {
  if(!dir.exists(here("course_r-intro", dir))){
    dir.create(here("course_r-intro", dir), recursive = T)
  }
}

