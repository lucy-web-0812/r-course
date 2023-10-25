library(here)
library(rstudioapi)

# setup -------------------------------------------------------------------

if(!dir.exists(here("data","vis"))){
  dir.create(here("data","vis"))
}

if(!dir.exists(here("data","vis","raw"))){
  dir.create(here("data","vis","raw"))
}



# Go ----------------------------------------------------------------------

jobRunScript(here("workshop_dataviz","pull_scripts","europe_job.R"))

jobRunScript(here("workshop_dataviz","pull_scripts","uk_job.R"))
