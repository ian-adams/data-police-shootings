# Source file
# this can be run from any analysis file as source("filename") and it will run everything below

## libraries

library(tidyverse)
library(readr)
library(here)


## set here location
here::here("WAPO data-police-shootings")

## Wapo data location - two files, one with shootings, one with agency information
urlfile="https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/v2/fatal-police-shootings-data.csv"

df_full <-read_csv(url(urlfile))

urlfile="https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/v2/fatal-police-shootings-agencies.csv"

df_agency <- read_csv(url(urlfile))

## Merge on agency id

df_agency <- df_agency %>% rename("agency_state" = "state")
df_agency <- df_agency %>% rename("agency_name" = "name")

df_agency <- df_agency %>% rename("agency_ids" = "id")
df_agency$agency_ids <- as.character(df_agency$agency_ids)
df_full$agency_ids <- as.character(df_full$agency_ids)

df_all <- left_join(df_full,df_agency, by= "agency_ids")


## Save out

write_rds(df_all, file = here("Data", "Clean", "WAPO_clean.rds"))

write_csv(df_all, file = here("Data", "Clean", "WAPO_clean.csv"))