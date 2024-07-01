# Ch 10
# Exercises
library(tidyverse)
library(tidycensus)
library(tigris)
library(tmap)
library(survey)
library(srvyr)
library(scales)

options(tigris_use_cache = TRUE)

# exercisez -------------------------------------------------------------------
#-------------------------------------------------------------------------------
nv_pums <- get_pums(
  variables = c("SEX", "AGEP"),
  state = "NV",
  survey = "acs5",
  year = 2020,
  recode = TRUE
)

nv_pums %>%
  count(SEX_label, AGEP, wt = PWGTP)

nv_pums %>%
  filter(AGEP >= 65) %>%
  count(SEX, wt = PWGTP)

get_acs(
  geography = "state",
  state = "NV",
  variables = c("DP05_0030", "DP05_0031"),
  year = 2020
)

# get weights with this

nv_pums_repl <- get_pums(
  variables = c("SEX", "AGEP"),
  state = "NV",
  recode = TRUE,
  year = 2020,
  rep_weights = "person"
)

nv_svy <- nv_pums_repl %>%
  to_survey(type = "person",
            design = "rep_weights") 

nv_svy %>%
  filter(AGEP >= 65) %>%
  survey_count(SEX_label)
  



