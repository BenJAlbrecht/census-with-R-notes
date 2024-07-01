# Ch 10
# Analyzing Cenus microdata
library(tidyverse)
library(tidycensus)
library(tigris)
library(tmap)
library(survey)
library(srvyr)
library(scales)

options(tigris_use_cache = TRUE)

# PUMS data & the tidyverse ----------------------------------------------------
#-------------------------------------------------------------------------------
# basic sample PUMS data from the 2016-2020 ACS for Mississippi
ms_pums <- get_pums(
  variables = c("SEX", "AGEP"),
  state = "MS",
  survey = "acs5",
  year = 2020,
  recode = TRUE
)
# no. of people in Mississippi
sum(ms_pums$PWGTP) # 2,981,835

# similar calculations with tidyverse tools
ms_pums %>%
  count(wt = PWGTP)

# count() has the additional benefit of allowing for the specification
# of one or more cols that will be grouped and tabulated
# for ex,
# we could tabulate data by unique vals of age and sex in MS
# the wt arg in count() specifies the col as the appropriate
# weight for data tbulation
ms_pums %>%
  count(SEX_label, AGEP, wt = PWGTP)

# We can also perform more custom analyses such as tabulating the number of
# people over age 65 by sex in Mississippi
ms_pums %>%
  filter(AGEP >= 65) %>%
  count(SEX, wt = PWGTP)

# lets use get_acs() to check our answer:
ms_acs_cheq <- get_acs(
  geography = "state",
  state = "MS",
  variables = c("DP05_0030", "DP05_0031"),
  year = 2020
)
# remember that microdata tabulations are done with a smaller subsample of info
# so it wont always match those from ACS data

# PUMS data can produce highly detailed estimates not available in the regular
# aggregate ACS...

# EX:
# rent burden, family type, and race/ethnicity 
# question:
# how does rent burden vary by race/ethnicity and household type for Mississippi
# households?
hh_vars <- c("PUMA", "GRPIP", "RAC1P",
             "HISP", "HHT")
ms_hh_data <- get_pums(
  variables = hh_vars,
  state = "MS",
  year = 2020,
  variables_filter = list(
    SPORDER = 1,
    TEN = 3
  ),
  recode = TRUE
)

# to analyze rent burdens wrt marital status and race/ethnicity
# lets do some additional recoding
ms_hh_recoded <- ms_hh_data %>%
  mutate(
    race_ethnicity = case_when(
      HISP != "01" ~ "Hispanic",
      HISP == "01" & RAC1P == "1" ~ "White",
      HISP == "01" & RAC1P == "2" ~ "Black",
      TRUE ~ "Other"
    ),
    married = case_when(
      HHT == "1" ~ "Married",
      TRUE ~ "Not married"
    )
  )

# percent of hh paying 40% or more of their incomes in rent
ms_hh_summary <- ms_hh_recoded %>%
  filter(race_ethnicity != "Other") %>%
  group_by(race_ethnicity, married) %>%
  summarize(
    prop_above_40 = sum(WGTP[GRPIP >= 40] / sum(WGTP))
  ) %>%
  arrange(desc(prop_above_40))

non_married_penalty <- ms_hh_summary %>%
  group_by(race_ethnicity) %>%
  summarize(
    unmarried_penalty = prop_above_40[married == "Not married"] - prop_above_40[married == "Married"]
  ) %>%
  arrange(desc(unmarried_penalty))
  
# ms pumas
ms_pumas <- pumas(
  "MS",
  year = 2020
)

plot(ms_pumas$geometry)

# to geographically visualize our info we need to group by PUMA cols
# then filter for the combination of variables that represent the group
# thatthe analyst wants to visualize
# lets look at unmarried black households by PUMA
ms_data_for_map <- ms_hh_recoded %>%
  group_by(race_ethnicity, married, PUMA) %>%
  summarize(
    percent_above_40 = 100 * (sum(WGTP[GRPIP >= 40]) / sum(WGTP))
  ) %>%
  filter(race_ethnicity == "Black",
         married == "Not married")

joined_pumas <- ms_pumas %>%
  left_join(ms_data_for_map,
            by = c("PUMACE10" = "PUMA"))

tm_shape(joined_pumas) +
  tm_polygons(col = "percent_above_40",
              palette = "Reds",
              title = "% rent-burdened\nunmarried Black households") +
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "right")
#-------------------------------------------------------------------------------


# Survey design and the ACS PUMS -----------------------------------------------
#-------------------------------------------------------------------------------
# the ACS is based on a sample, so we have sample errror
# especially for small-sub populations in the PUMA data

# getting replicate weights

# code below re-downloads the mississippi rent burden dataset but with HH
# replicated weights
ms_hh_replicate <- get_pums(
  variables = c("TEN", hh_vars),
  state = "MS",
  recode = TRUE,
  year = 2020,
  variables_filter = list(
    SPORDER = 1
  ),
  rep_weights = "housing"
)

# creating a survey object!
# with these weights we need to handle our sample
# we can use the survey package and srvyr
# they return a survey class object that calcs standard errors
# when data are tabulated with appropriate functions
# tidycensus includes a function to_surveY()
# to convert ACS microdata to survey or srvyr objects
ms_hh_svy <- ms_hh_replicate %>%
  to_survey(type = "housing",
            design = "rep_weights") %>%
  filter(TEN == 3)

class(ms_hh_svy)

# note we use the filter LAST
# its important to FIRST convert the dataset to a survey object
# and THEN identify the "subpopulation" for the model

# calculating estimates & errors with srvyr
ms_hh_svy %>%
  survey_count(PUMA, HHT_label)

# survey_count()
# returns tabulations for each HH type in PUMA along with SE's

# Below is an adaptation of the rent burden analysis above but
# using the srvyr function survey_mean()
ms_svy_summary <- ms_hh_svy %>%
  mutate(
    race_ethnicity = case_when(
      HISP != "01" ~ "Hispanic",
      HISP == "01" & RAC1P == "1" ~ "White",
      HISP == "01" & RAC1P == "2" ~ "Black",
      TRUE ~ "Other"
    ),
    married = case_when(
      HHT == "1" ~ "Married",
      TRUE ~ "Not married"
    ),
    above_40 = GRPIP >= 40
  ) %>%
  filter(race_ethnicity != "Other") %>%
  group_by(race_ethnicity, married) %>%
  summarize(
    prop_above_40 = survey_mean(above_40)
  )
# same derived estimes but srvyr workflow also gives SE's
# to convert SE's to MOE's we should multiply the SE's by the coefficients
# 90% conf level : 1.645
# 95% conf level : 1.96
# 99% conf level : 2.576

# similar visualization as previous parts of the book
ms_svy_summary_moe <- ms_svy_summary %>%
  mutate(prop_above_40_moe = prop_above_40_se * 1.645,
         label = paste(race_ethnicity, married, sep = ", "))

ggplot(ms_svy_summary_moe, aes(x = prop_above_40,
                               y = reorder(label,
                                           prop_above_40))) +
  geom_errorbar(aes(xmin = prop_above_40 - prop_above_40_moe,
                    xmax = prop_above_40 + prop_above_40_moe)) +
  geom_point(size = 3, color = "navy") +
  labs(title = "Rent burdened-households in Mississippi",
       x = "2016-2020 ACS estimate (from PUMS data)",
       y = "",
       caption = "Rent-burdened defined when gross rent is 40 percent or more\nof household income. Error bars represent a 90 percent confidence level.") +
  scale_x_continuous(labels = scales::percent) +
  theme_grey(base_size = 12)
#-------------------------------------------------------------------------------


# Modeling with PUMS data ------------------------------------------------------
#-------------------------------------------------------------------------------
# statistical methods that use complex survey samples require special methods!

# EX:
# model whether or not an individual in the labor force aged between
# 25 and 49 changed residences in the past year as a function of
# educational attainment, wages, age, class of worker, and family status
# in Rhode Island!
ri_pums_to_model <- get_pums(
  variables = c("PUMA", "SEX", "MIG",
                "AGEP", "SCHL", "WAGP",
                "COW", "ESR", "MAR", "NOC"),
  state = "RI",
  survey = "acs5",
  year = 2020,
  rep_weights = "person"
)
# even though or model focuses on a subset of the population
# variables_filter should not be used here

# Data prep / feature engineering
ri_pums_recoded <- ri_pums_to_model %>%
  mutate(
    emp_type = case_when(
      COW %in% c("1", "2") ~ "private",
      COW %in% c("3", "4", "5") ~ "public",
      TRUE ~ "self"
    ),
    child = case_when(
      NOC > 0 ~ "yes",
      TRUE ~ "no"
    ),
    married = case_when(
      MAR == 1 ~ "yes",
      TRUE ~ "no"
    ),
    college = case_when(
      SCHL %in% as.character(21:24) ~ "yes",
      TRUE ~ "no"
    ),
    sex = case_when(
      SEX == 2 ~ "female",
      TRUE ~ "male"
    ),
    migrated = case_when(
      MIG == 1 ~ 0,
      TRUE ~ 1
    )
  )

# we'll be estimating a logistic regression with a binary outcoe
# whether or not an individual is a migrant
ri_model_svy <- ri_pums_recoded %>%
  to_survey() %>%
  filter(
    ESR == 1,  # civilian employed
    WAGP > 0,  # earned wages last year
    AGEP >= 25,
    AGEP <= 49
  ) %>%
  rename(age = AGEP, wages = WAGP)

# modeling functions in the survey package can be used for modeling data
# in survey design objects, they will take into account the replicate weights,
# survey design, and subpopulation structure
migration_model <- svyglm(
  formula = migrated ~ log(wages) + sex + age + emp_type +
    child + married + college + PUMA,
  design = ri_model_svy,
  family = quasibinomial()
)
summary(migration_model)





















































































