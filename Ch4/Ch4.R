# CHAPTER 4
library(tidyverse)
library(tidycensus)
library(scales)
library(ggridges)
library(ggbeeswarm)


# BASIC VISUALS
###############

# median HH inc and age in GA
ga_wide <- get_acs(
  geography = "county",
  state = "Georgia",
  variables = c(medinc = "B19013_001",
                medage = "B01002_001"),
  output = "wide",
  year = 2020
)
options(scipen = 999) # instruction to avoid scientific notation
ggplot(ga_wide, aes(x = medincE)) +
  geom_histogram() # default is 30 bins

ggplot(ga_wide, aes(x = medincE)) +
  geom_histogram(bins = 15)

# other option for univaraite data: box-and-whisker
ggplot(ga_wide, aes(y = medincE)) +
  geom_boxplot()

# multivariate relations with scatter
ggplot(ga_wide, aes(x = medageE, y = medincE)) +
  geom_point()

# relationship?
ggplot(ga_wide, aes(x = medageE, y = medincE)) +
  geom_point() +
  geom_smooth(method = "lm") # smoothed relationships can b visualized with
                             # method = "loess"


# CUSTOMIZING
#############
# percent of commuters that take public transit for largest metros
metros <- get_acs(
  geography = "cbsa",
  variables = "DP03_0021P",
  summary_var = "B01003_001",
  survey = "acs1",
  year = 2019
) %>%
  slice_max(summary_est, n = 20) # retain largest 20 by pop

# basic
ggplot(metros, aes(x = NAME, y = estimate)) +
  geom_col()

# more legible
metros %>%
  mutate(NAME = str_remove(NAME, "-.*$")) %>%
  mutate(NAME = str_remove(NAME, ",.*$")) %>%
  ggplot(aes(y = reorder(NAME, estimate), x = estimate)) +
  geom_col()

# lets make it even better with labels
metros %>%
  mutate(NAME = str_remove(NAME, "-.*$")) %>%
  mutate(NAME = str_remove(NAME, ",.*$")) %>%
  ggplot(aes(y = reorder(NAME, estimate), x = estimate)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Public transit commute share",
       subtitle = "2019 1-year ACS estimates",
       y = "",
       x = "ACS estimate",
       caption = "Source: ACS Data Profile variable DP03_0021P via the tidycensus R package")

# more customization
metros %>%
  mutate(NAME = str_remove(NAME, "-.*$")) %>%
  mutate(NAME = str_remove(NAME, ",.*$")) %>%
  ggplot(aes(y = reorder(NAME, estimate), x = estimate)) +
  geom_col(color = "red", fill = "red",
           alpha = 0.5, width = 0.85) +
  theme_minimal(base_size = 12) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  labs(title = "Public transit commute share",
       subtitle = "2019 1-year ACS estimates",
       y = "",
       x = "ACS estimate",
       caption = "Source: ACS Data Profile variable DP03_0021P via the tidycensus R package") +
  theme(
    plot.title = element_text(face = "bold"), # title bold
    axis.title.x = element_text(face = "bold") # x-axis bold
  )

# exporting our visualization
ggsave("metro_transit.png") # saves last plot to the wd

# more options
ggsave(
  filename = "metro_transit.png",
  path = getwd(),
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)


# VISUAL MOE
############
# median HH inc for counties in Maine
# pops
maine <- get_decennial(
  state = "Maine",
  geography = "county",
  variables = c(totalpop = "P1_001N"),
  year = 2020
) %>%
  arrange(desc(value))

# hh income
maine_income <- get_acs(
  state = "Maine",
  geography = "county",
  variables = c(hhincome = "B19013_001"),
  year = 2020
) %>%
  mutate(NAME = str_remove(NAME, " County, Maine"))

# plot
ggplot(maine_income, aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_point(size = 3, color = "darkgreen") +
  labs(title = "Median household income",
       subtitle = "Counties in Maine",
       x = "",
       y = "ACS estimate") +
  theme_minimal(base_size = 12.5) +
  scale_x_continuous(labels = label_dollar()) # but do the MOEs matter?

# error bars for MOEs
maine_income %>%
  arrange(desc(moe))

# lets visualize this uncertainty
ggplot(maine_income, aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(size = 3, color = "darkgreen") +
  theme_minimal(base_size = 12.5) +
  labs(title = "Median household income",
       subtitle = "Counties in Maine",
       x = "2016-2020 ACS estimate",
       y = "") +
  scale_x_continuous(labels = label_dollar())

# SAME BUT WITH NEVADA
# EXAMPLE / EXERCISE
nevada <- get_decennial(
  state = "Nevada", geography = "county",
  variables = c(totalpop = "P1_001N"), year = 2020
) %>%
  arrange(desc(value))

# NV income
nevada_income <- get_acs(
  state = "Nevada", geography = "county",
  variables = c(hhincome = "B19013_001"), year = 2020
) %>%
  mutate(NAME = str_remove(NAME, " County, Nevada"))

# PLOT
ggplot(nevada_income, aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(size = 3, color = "darkgreen") +
  theme_minimal(base_size = 12.5) +
  labs(title = "Median household income",
       subtitle = "Counties in Nevada",
       x = "2016-2020 ACS estimate",
       y = "") +
  scale_x_continuous(labels = label_dollar())
ggsave(
  filename = "nv_median_inc.png",
  path = getwd(),
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)

  
# VISUAL TIME SERIES
####################

# 1 year ACS data from 2005 to 2019 on median home value
# in Deschutes County, OR (Home of Bend the City)
years <- 2005:2019
names(years) <- years

deschutes_value <- map_dfr(years, ~{
  get_acs(
    geography = "county",
    variables = "B25077_001",
    state = "OR",
    county = "Deschutes",
    year = .x,
    survey = "acs1"
  )
}, .id = "year")

# basic visual
ggplot(deschutes_value, aes(x = year, y = estimate, group = 1)) + # group =1 lets ggplot understand how to connect dots
  geom_line() +                                                   # since only one county is visualized
  geom_point()

# better visual
ggplot(deschutes_value, aes(x = year, y = estimate, group = 1)) +
  geom_ribbon(aes(ymax = estimate + moe, ymin = estimate - moe),
              fill = "navy",
              alpha = 0.4) +
  geom_line(color = "navy") +
  geom_point(color = "navy", size = 2) +
  theme_minimal(base_size = 12) +
  scale_y_continuous(labels = label_dollar(scale = .001, suffix = "k")) +
  labs(title = "Median home value in Deschutes County, OR",
       x = "Year",
       y = "ACS estimate",
       caption = "Shaded area represents margin of error around the ACS estimate")

# SAME WITH WASHOE!!!
years <- 2005:2019
names(years) <- years

washoe_val <- map_dfr(years, ~{
  get_acs(
    geography = "county", variables = "B25077_001",
    state = "NV", county = "Washoe", year = .x, survey = "acs1"
  )
}, .id = "year")

# visual
ggplot(washoe_val, aes(x = year, y = estimate, group = 1)) +
  geom_ribbon(aes(ymax = estimate + moe, ymin = estimate - moe),
              fill = "red",
              alpha = 0.4) +
  geom_line(color = "red") +
  geom_point(color = "red", size = 2) +
  theme_minimal(base_size = 12) +
  scale_y_continuous(labels = label_dollar(scale = .001, suffix = "k")) +
  labs(title = "Median home value in Washoe County, NV",
       x = "Year", y = "ACS estimate",
       caption = "Shaded area represents margin of error around the ACS estimate")
ggsave(
  filename = "washoe_homeval_median.png",
  path = getwd(),
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)


# POP PYRAMIDS
# ############

# pop estimates API for utah
utah <- get_estimates(
  geography = "state",
  state = "UT",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  year = 2019
)  # note we dont need some rows, like both sexes or all ages

utah_filtered <- filter(utah, str_detect(AGEGROUP, "^Age"),
                        SEX != "Both sexes") %>%
  mutate(value = ifelse(SEX == "Male", -value, value))

# pyramid
ggplot(utah_filtered, aes(x = value, y = AGEGROUP, fill = SEX)) +
  geom_col()

# cleaner / final pyramid
utah_pyramid <- ggplot(utah_filtered,
                       aes(x = value,
                           y = AGEGROUP,
                           fill = SEX)) +
  geom_col(width = 0.95, alpha = 0.75) +
  theme_minimal(base_size = 12) +
  scale_x_continuous(
    labels = ~ number_format(scale = .001, suffix = "k")(abs(.x)),
    limits = 140000 * c(-1, 1)
  ) +
  scale_y_discrete(labels = ~ str_remove_all(.x, "Age\\s|\\syears")) +
  scale_fill_manual(values = c("darkred", "navy")) +
  labs(x = "",
       y = "2019 Census Bureau population estimate",
       title = "Population structure in Utah",
       fill = "",
       caption = "Data source: US Census Bureau population estimates & tidycensus R package")

# NEVADA EQUIV.
nevada <- get_estimates(
  geography = "state", state = "NV", product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"), breakdown_labels = TRUE, year = 2019
)
# filter
nevada_filtered <- filter(nevada, str_detect(AGEGROUP, "^Age"),
                          SEX != "Both sexes") %>%
  mutate(value = ifelse(SEX == "Male", -value, value))
# pyramid
nv_pyramid <- ggplot(nevada_filtered, aes(x = value, y = AGEGROUP, fill = SEX))+
  geom_col(width = 0.95, alpha = 0.75) +
  theme_minimal(base_size = 12) +
  scale_x_continuous(
    labels = ~ number_format(scale = .001, suffix = "k")(abs(.x)),
    limits = 140000 * c(-1, 1)
  ) +
  scale_y_discrete(labels = ~ str_remove_all(.x, "Age\\s|\\syears")) +
  scale_fill_manual(values = c("darkred", "navy")) +
  labs(x = "",
       y = "2019 Census Bureau population estimate",
       title = "Population structure in Nevada",
       fill = "",
       caption = "Data source: US Census Bureau population estimates & tidycensus R package")
ggsave(
  filename = "nv_pop_pyramid.png",
  path = getwd(),
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)


# GROUP-WISE COMPARISONS
########################
# median home vals by census tract for counties in oregon
or_counties <- c("Multnomah", "Clackamas", "Washington",
                 "Yamhil", "Marion", "Columbia")
housing_val <- get_acs(
  geography = "tract",
  variables = "B25077_001",
  state = "OR",
  county = or_counties,
  year = 2020
)

# sep geographies into 3 cols
housing_val2 <- separate(
  housing_val, NAME,
  into = c("tract", "county", "state"), sep = ", "
)

# get county desc. stats.
housing_val2 %>%
  group_by(county) %>%
  summarize(min = min(estimate, na.rm = TRUE),
            mean = mean(estimate, na.rm = TRUE),
            median = median(estimate, na.rm = TRUE),
            max = max(estimate, na.rm = TRUE))

# kernel density plot shows overall shape of distr. of median home vals
ggplot(housing_val2, aes(x = estimate)) +
  geom_density()

# mapping the county col into fill will draw superimposed density plots
ggplot(housing_val2, aes(x = estimate, fill = county)) +
  geom_density(alpha = 0.3)

# alternatively, facet_wrap() fcn will give side-by-side graphics
ggplot(housing_val2, aes(x = estimate)) +
  geom_density(fill = "darkgreen", color = "darkgreen", alpha = 0.5, linewidth = 1.25) +
  facet_wrap(~county) +
  scale_x_continuous(labels = dollar_format(scale = 0.000001,
                                            suffix = "m")) +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 45)) +
  labs(x = "ACS estimate",
       y = "",
       title = "Median home values by Census tract, 2015-2019 ACS")

# NEVADA EQUIV!!!!
nevada_counties <- c(
  "Clark", "Washoe", "Lyon", "Carson", "Elko", "Nye", "Douglas"
)
haus_val_nv <- get_acs(
  geography = "tract",
  variables = "B25077_001",
  state = "NV",
  county = nevada_counties,
  year = 2020
)
haus_val2 <- separate(
  haus_val_nv, NAME,
  into = c("tract", "county", "state"),
  sep = ", "
)
ggplot(haus_val2, aes(x = estimate)) +
  geom_density(fill = "red", color = "red",
               alpha = 0.5, linewidth = 1.25) +
  facet_wrap(~county) +
  scale_x_continuous(labels = dollar_format(scale = 0.000001,
                                            suffix = "m")) +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 45)) +
  labs(x = "ACS estimate",
       y = "",
       title = "Median home values by Census tract, 2015-2019 ACS")
ggsave(
  filename = "nv_home_vals.png",
  path = getwd(),
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)


# ADVANCED VISUALS
##################

# get county medians
county_meds <- haus_val2 %>%
  group_by(county) %>%
  summarize(median = median(estimate))

# ridgeline plot for NV data
ggplot(haus_val2, aes(x = estimate, y = county, fill = county)) +
  geom_density_ridges() +
  theme_ridges() +
  labs(x = "Median home value: 2016-2020 ACS estimate",
       y = "") +
  scale_x_continuous(labels = label_dollar(scale = .000001, suffix = "m"),
                     breaks = c(0, 500000, 1000000)) +
  theme(axis.text.x = element_text(angle = 45))

ggsave(
  filename = "nv_home_vals_ridge.png",
  path = getwd(),
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)

# ggbeeswarm package
race_vars = c(White = "B03002_003", 
              Black = "B03002_004", 
              Hispanic = "B03002_012")
nv_race_income <- get_acs(
  geography = "tract",
  state = "NV",
  variables = race_vars,
  summary_var = "B19013_001",
  year = 2020
) %>%
  group_by(GEOID) %>%
  filter(estimate == max(estimate, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(estimate != 0)

# plot
ggplot(nv_race_income, aes(x = variable, y = summary_est, color = summary_est)) +
  geom_quasirandom(alpha = 0.5, size=4) +
  coord_flip() +
  theme_minimal(base_size = 13) +
  scale_color_viridis_c(guide = "none") +
  scale_y_continuous(labels = label_dollar()) +
  labs(x = "Largest group in Census tract",
       y = "Median household income",
       title = "Household income distribution by largest racial/ethnic group",
       subtitle = "Census tracts, Nevada",
       caption = "Data source: 2016-2020 ACS")
ggsave(
  filename = "nv_race_ineq_beeswarm.png",
  path = getwd(),
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)

###############################
###############################
###############################
# Remaining stuff is geofaceted
# and plotly (which i know from python)
# so not that interesting
###############################
###############################
###############################




