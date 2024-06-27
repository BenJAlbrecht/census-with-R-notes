# ch8 exercises
library(tidyverse)
library(tidycensus)
library(segregation)
library(tigris)
library(sf)
library(scales)
library(patchwork)
library(units)
library(corrr)
library(spdep)
library(units)


#=============================================#
#---------------------------------------------#
#-# geo-deomographic classification in reno #-#
#---------------------------------------------#
#=============================================#


# Reno CBSA / urban # ------------------------------------------
#---------------------------------------------------------------
reno <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "Reno")) %>%
  st_transform(8528)

reno_urb <- urbans %>%
  filter(str_detect(NAMELSAD10, "Reno, NV")) %>%
  st_transform(8528)
#---------------------------------------------------------------


# Get data from Census API -------------------------------------
#---------------------------------------------------------------
variables_to_get <- c(
  median_value = "B25077_001",
  median_rooms = "B25018_001",
  median_income = "DP03_0062",
  total_population = "B01003_001",
  median_age = "B01002_001",
  pct_college = "DP02_0068P",
  pct_foreign_born = "DP02_0094P",
  pct_white = "DP05_0077P",
  median_year_built = "B25037_001",
  percent_ooh = "DP04_0046P"
)

reno_data <- get_acs(
  geography = "tract",
  variables = variables_to_get,
  state = "NV",
  year = 2020,
  output = "wide",
  geometry = TRUE
) %>%
  st_transform(8528) %>%
  st_filter(reno, .predicate = st_within) %>%
  separate(NAME, c("tract", "county", "state"), ", ") %>%
  mutate(tract = as.numeric(gsub("Census Tract ", "", tract))) %>%
  select(-county, -state) %>%
  filter(!(tract %in% c(35.01, 9402, 9901))) %>%
  na.omit()
#---------------------------------------------------------------


# Feature engineering ------------------------------------------
#---------------------------------------------------------------
reno_data_for_model <- reno_data %>%
  mutate(pop_density = as.numeric(set_units(total_populationE / st_area(.), "1/km2")),
         median_structure_age = 2018 - median_year_builtE) %>%
  select(!ends_with("M")) %>%
  rename_with(.fn = ~str_remove(.x, "E$")) %>%
  na.omit()

reno_estimates <- reno_data_for_model %>%
  select(-GEOID, -median_value, -median_year_built, -tract) %>%
  st_drop_geometry()
#---------------------------------------------------------------


# Dimension reduction w/ PCA -----------------------------------
#---------------------------------------------------------------
pca <- prcomp(
  formula = ~.,
  data = reno_estimates,
  scale. = TRUE,
  center = TRUE
)

summary(pca) # up to PC5 captures 90% of variance

components <- predict(pca, reno_estimates)

reno_pca <- reno_data_for_model %>%
  select(GEOID, median_value) %>%
  cbind(components)
#---------------------------------------------------------------


# Kmeans -------------------------------------------------------
#---------------------------------------------------------------
set.seed(1983)
reno_kmeans <- reno_pca %>%
  st_drop_geometry() %>%
  select(PC1:PC5) %>%
  kmeans(centers = 5)

table(reno_kmeans$cluster)

reno_clusters <- reno_pca %>%
  mutate(cluster = as.character(reno_kmeans$cluster))
#---------------------------------------------------------------


# Kmeans plot --------------------------------------------------
#---------------------------------------------------------------
ggplot(reno_clusters, aes(fill = cluster)) +
  geom_sf(size = 0.1) +
  scale_fill_brewer(palette = "Set1") +
  theme_void() +
  labs(fill = "Cluster ")
#---------------------------------------------------------------


# Spatial clustering -------------------------------------------
#---------------------------------------------------------------
input_vars <- reno_pca %>%
  select(PC1:PC5) %>%
  st_drop_geometry() %>%
  as.data.frame()

skater_nbrs <- poly2nb(reno_pca, queen = TRUE)
costs <- nbcosts(skater_nbrs, input_vars)
skater_weights <- nb2listw(skater_nbrs, costs, style = "B")

mst <- mstree(skater_weights)
regions <- skater(
  mst[,1:2],
  input_vars,
  ncuts = 4,
  crit = 8
)
#---------------------------------------------------------------


# Plot ---------------------------------------------------------
#---------------------------------------------------------------
reno_clusters$region <- as.character(regions$group)

ggplot(reno_clusters, aes(fill = region)) +
  geom_sf(size = 0.1) +
  geom_sf(data = reno_urb, fill = NA, color = "red") +
  theme_void() +
  labs(
    title = "Reno geospatial clusters"
  )
#---------------------------------------------------------------




