# CH 8
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


# indices of segregation & diversity
####################################

# looking at california!
ca_acs_data <- get_acs(
  geography = "tract",
  variables = c(
    white = "B03002_003",
    black = "B03002_004",
    asian = "B03002_006",
    hispanic = "B03002_012"
  ),
  state = "CA",
  geometry = TRUE,
  year = 2019
)

# Urbanized areas by population with geometry
# then filter for those with a pop of 750k or more
us_urban_areas <- get_acs(
  geography = "urban area",
  variables = "B01001_001",
  geometry = TRUE,
  year = 2019,
  survey = "acs1"
) %>%
  filter(estimate >= 750000) %>%
  transmute(urban_name = str_remove(NAME,
                                    fixed(", CA Urbanized Area (2010)")))

# Inner spatial join between ca tracts and the urbanized areas
# giving tracts in the largest california urban areas with
# the urban_name column appended
ca_urban_data <- ca_acs_data %>%
  st_join(us_urban_areas, left = FALSE) %>%
  select(-NAME) %>%
  st_drop_geometry()

# The dissimilarity index!
ca_urban_data %>%
  filter(variable %in% c("white", "hispanic"),
         urban_name == "San Francisco--Oakland") %>%
  dissimilarity(
    group = "variable",
    unit = "GEOID",
    weight = "estimate"
  )
# but its better to compare city to city
# 0 = perfect integration
# 1 = complete segregation
ca_urban_data %>%
  filter(variable %in% c("white", "hispanic")) %>%
  group_by(urban_name) %>%
  group_modify(~
    dissimilarity(.x,
      group = "variable",
      unit = "GEOID",
      weight = "estimate"
    )
  ) %>%
  arrange(desc(est))
# but this only does segregation between two groups

# Nevada ! #
nv_acs_data <- get_acs(
  geography = "tract",
  variables = c(
    white = "B03002_003",
    hispanic = "B03002_012"
  ),
  state = "NV",
  geometry = TRUE,
  year = 2019
)

us_urban_areas <- get_acs(
  geography = "urban area",
  variables = "B01001_001",
  geometry = TRUE,
  year = 2019,
  survey = "acs1"
) %>%
  transmute(urban_name = str_remove(NAME,
                                    fixed(", NV Urbanized Area (2010)")))
nv_urban_data <- nv_acs_data %>%
  st_join(us_urban_areas, left = FALSE) %>%
  select(-NAME) %>%
  st_drop_geometry()

nv_urban_data %>%
  group_by(urban_name) %>%
  group_modify(~
    dissimilarity(.x,
      group = "variable",
      unit = "GEOID",
      weight = "estimate"
    )
  ) %>%
  arrange(desc(est))

# multi-group segregation indicces
# but the dissimilarity index only considers two groups!
# theres two indices the segregation package uses:
# 1) Mutual Information Index M
# 2) Theil's Entropy Index H 
mutual_within(
  data = ca_urban_data,
  group = "variable",
  unit = "GEOID",
  weight = "estimate",
  within = "urban_name",
  wide = TRUE
) %>%
  arrange(desc(M))
# we can also do local segregation analysis
# by decomposing M into unit-level segregation scores
# represeted by ls.
# in this example, we'll do this with the most segregated urban area: LA
la_local_seg <- ca_urban_data %>%
  filter(urban_name == "Los Angeles--Long Beach--Anaheim") %>%
  mutual_local(
    group = "variable",
    unit = "GEOID",
    weight = "estimate",
    wide = TRUE
  )
# the results can be mapped by joining the data to a dataset of census tracts
# from tigris
la_tracts_seg <- tracts("CA", cb = TRUE, year = 2019) %>%
  inner_join(la_local_seg, by = "GEOID")

la_tracts_seg %>%
  ggplot(aes(fill = ls)) +
  geom_sf(color = NA) +
  coord_sf(crs = 26946) +
  scale_fill_viridis_c(option = "inferno") +
  theme_void() +
  labs(fill = "Local\nsegregation index")

# the diversity gradient

# requires mapbox :()


# Regression with US Census Data
################################
# topic: median home value by census tract in DFW

dfw_counties <- c("Collin County", "Dallas", "Denton",
                  "Ellis", "Hunt", "Kaufman", "Rockwall",
                  "Johnson", "Parker", "Tarrant", "Wise")
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

dfw_data <- get_acs(
  geography = "tract",
  variables = variables_to_get,
  state = "TX",
  county = dfw_counties,
  geometry = TRUE,
  output = "wide",
  year = 2020
) %>%
  select(-NAME) %>%
  st_transform(32138)

# Visualizing the outcome variable
mhv_map <- ggplot(dfw_data, aes(fill = median_valueE)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(labels = scales::label_dollar()) +
  theme_void() +
  labs(fill = "Median home value")

mhv_histogram <- ggplot(dfw_data, aes(x = median_valueE)) +
  geom_histogram(alpha = 0.5, fill = "navy", color = "navy", bins = 100) +
  theme_minimal() +
  labs(x = "Median home value")

mhv_map + mhv_histogram


# the hist is right-skewed
# we may violate normality for model residuals, so lets log-transform?

mhv_map_log <- ggplot(dfw_data, aes(fill = log(median_valueE))) +
  geom_sf(color = NA) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = "Median home \nvvalue (log)")

mhv_histogram_log <- ggplot(dfw_data, aes(x = log(median_valueE))) +
  geom_histogram(alpha = 0.5, fill = "navy", color = "navy", bins = 100) +
  theme_minimal() +
  scale_x_continuous() +
  labs(x = "Median home value (log)")

mhv_map_log + mhv_histogram_log


# the log transform lets us better see variation while preserving that expensive
# area. we also see our hist is normal
# a common term for prepping data is "feature engineering" which refers to 
# transforming the predictors to better represent the relationship between
# predictors and the outcome variable

# most of our features are engineered already, theyre %'s but some could be
# further designed

# lets make 2 new vars:
# 1) pop_density == # of people in each tract per square kilometer
# 2) median_structure_age == median age of housing structures in the tract
dfw_data_for_model <- dfw_data %>%
  mutate(pop_density = as.numeric(set_units(total_populationE / st_area(.), "1/km2")),
         median_structure_age = 2018 - median_year_builtE) %>%
  select(!ends_with("M")) %>%
  rename_with(.fn = ~str_remove(.x, "E$")) %>%
  na.omit()

# regression!
formula <- "log(median_value) ~ median_rooms + median_income + pct_college + pct_foreign_born + pct_white + median_age + median_structure_age + percent_ooh + pop_density + total_population"

model1 <- lm(formula = formula, data = dfw_data_for_model)

summary(model1)

dfw_estimates <- dfw_data_for_model %>%
  select(-GEOID, -median_value, -median_year_built) %>%
  st_drop_geometry()
correlations <- correlate(dfw_estimates, method = "pearson")

rplot(correlations)
network_plot(correlations) # yea most predictors are correlated


# collinearity can be diagnosed further by calculating the variance inflation factor
# (VIF) for the model. this takes into account not only pairwise corr, but the extent
# to which predictors are collinear with all other predictors
# VIF == 1 indicates no collinearity; VIF abiove 5 suggest collinearity that
# has problematic influence on model interpretation
library(car)
vif(model1)
# median_income is the most problematic with a VIF over 6 so lets try dropping it
formula2 <- "log(median_value) ~ median_rooms + pct_college + pct_foreign_born + pct_white + median_age + median_structure_age + percent_ooh + pop_density + total_population"
model2 <- lm(formula = formula2, data = dfw_data_for_model)
summary(model2)
# now lets also check the VIF
vif(model2)

# dimension reduction with PCA
# an alternative approach with collinearity is dimension reduction
# we can transform predictors into a series of dimensions
# that represent the variance of the predictors but are uncorrelated
pca <- prcomp(
  formula = ~.,
  data = dfw_estimates,
  scale. = TRUE,
  center = TRUE
)
summary(pca)

# to understand what the different PC mean, we should plot a variable
# that repr. the relationships between the original vars in the model
# and the derived components
pca_tibble <- pca$rotation %>%
  as_tibble(rownames = "predictor")
# positive values mean that the original variable is positively loaded
# onto a given component, and negative values mean that the variable
# is negatively loaded.

# large values in either direction are most interesting
# a value near 0 mean the variable is not meaningfully explained by a component
# lets visualize further
pca_tibble %>%
  select(predictor:PC5) %>%
  pivot_longer(PC1:PC5, names_to = "component", values_to = "value") %>%
  ggplot(aes(x = value, y = predictor)) +
  geom_col(fill = "darkgreen", color = "darkgreen", alpha = 0.5) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL, x = "Value")

# with respect to PCA1, which explains nearly 41% of the variance in the
# overall predictor set
# some vars load negatively and only 2 positively
# we can attach these principal components to our original data with predict()
# and cbind() then make a map of PC1 for further exploration!
components <- predict(pca, dfw_estimates)

dfw_pca <- dfw_data_for_model %>%
  select(GEOID, median_value) %>%
  cbind(components)

ggplot(dfw_pca, aes(fill = PC1)) +
  geom_sf(color = NA) +
  theme_void() +
  scale_fill_viridis_c()

# the visual helps us understand how the multiple variables represent
# latent social processes at play in DFW
# the yllow areas which have higher vals for PC1 are located in communities
# like east fort worth, east arlington, grand prairie and south dallas

# generally speaking, these are low-to-middle income areas with large non-white pops
# locations with low PC1 are segregated, dominantly non-hisp white and are some
# of the wealthiest in the US
# in turn PC1 captures the gradient that represents these social differences,
# with which multiple demographic characteristics will be associated


# these principal components can be used for principal components regression
# in which derived components are used as predictors

# generally, components should be chosen that account for at least 90% of
# the original variance.
# though this is discretionary!
# lets fit a model to the first 6 PC's

pca_formula <- paste0("log(median_value) ~ ",
                      paste0("PC", 1:6, collapse = " + "))

pca_model <- lm(formula = pca_formula, data = dfw_pca)
summary(pca_model)



# Spatial regression
####################
# a core assumption of the linear model is that the errors
# are independent of one another and normally distributed
# log transforming the right-skewed outcome variable
# was intended to solve the latter;
# lets check this by adding the residuals for model2 to our data
# and drawing a hist

dfw_data_for_model$residuals <- residuals(model2)

ggplot(dfw_data_for_model, aes(x = residuals)) +
  geom_histogram(bins = 100, alpha = 0.5, color = "navy",
                 fill = "navy") +
  theme_minimal()

# the former assumption of independent residuals is commonly violated
# in models that use spatial data
# this is because spatial processes commonly are characterized
# by spatial autocorrelation in the error term
# so the model's performance itself depends on geographic location
# we can assess this with Moran's I
wts <- dfw_data_for_model %>%
  poly2nb() %>%
  nb2listw()

moran.test(dfw_data_for_model$residuals, wts) # modest and positive, but statistically significant

dfw_data_for_model$lagged_residuals <- lag.listw(wts, dfw_data_for_model$residuals)

ggplot(dfw_data_for_model, aes(x = residuals, y = lagged_residuals)) +
  theme_minimal() +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red")

# the plot illustrates the positive spatial autocorrelation in the residuals
# so the assumption of independence in the model error term is likely violated
# to resolve this issue, we can turn to spatial regression methods

# SPATIAL ECONOMETRICS ! ! !
# two main models:
#                 1) spatial lag models
#                 2) spatial error models

# spatial lag models
# account for spatial dependence by including a spatial lag of the outcome
# variable in the model. in doing so, it accounts for spatial spillover effects
# which is the possibility that values in neighboring areas have an influence
# on values in a given location

# since a spatially lagged outcome on the RHS violates exogeneity assumptions
# we need a special method for estimating the spatial lag model
library(spatialreg)

lag_model <- lagsarlm(
  formula = formula2,
  data = dfw_data_for_model,
  listw = wts
)

summary(lag_model, Nagelkerke = TRUE)


# spatial error models
# in contrast, spatial error models include a spatial lag in model's error term
# this is designed to capture latent spatial processes that arent being accounted
# for in the model estimation and can show up in the residuals

error_model <- errorsarlm(
  formula = formula2,
  data = dfw_data_for_model,
  listw = wts
)

summary(error_model, Nagelkerke = TRUE)

# Choosing between spatial lag and spatial error models
# the methods are dependent on our problem
# for ex, if spatial spillover effects are related to the hypothesis
# ex: values on focal home values, a spatial lag model may be preferred
# other: if the autocorr factors that influence the outcome are hard to measure:
# ex: disc, racial bias (unobserved) then a spatial error model may be preferred

# we can also look at the moran's I over the residuals to see if the spatial
# model has resolved our problems

# spatial lag model
moran.test(lag_model$residuals, wts)

# spatial error model
moran.test(error_model$residuals, wts)

# spatial error does a better job
# we can use a lagrange multiplier test to identify the best model
lm.LMtests(
  model2,
  wts,
  test = c("LMerr", "LMlag", "RLMerr", "RLMlag")
)



# geographically weighted regression
####################################
# the previous models got us --global-- estimates
# this lends to conclusions like
# in the DFW area, higher levels of educ attainment are associated
# with higher median home values
# but metro areas like DFW are diverse and multifaceted
# its possible that relations between predictors & outcomes that is observed
# for the entire region may vary neighborhood to neighborhood

# this type of phenomenon is called spatial non-stationarity
# and we can explore it with geographically weighted regression or GWR

# GWR is a technique designed to evaluate local variations in the results
# of regression models given a kernel (distance-decay) weighting function

# GWR relies on th econcept of a "kernel bandwidth" to compute the local regr model
library(GWmodel)
library(sf)

dfw_data_sp <- dfw_data_for_model %>%
  as_Spatial()

bw <- bw.gwr(
  formula = formula2,
  data = dfw_data_sp,
  kernel = "bisquare",
  adaptive = TRUE
)


formula2 <- "log(median_value) ~ median_rooms + pct_college + pct_foreign_born + pct_white + median_age + median_structure_age + percent_ooh + pop_density + total_population"

gw_model <- gwr.basic(
  formula = formula2,
  data = dfw_data_sp,
  bw = bw,
  kernel = "bisquare",
  adaptive = TRUE
)

gw_model_results <- gw_model$SDF %>%
  st_as_sf()

ggplot(gw_model_results, aes(fill = Local_R2)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c() +
  theme_void()

ggplot(gw_model_results, aes(fill = percent_ooh)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = "Local β for \npercent_ooh")

ggplot(gw_model_results, aes(fill = pop_density)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = "Local β for \npopulation density")


# classification and clustering of ACS data
###########################################
# Geodemographic classification
# refers to the grouping of obs based on similar characteristics
set.seed(1983)

dfw_kmeans <- dfw_pca %>%
  st_drop_geometry() %>%
  select(PC1:PC8) %>%
  kmeans(centers = 6)

table(dfw_kmeans$cluster)

dfw_clusters <- dfw_pca %>%
  mutate(cluster = as.character(dfw_kmeans$cluster))

ggplot(dfw_clusters, aes(fill = cluster)) +
  geom_sf(size = 0.1) +
  scale_fill_brewer(palette = "Set1") +
  theme_void() +
  labs(fill = "Cluster ")

# scatter of clusters
library(plotly)

cluster_plot <- ggplot(dfw_clusters,
                       aes(x = PC1, y = PC2, color = cluster)) +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

ggplotly(cluster_plot) %>%
  layout(legend = list(orientation = "h", y = -0.15,
                       x = 0.2, title = "Cluster"))

# note that this approach was aspatial, insofar as it did not take the
# geographic properties of the tracts into account
# but maybe we want contiguous areas / neighborhoods

input_vars <- dfw_pca %>%
  select(PC1:PC8) %>%
  st_drop_geometry() %>%
  as.data.frame() 

skater_nbrs <- poly2nb(dfw_pca, queen = TRUE)
costs <- nbcosts(skater_nbrs, input_vars)
skater_weights <- nb2listw(skater_nbrs, costs, style = "B")

mst <- mstree(skater_weights)

regions <- skater(
  mst[,1:2], 
  input_vars, 
  ncuts = 7,
  crit = 10
)

dfw_clusters$region <- as.character(regions$group)

ggplot(dfw_clusters, aes(fill = region)) +
  geom_sf(size = 0.1) +
  scale_fill_brewer(palette = "Set1") +
  theme_void()









































