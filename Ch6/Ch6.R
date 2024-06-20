# Chapter 6
library(tidycensus)
library(tidyverse)
library(tigris)
library(mapview)

library(ggiraph)
library(scales)
library(patchwork)

options(tigris_use_cache = TRUE)

# GEOMETRY IN TIDYCENSUS
########################
sf_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "CA",
  county = "San Francisco",
  year = 2020,
  geometry = TRUE
)

# Clean it up
bad_tracts <- c("Census Tract 9804.01", "Census Tract 179.03")
sf_income <- sf_income %>%
  separate(NAME, into = c("tract", "county", "state"),
           sep =", ",
           remove = FALSE) %>%
  filter(!(tract %in% bad_tracts))

# plot
ggplot(data = sf_income, aes(fill = estimate)) +
  geom_sf() +
  scale_fill_distiller(palette = "YlOrRd",
                       direction = 1,
                       labels = scales::comma_format(scale = 1e-3, suffix = "k")) +
  labs(title = "San Francisco, Median Income by Tract, 2020",
       caption = "Data source: 2016-2020 5-year ACS, US Census Bureau",
       fill = "ACS estimate") +
  theme_void()


# Map-MAKING W GGPLOT AND GEOMSF
################################
# choropleth mapping
# this is what we did above, i jumped the gun (as usual)
us_median_age <- get_acs(
  geography = "state",
  variables = "B01002_001",
  year = 2019,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
) %>%
  shift_geometry()   # tigris fcn toshift and rescale areas
plot(us_median_age$geometry)

# basic map of median age by state
ggplot(data = us_median_age, aes(fill = estimate)) +
  geom_sf()

# more customs
ggplot(data = us_median_age, aes(fill = estimate)) +
  geom_sf() +
  scale_fill_distiller(palette = "RdPu",
                       direction = 1) +
  labs(title = " Median Age by State, 2019",
       caption = "Data source: 2019 1-year ACS, US Census Bureau",
       fill = "ACS estimate") +
  theme_void()


# TMAP
######
# race & ethnicity data from 2020 decennial census
# hennepin county, minnesota
hennepin_race <- get_decennial(
  geography = "tract",
  state = "MN",
  county = "Hennepin",
  variables = c(
    Hispanic = "P2_002N",
    White = "P2_005N",
    Black = "P2_006N",
    Native = "P2_007N",
    Asian = "P2_008N"
  ),
  summary_var = "P2_001N",
  year = 2020,
  geometry = TRUE
) %>%
  mutate(percent = 100 * (value / summary_value))

library(tmap)
hennepin_black <- filter(hennepin_race,
                         variable == "Black")
tm_shape(hennepin_black) +
  tm_polygons()

# choropleth map
tm_shape(hennepin_black) +
  tm_polygons(col = "percent")

# washoe!
washoe_race <- get_decennial(
  geography = "tract",
  state = "NV",
  county = "Washoe",
  variables = c(
    Hispanic = "P2_002N",
    White = "P2_005N",
    Black = "P2_006N",
    Native = "P2_007N",
    Asian = "P2_008N"
  ),
  summary_var = "P2_001N",
  year = 2020,
  geometry = TRUE
) %>%
  mutate(percent = 100 * (value / summary_value))

washoe_black <- filter(washoe_race,
                       variable == "Black")
tm_shape(washoe_black) +
  tm_polygons(col = "percent")
hist(washoe_black$percent)

# notice tmpa uses a classed scheme rather than continuous by ggplot2
# so we identify classes, which we can see based on the distribution of data
# hence the histogram analysis

# lets make it continuous like ggplot2
tm_shape(hennepin_black) +
  tm_polygons(col = "percent",
              style = "quantile",
              n = 5,
              palette = "Purples",
              title = "2020 US Census") +
  tm_layout(title = "Percent Black\nby Census tract",
            frame = FALSE,
            legend.outside = TRUE)
# we can now see some more neighborhood level differences in suburban areas, but
# we do miss some information since theres values between 21% and 88%
# a compromise in GIS cartography applications is the Jenks natural-breaks method
# this uses an algorithm to identify meaningful breaks in data for bin boundaries
tm_shape(hennepin_black) +
  tm_polygons(col = "percent",
              style = "jenks",
              n = 5,
              palette = "Purples",
              title = "2020 US Census",
              legend.hist = TRUE) +
  tm_layout(title = "Percent Black\nby Census tract",
            frame = FALSE,
            legend.outside = TRUE,
            bg.color = "grey70",
            legend.hist.width = 5)

# maybe we want more reference elements to add context
# like a basemap, arrow, scale bar -- all can be done with tmap!
# we'll use mapboxapi but we need an account and access token!

# none of mapbox was working!!!

# SF
sf_race <- get_decennial(
  geography = "tract",
  state = "CA",
  county = "San Francisco",
  variables = c(
    Hispanic = "P2_002N",
    White = "P2_005N",
    Black = "P2_006N",
    Native = "P2_007N",
    Asian = "P2_008N"
    ),
    summary_var = "P2_001N",
    year = 2020,
    geometry = TRUE
  ) %>%
    mutate(percent = 100 * (value / summary_value))
  
bad_tracts <- c(9804.01, 179.03, 9901, 9902)
sf_race <- sf_race %>%
  separate(NAME, into = c("tract", "county", "state"),
           sep = ", ") %>%
  mutate(tract = str_replace(tract, "Census Tract ", "")) %>%
  mutate(tract = as.numeric(tract)) %>%
  filter(!(tract %in% bad_tracts))

filter(sf_race, tract %in% c(9803, 9809))


sf_asian <- filter(sf_race,
                   variable == 'Asian')

# Asian % fig, SF
tm_shape(sf_asian) +
  tm_polygons(col = "percent",
              style = "jenks",
              n = 5,
              palette = "Purples",
              title = "2020 US Census",
              legend.hist = TRUE) +
  tm_layout(title = "Percent Asian\nby Census Tract",
            frame = FALSE,
            legend.outside = TRUE,
            bg.color = "grey70",
            legend.hist.width = 5)

# color palettes!

# graduated symbol map
# we use shapes references to geographic units
tm_shape(hennepin_black) +
  tm_polygons() +
  tm_bubbles(size = "value", alpha = 0.5,
             col = "navy", title.size = "Non-Hispanic Black - 2020 US Census") +
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "bottom")

# sf example
tm_shape(sf_asian) +
  tm_polygons() +
  tm_bubbles(size = "value", alpha = 0.5,
             col = "navy", title.size = "Non-Hispanic Black - 2020 US Census") +
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "bottom")

# Faceted maps
# Maybe a cartographer wants to visualize groups comparatively
# SF
sf_race <- sf_race %>%
  filter(!(variable == "Native"))

sf_facet <- tm_shape(sf_race) +
  tm_facets(by = "variable", scale.factor = 4) +
  tm_fill(
    col = "percent",
    style = "jenks",
    n = 5,
    palette = "Oranges",
    title = "Percent (2020 US Census)"
  ) +
  tm_layout(bg.color = "grey",
            legend.position = c(0.2, 0.75),
            panel.label.bg.color = "white")

# Dot-density map
# Scatter dots within areal units relative to the size of a data attribute
# Use as_dot_density() in tidycensus to get Census data ready for visualization
sf_dots <- sf_race %>%
  as_dot_density(
    value = "value",
    values_per_dot = 100,
    group = "variable"
  )

background_tracts <- filter(sf_race, variable == "White")

tm_shape(background_tracts) +
  tm_polygons(col = "white",
              border.col = "grey") +
  tm_shape(sf_dots) +
  tm_dots(col = "variable",
          palette = "Set1",
          size = 0.1,
          title = "1 dot = 100 people") +
  tm_layout(legend.outside = TRUE,
            title = "Race/ethnicity,\n2020 US Census")


# CARTOGRAPHY W NON-CENSUS DATA
###############################
# Visualizing presidential election data
cpr2024 <- tibble(
  state_po = state.abb
)

# Assign ratings
solid_D <- c('CA', 'CO', 'CT', 'DE', 'HI', 'IL', 'ME', 'MD', 'MA', 'NJ', 'NM',
             'NY', 'OR', 'RI', 'VT', 'VA', 'WA')
likely_D <- c('MN', 'NH')
toss_up <- c('AZ', 'GA', 'MI', 'NV', 'PA', 'WI')
lean_R <- c('NC')
likely_R <- c('FL', 'TX')
solid_R <- c('AL', 'AK', 'AR', 'ID', 'IN', 'IA', 'KS', 'KY', 'LA', 'MS', 'MO',
             'MT', 'NE', 'ND', 'OH', 'OK', 'SC', 'SD', 'TN', 'UT', 'WV', 'WY')
cpr2024 <- cpr2024 %>%
  mutate(cpr_rate = case_when(
    state_po %in% solid_D ~ "Solid D",
    state_po %in% likely_D ~ "Likely D",
    state_po %in% toss_up ~ "Toss Up",
    state_po %in% lean_R ~ "Lean R",
    state_po %in% likely_R ~ "Likely R",
    state_po %in% solid_R ~ "Solid R"
  ))

# Plotting data
us_states <- states(cb = TRUE, resolution = "20m") %>%
  filter(!(NAME %in% c("Puerto Rico", "District of Columbia"))) %>%
  shift_geometry()

us_states_joined <- us_states %>%
  left_join(cpr2024, by = c("STUSPS" = "state_po")) %>%
  mutate(cpr_rate = factor(cpr_rate, levels = c("Solid D", "Likely D", "Lean D",
                                                "Toss Up", "Lean R", "Likely R",
                                                "Solid R")))
cook_colors <- c(
  "Solid D" = "#0000FF",
  "Likely D" = "#3366FF",
  "Lean D" = "#6699FF",
  "Toss Up" = "#CCCCCC",
  "Lean R" = "#FF9999",
  "Likely R" = "#FF6666",
  "Solid R" = "#FF0000"
)

# Plot using ggplot2
ggplot(us_states_joined, aes(fill = cpr_rate)) +
  geom_sf(color = "white", lwd = 0.2) +
  scale_fill_manual(values = cook_colors, na.value = "white",
                    breaks = names(cook_colors)) +
  theme_void() +
  labs(fill = "CPR Rating",
       title = "Cook Political Report ratings 2024",
       caption = "Note: Nebraska and Maine split electoral college votes by congressional district")

# working with ZCTAs
# the most granular geography at which many agencies release data is at the zip code level
# not an ideal geo for visuals, given that zip codes represent collections of US postal routes
# (or even sometimes a single building or PO box), so theyre not entirely coherent geographies
# but the US Census Bureau allows for approx. of zip code mapping with zip code tabulation areas (ZCTAs)
# theyre shapes built from census blocks in which the most common zip in each block allocates the blocks

# example is the IRS service's statistics of income (SOI) data
irs_data <- read_csv("https://www.irs.gov/pub/irs-soi/18zpallnoagi.csv")
glimpse(irs_data)


self_employment <- irs_data %>%
  select(ZIPCODE, self_emp = N09400, total = N1)

reno_zctas <- zctas(
  cb = TRUE,
  starts_with = c("89"),
  year = 2018
)
reno_zctas <- reno_zctas %>%
  slice(c(170, 128, 131, 88, 137, 119, 169, 132))

reno_se_data <- reno_zctas %>%
  left_join(self_employment, by = c("GEOID10" = "ZIPCODE")) %>%
  mutate(pct_self_emp = 100 * (self_emp / total)) %>%
  select(GEOID10, self_emp, pct_self_emp)

tm_shape(reno_se_data) +
  tm_borders(col = "black") +  # Add borders around the ZCTAs in black color
  tm_fill(col = "pct_self_emp",
          palette = "Purples",
          title = "Reno, NV\n% self-employed") 

# Maybe we want to see where most self-emp income filings are rather than their relative share
tm_shape(reno_se_data) +
  tm_polygons() +
  tm_bubbles(size = "self_emp",
             alpha = 0.5,
             col = "navy",
             title.size = "Reno, NV\nSelf-employed filers")


# INTERACTIVE MAPPING
#####################
# So far we have focused on *static maps* where the output is fixed after
# rendering the map. 

# percent of pop 25 and up with a bach degree or higher
# 2016-2020 acs
dallas_bachelors <- get_acs(
  geography = "tract",
  variables = "DP02_0068P",
  year = 2020,
  state = "TX",
  county = "Dallas",
  geometry = TRUE
)

# interactive choropleth map
library(mapview)
mapview(dallas_bachelors, zcol = "estimate")

# conversion of tmap maps to interactive leaflet is also stragithforward
# after we enter this command ,all maps in our session will be interactive leaflet
tmap_mode("view")

tm_shape(dallas_bachelors) +
  tm_fill(col = "estimate", palette = "magma",
          alpha = 0.5)

# switch back to static plots:
tmap_mode("plot")

# for more fine-grained control, we reproduce the examples using the leaflet package
# native syntax
library(leaflet)
pal <- colorNumeric(
  palette = "magma",
  domain = dallas_bachelors$estimate
)
pal(c(10, 20, 30, 40, 50))

leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addPolygons(data = dallas_bachelors,
              color = ~pal(estimate),
              weight = 0.5,
              smoothFactor = 0.2,
              fillOpacity = 0.5,
              label = ~estimate) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = dallas_bachelors$estimate,
    title = "% with bachelor's<br/>degree"
  ) # lol doesnt work for some reason


# ADVANCED EXAMPLES
###################

# looking at migration flows into Travis County Texas, home of Austin
travis_inflow <- get_flows(
  geography = "county",
  state = "TX",
  county = "Travis",
  geometry = TRUE
) %>%
  filter(variable == "MOVEDIN") %>%
  na.omit() %>%       # drops migration from outside US, as these areas dont have GEOIDs
  arrange(desc(estimate))


# NOOOOOOOOOOOOO IT NEEDS MAPBOX!!!

# linking maps & charts
# we can take advantage of non geo visuals and geo visuals

vt_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "VT",
  year = 2020,
  geometry = TRUE
) %>%
  mutate(NAME = str_remove(NAME, " County, Vermont"))

vt_map <- ggplot(vt_income, aes(fill = estimate)) + 
  geom_sf_interactive(aes(data_id = GEOID)) + 
  scale_fill_distiller(palette = "Greens",
                       direction = 1, 
                       guide = "none") + 
  theme_void()

vt_plot <- ggplot(vt_income, aes(x = estimate, y = reorder(NAME, estimate), 
                                 fill = estimate)) +
  geom_errorbar(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point_interactive(color = "black", size = 4, shape = 21,
                         aes(data_id = GEOID)) +
  scale_fill_distiller(palette = "Greens", direction = 1,
                       labels = label_dollar()) + 
  scale_x_continuous(labels = label_dollar()) + 
  labs(title = "Household income by county in Vermont",
       subtitle = "2016-2020 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)",
       fill = "ACS estimate") + 
  theme_minimal(base_size = 14)

girafe(ggobj = vt_map + vt_plot, width_svg = 10, height_svg = 5) %>%
  girafe_options(opts_hover(css = "fill:cyan;"))


# same in NV
nv_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "NV",
  year = 2020,
  geometry = TRUE
) %>%
  mutate(NAME = str_remove(NAME, " County, Nevada")) %>%
  mutate(NAME = str_remove(NAME, " City, Nevada"))

nv_map <- ggplot(nv_income, aes(fill = estimate)) + 
  geom_sf_interactive(aes(data_id = GEOID)) + 
  scale_fill_distiller(palette = "Greens",
                       direction = 1, 
                       guide = "none") + 
  theme_void()

nv_plot <- ggplot(nv_income, aes(x = estimate, y = reorder(NAME, estimate), 
                                 fill = estimate)) +
  geom_errorbar(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point_interactive(color = "black", size = 4, shape = 21,
                         aes(data_id = GEOID)) +
  scale_fill_distiller(palette = "Greens", direction = 1,
                       labels = label_dollar()) + 
  scale_x_continuous(labels = label_dollar()) + 
  labs(title = "Household income by county in Nevada",
       subtitle = "2016-2020 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)",
       fill = "ACS estimate") + 
  theme_minimal(base_size = 14)

girafe(ggobj = nv_map + nv_plot, width_svg = 10, height_svg = 5) %>%
  girafe_options(opts_hover(css = "fill:cyan;"))




