# Load Packages
library(dplyr)
library(readr)
library(tidyr)
library(viridis)
library(GGally)
library(tidyverse)
library(broom)
# Mapping packages
library(sf)
library(ggspatial)


#load Boston sf files
boston = read_sf("data/social_infra/bounds.geojson") %>% 
  filter(name == "boston") %>% 
  summarize(geometry = st_union(geometry))

blocks = read_rds("data/bluebikes/bgdataset.rds") %>% 
  
  #load blue bike stations RDS files
  bikedb = readRDS("data/bluebikes/bgdataset.rds")
stationbg_db = readRDS("data/bluebikes/stationbg_dataset.rds")

bikedb %>%  glimpse()
stationbg_db %>% glimpse()

# Create dataset that only shows bluebike stations within Boston
boston_stations = stationbg_db %>% 
  # Join in boston, but keep only the rows with a valid join (left = FALSE)
  st_join(boston, left = FALSE)

# How many are there?
boston_stations %>% 
  as_tibble() %>% 
  summarise(count = n())


# Plot them
ggplot()+
  geom_sf(data = boston, fill = "lightgreen", alpha = 0.5)+
  geom_sf(data = stationbg_db, size = 4, color = "grey", alpha = 0.5)+
  geom_sf(data = boston_stations, size = 3, color = "navy")+
  theme_bw()+
  # add a scale to help reader measure distance
  annotation_scale(bar_cols=c("lightgreen", "white"))+
  labs(title = "Blue Bike Stations within Boston Neighborhoods",
       x = "Longitude",
       y = "Latitude")

# short table describing the spatial structure
tb = boston_stations %>% 
  summarise(
    num_stations = n(),            # Count of stations (rows)
    total_area = sum(area, na.rm = TRUE), # Sum of area inside Boston
    .groups = "drop"
  ) %>% 
  st_drop_geometry() %>% 
  select(
    num_stations,
    total_area
  )

tb


# Suppose we want to know how many bike stations 
# are in neighborhoods with high income (pop of family earning 60k+k+ > 0.5)

# We want to filter the spatial data of neighborhoods with pop over 0.5 earning income 60k+

# get id and geometry of each station
stations = boston_stations %>% 
  select(geoid, geometry)

# get high income zone
zone = bikedb %>% 
  # add a filter for neighborhoods where 
  # pop earning 60k–100k plus pop earning 100k+ is greater than 0.5
  filter((pop_100000_plus_2019 + pop_60001_100000_2019) > 0.5) %>% 
  # keep the geometry spatial data
  select(geometry) %>%
  summarize(geometry = st_union(geometry))

# give me just the stations in the high income zone
zonestations = stations %>% 
  st_join(zone, left = FALSE)

# How many are there?
zonestations %>% 
  as_tibble() %>% 
  summarize(count = n())

# Plot them
ggplot() +
  geom_sf(data = zone, fill = "chartreuse", alpha = 0.5) +
  geom_sf(data = stations, size = 4, color = "grey", alpha = 0.5) +
  geom_sf(data = zonestations, size = 3, color = "blueviolet") +
  theme_void() +
  # Crop the map
  coord_sf(
    xlim = c(-71.2, -70.95),
    ylim = c(42.24, 42.41)
  ) +
  labs(
    title = "Blue Bike Stations inside high income neighborhoods",
    caption = paste0(
      "High-income neighborhoods are defined as areas where\n",
      "more than half of the population earns over $60,000 per year.\n",
      "Of the 249 stations, 159 (or 64%) are located within these high-income\n",
      "neighborhoods."
    )
  ) +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
  )



# Report the mean outcome variable BEFORE and AFTER applying your spatial filters
# What is the mean pop of the 249 stations' neighborhoods that earn more than 60k?

before = boston_stations %>%
  summarise(
    mean = mean(pop_60001_100000_2019 + pop_100000_plus_2019, na.rm = TRUE)
  )
before

# What is the mean pop of the 159 stations in high-income neighborhoods that earn more than 60k?

after = zonestations %>% 
  st_join(bikedb, left = FALSE) %>% 
  summarise(
    mean = mean(pop_60001_100000_2019 + pop_100000_plus_2019, na.rm = TRUE)
  )
after



# Spatial Aggregation
# Pick a style of spatial aggregation 
# Select a set of larger polygons that encompass/overlap your features

# Equal Area projection
aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Equal Distance projection
aed <- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Get EPSG:4326 (WGS 84) projection
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# I want to see stations overlapping with Boston neighborhoods
# and compute the mean, median, sum of station density
# station density = (# stations inside a neighborhood) / (neighborhood pop density)

# First, get boston_census_data.csv for neighborhood names & pop density
census = read.csv("data/boston_social_infra/boston_census_data.csv")

# Then, get boston_grid.geojson (geometry for each census cell)
grid = read_sf("data/boston_social_infra/boston_grid.geojson")

# Join boston grid to boston census
census_grid = census %>% 
  left_join(grid, by = "cell")

# Convert census_grid into sf object and compute area for each grid cell
census_grid <- st_as_sf(census_grid, sf_column_name = "geometry", crs = 4326)

census_grid = census_grid %>% 
  st_transform(crs = aea) %>%          # equal-area projection
  mutate(area = as.numeric(st_area(.)) / 1e6) %>%  # m^2 → km^2
  st_transform(crs = wgs)

census_grid

# Aggregate polygons for each neighborhood
neighborhood = census_grid %>% 
  group_by(neighborhood) %>% 
  summarise(
    geometry = st_union(geometry),
    pop_density = sum(pop_density * area) / sum(area),
    total_area = sum(area),
    .groups = "drop"
  )

neighborhood

# Recast as sf (even if you think it already is)
neighborhood <- st_as_sf(neighborhood)
st_geometry(neighborhood) <- "geometry"

# View neighborhood polygons
ggplot() +
  geom_sf(data = neighborhood, aes(fill = neighborhood), color = "black") +
  scale_fill_viridis_d(option = "D") +
  theme_void() +
  labs(fill = "Neighborhood")

# Project both datasets to planar CRS
neighborhood_proj <- st_transform(neighborhood, 26986)
stations_proj <- st_transform(stations, 26986)

# Join stations to neighborhoods & count stations per neighborhood
stations_nb = neighborhood_proj %>% 
  st_join(y = stations_proj, join = st_intersects) %>% 
  group_by(neighborhood) %>% 
  summarize(n_stations = n(), .groups = "drop") %>% 
  left_join(neighborhood %>% as_tibble(), by = "neighborhood") %>% 
  mutate(rate = n_stations / ((pop_density * total_area) / 1000))

stations_nb

# Compute mean, median, sum of rate of bike stations per thousand people
stat = stations_nb %>% 
  summarise(
    n_stations = sum(n_stations),
    total_pop = sum(pop_density * total_area),
    total_rate = (sum(n_stations) / sum(pop_density * total_area)) * 1000,
    mean_rate = mean(rate),
    median_rate = median(rate)
  ) %>% 
  st_drop_geometry()

stat

# Rank neighborhoods by highest station-per-population rate
stations_nb %>% 
  arrange(desc(rate)) %>% 
  select(neighborhood, n_stations, rate) %>% 
  st_drop_geometry()


# Compute centroids
stations_centroids <- stations_nb %>%
  st_centroid()

# Create final visualization in ggplot
viz = ggplot() +
  geom_sf(data = neighborhood, aes(fill = neighborhood), alpha = 0.5, color = "white") +
  scale_fill_viridis_d(option = "D") +
  # Plot station-to-people rate as points (size by rate)
  geom_sf(data = stations_centroids, aes(size = rate), color = "navy",
          fill = "cadetblue1", shape = 21, stroke = 0.5) +
  geom_sf_text(data = stations_centroids, aes(label = round(rate, 3)),
               size = 3, color = "black", nudge_y = -0.002) +
  theme_void() +
  # Crop the map
  coord_sf(xlim = c(-71.15, -71.00),
           ylim = c(42.26, 42.38)) +
  labs(
    fill = "Boston Neighborhoods",
    size = "Number of 🚲 Stations Per 1000 Residents",
    title = "Blue Bike Stations Density Across Boston Neighborhoods 🚲",
    subtitle = "Rate indicates stations per 1,000 residents",
    caption = "169 blue bikes station located inside of 10 biggest Boston neighborhoods.
    \nOf the 10 neighborhoods within the Boston boundary,
    \nDowntown has the highest ratio of blue bike stations per 1000 residents."
  ) +
  theme(
    legend.position = "right",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  )

viz

# Recall our key rates we computed
stat

# Rename the columns and round the rates
stat2 = stat %>%
  reframe(
    "Total Stations" = n_stations,
    "Rate of all of Boston" = round(total_rate, 3),
    "Average Rate per Neighborhood" = round(mean_rate, 3),
    "Median Rate per Neighborhood" = round(median_rate, 3)
  )

stat2

# Convert to a grob
library(gridExtra)
table_grob = tableGrob(stat2, rows = NULL)

# Combine plot and table
combined_plot = grid.arrange(viz, table_grob, ncol = 1, heights = c(3, 1))

# Save high-quality image
ggsave("bike_station_map_table.png", plot = combined_plot,
       width = 10, height = 10, dpi = 500)


# Cluster your geometries with k-means

# Since kmeans() is not spatial-aware, we need to drop spatial geometry
data = stations_nb %>%
  st_drop_geometry() %>%
  select(n_stations, rate)

# apply k-means
set.seed(41)  # set seed for same results
k_result = kmeans(data, centers = 3)  # Choose k = 3 or based on analysis

# add clusters
viz2 = stations_nb %>%
  mutate(
    cluster = factor(k_result$cluster)
  )


# Summarize
rate_summary = viz2 %>%
  st_drop_geometry() %>%
  group_by(cluster) %>%
  summarise(
    count = n(),
    mean_rate = mean(rate, na.rm = TRUE),
    sd_rate = sd(rate, na.rm = TRUE),
    min_rate = min(rate, na.rm = TRUE),
    max_rate = max(rate, na.rm = TRUE)
  )

rate_summary


# visualize
ggplot() +
  geom_sf(data = viz2, aes(fill = cluster), color = "white") +
  scale_fill_viridis_d(option = "E") +
  geom_sf_text(data = stations_centroids, aes(label = round(rate, 3)), size = 3, color = "white", nudge_y = -0.002) +
  theme_void() +
  labs(
    title = "Bike Station to Resident Rate in Boston",
    subtitle = "Cluster-method by k-means on rate",
    caption = "Downtown is shown in its own cluster (yellow), with a much higher
    \nstation-to-resident ratio compared to other Boston neighborhoods."
  ) +
  # put legend on the right and captions left justified on the bottom
  theme(
    legend.position = "right",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  )
