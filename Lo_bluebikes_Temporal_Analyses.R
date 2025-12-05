## Packages ####################

library(dplyr)
library(readr)
library(ggplot2)
library(RSQLite)
library(stringr)
library(lubridate)
library(tidyr)

## Loading bluebikes ##############################

# Use SQL translator to load the bluebikes data from sqlite
db <- dbConnect(RSQLite::SQLite(), "data/bluebikes/bluebikes.sqlite")

# Query our data from tally_rush_edge ###################

edges = db %>% 
  tbl("tally_rush_edges") %>% 
  # zoom into year 2021
  filter(str_sub(day, 1,4) == "2021") %>%
  # Collect
  collect() %>%
  as_tibble()

# check how many rows
edges %>%
  summarize(count = n())

# Disconnect database to save RAM
DBI::dbDisconnect(db)

# Get stations data ################
stations = read_rds("data/bluebikes/stationbg_dataset.rds") %>%
  # Let's get the block group population of families earning over 100k a year,
  # using our spatially smoothed estimate, 'pop_black_2020_smooth5'
  select(code, pop_100000_plus_2019) %>%
  # And let's classify it as 
  # above 50% or below 50%
  mutate(maj_high_income = if_else(pop_100000_plus_2019 > 0.5, "yes", "no")) %>%
  # convert to tibble, 
  as_tibble() %>%
  select(code, maj_high_income)

stations %>% head()
stations

# Join the station data to the edges data ###############
data = edges %>% 
  # Join whether this station is located in high income neighborhood
  left_join(by = c("start_code" = "code"), 
            y = stations %>% select(code, start_high_income = maj_high_income)) %>% 
  # We can join in the destination traits,
  # Eg. whether that station is in a majority Black neighborhood
  left_join(by = c("end_code" = "code"), 
            y = stations %>% select(code, end_high_income = maj_high_income))


# View
data

# some data is NA
# drop NA

data2 = data %>% 
  drop_na()


data2


# Calculate confidence interval (lower ci and upper ci) ###########################
viz = data2 %>%
  # make sure day is in date format
  mutate(day = ymd(day)) %>%
  group_by(day, start_high_income) %>%
  summarise(
    mean_rides = mean(count),
    se = sd(count) / sqrt(n()),
    lower_ci = mean_rides - 1.96 * se,
    upper_ci = mean_rides + 1.96 * se,
    .groups = "drop"
  )


# Visualize ###################

viz %>% 
  ggplot(aes(x = day, y = mean_rides, color = start_high_income)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = start_high_income), 
              alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  #geom_point() +
  labs(
    title = "Mean Bike Ride Counts with Confidence Intervals",
    x = "Year",
    y = "Mean Number of Rides",
    color = "High Income?",
    fill = "High Income?"
  ) +
  theme_bw()


## Analyzing riding trends ###############################


# Has ridership increase over the years for riders biking from high income to high income neighborhood?
# Has ridership increase over the years for riders biking from high income to low income neighborhood?
# Has ridership increase over the years for riders biking from low income to high income neighborhood?
# Has ridership increase over the years for riders biking from low income to low income neighborhood?


viz2 = data2 %>% 
  mutate(day = ymd(day)) %>%  
  group_by(day, start_high_income) %>% 
  summarise(total_rides = sum(count), .groups = "drop")

viz2 %>% 
ggplot(aes(x = day, y = total_rides, color = start_high_income)) +
  geom_smooth(linewidth = 1.2) +
  # geom_point() +
  labs(
    title = "Bike Ride Counts Over 2021 by Income Groups",
    x = "Day",
    y = "Number of Rides",
    color = "High Income?"
  ) +
  theme_bw()


viz %>% 
  filter(start_high_income == "no")


#min max average of cross-sections per time step
viz_summary = data2 %>% 
  mutate(day = ymd(day)) %>%  
  group_by(day, start_high_income) %>% 
  summarise(
    min = min(count),
    max = max(count),
    average = mean(count),
    .groups = "drop"
  )

viz_summary

# calulate share of missing values in data

missing_share_per_day = data %>%
  mutate(day = ymd(day)) %>%
  group_by(day) %>%
  summarise(
    missing_share = mean(is.na(start_high_income) | is.na(end_high_income))
  )

# calculate min and max share of missing values
minmax_stat = missing_share_per_day %>% 
  summarise(
    min = min(missing_share),
    max = max(missing_share)
  )
minmax_stat


# Share of missing values per cross-section
#calculate share of missing data in start_high_income and end_high_income
missing_share_per_cs = data %>%
  summarise(
    missing_start = mean(is.na(start_high_income)),
    missing_end   = mean(is.na(end_high_income))
  )
missing_share_per_cs


#min max  share of missing values among cross-sections 

minmax_cs = missing_share_per_cs %>%
  summarise(
    min_start = min(missing_start),
    max_start = max(missing_start),
    min_end = min(missing_end),
    max_end = max(missing_end)
  )

minmax_cs


#  Calculate missing share per cross-section
missing_share_per_cs = data %>%
  group_by(start_high_income, end_high_income) %>%
  summarise(
    missing_share = mean(is.na(start_high_income) | is.na(end_high_income)),
    .groups = "drop"
  )

missing_share_per_cs

# Get min and max of missing share among cross-sections
minmax_cs = missing_share_per_cs %>%
  summarise(
    min_missing_share = min(missing_share),
    max_missing_share = max(missing_share)
  )

minmax_cs

# check if data is balanced by counting how many cross sections it has on each day
# and taking a look at the min, max and average

# Count number of unique cross-sections per day
cs_counts = data %>%
  mutate(day = ymd(day)) %>%
  group_by(day) %>%
  summarise(
    n_cross_sections = n_distinct(paste(start_high_income, end_high_income)),
    .groups = "drop"
  )

# Report min, max, and average
cs_stats = cs_counts %>%
  summarise(
    min_css = min(n_cross_sections),
    max_cs = max(n_cross_sections),
    avg_cs = mean(n_cross_sections)
  )

cs_stats


# Visualize distribution of outcome variable
# group the dataset by day and high income starting station (yes/no)
viz2 = data2 %>% 
  mutate(day = ymd(day)) %>%  
  group_by(day, start_high_income) %>% 
  summarise(total_rides = sum(count), .groups = "drop")

# Visualize distribution using density plot
viz2 %>% 
  ggplot(aes(x = total_rides, color = start_high_income, fill = start_high_income)) +
  geom_density(alpha = 0.3) +
  labs(
    title = "Distribution of Ride Counts by Income Group",
    x = "Total Rides per Day",
    y = "Density",
    color = "High Income?",
    fill = "High Income?"
  ) +
  theme_minimal()

# Calculate the mean, sd, min, median, and max and interquartile range for low income distribution
viz2 %>% 
  filter(start_high_income == "no") %>% 
  summarise(
    mean = mean(total_rides),
    sd = sd(total_rides),
    min = min(total_rides),
    median = median(total_rides),
    max = max(total_rides),
    q1 = quantile(total_rides, 0.25),
    q2 = quantile(total_rides, 0.5),
    q3 = quantile(total_rides, 0.75))


# Calculate the mean, sd, min, median, and max and interquartile range for high income distribution
viz2 %>% 
  filter(start_high_income == "yes") %>% 
  summarise(
    mean = mean(total_rides),
    sd = sd(total_rides),
    min = min(total_rides),
    median = median(total_rides),
    max = max(total_rides),
    q1 = quantile(total_rides, 0.25),
    q2 = quantile(total_rides, 0.5),
    q3 = quantile(total_rides, 0.75))


# find out which cross-sections exceed threshold
threshold_stat = viz2 %>% 
  mutate(over_threshold = total_rides>2000)

# Calculate share of cross-sections exceeding threshold per day
threshold = threshold_stat %>% 
  group_by(day) %>% 
  summarise(
    total_cross_sections = n(),
    cross_sections_over = sum(over_threshold),
    share_over_threshold = cross_sections_over / total_cross_sections,
    .groups = "drop"
  )

# Find day with lowest share exceeding threshold

threshold %>%
  filter(share_over_threshold > 0) %>% 
  arrange(share_over_threshold) %>%
  slice(1)


# Find day with highest share exceeding threshold
threshold %>%
  filter(share_over_threshold > 0) %>% 
  arrange(desc(share_over_threshold)) %>%
  slice_head(n=10)

# calculate the mean, sd per time-step 

viz2 %>%
  group_by(day) %>%
  reframe(
    mean_total_rides = mean(total_rides),
    sd_total_rides = sd(total_rides),
    iqr_total_rides = IQR(total_rides) # inter-quartile range
  ) %>% 
  print (n=261)


# Calculate confidence interval (lower ci and upper ci) ###########################
viz3 = data2 %>%
  # make sure day is in date format
  mutate(day = ymd(day)) %>%
  group_by(day, start_high_income) %>%
  summarise(
    total_rides = sum(count),
    mean_rides = mean(count),
    se = sd(count) / sqrt(n()),
    # calculate the lower CI and upper CI for 95% Confidence interval
    lower_ci = mean_rides - 1.96 * se,
    upper_ci = mean_rides + 1.96 * se,
    .groups = "drop"
  ) 


# Visualize ###################

viz3 %>% 
  ggplot(aes(x = day, y = mean_rides, color = start_high_income)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = start_high_income), 
              alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  #geom_point() +
  labs(
    title = "Average Bike Ride Counts Per Station Per Day in 2021",
    x = "Year 2021",
    y = "Mean Number of Rides",
    caption = "Shaded areas represent 95% confidence intervals. Data grouped by high- vs. low-income neighborhoods.
    \nAverage rides rise in summer and decline afterward. Daily averages are low (2–3), with tight confidence intervals."
    )+
  scale_color_manual(
    name = "Neighborhoods",  # legend title
    values = c("no" = "pink", "yes" = "lightgreen"),
    labels = c("no" = "Low-Income", "yes" = "High-Income")
  ) +
  scale_fill_manual(
    name = "Neighborhoods",
    values = c("no" = "pink", "yes" = "lightgreen"),
    labels = c("no" = "Low-Income", "yes" = "High-Income")
  )+
  theme_light()+
theme(
    legend.position = "bottom",
    plot.caption.position = "plot",  # Ensures caption follows alignment settings
    plot.caption = element_text(hjust = 0)  # 0 = left, 0.5 = center, 1 = right
  )


# Run Regression Model
m1 = lm(total_rides ~ start_high_income, data = viz2)

#view it
summary(m1)
confint(m1)



# Estimate  the efftect of time on the outcome variable of interest using many linear models
# Run another linear model with end_high_income
# Create a viz4 to include more variables
viz4 = data2 %>% 
  mutate(day = ymd(day)) %>%  
  group_by(day, start_high_income, end_high_income, rush) %>% 
  summarise(total_rides = sum(count), .groups = "drop")
viz4


# Run a model using end high income
m2 = lm(total_rides ~ end_high_income, data = viz4)

#view it
summary(m2)
confint(m2)

# Run a model using start high income, end high income and rush (am/pm)



m3 = lm(total_rides ~ start_high_income + end_high_income + rush, data = viz4)

#view it
summary(m3)
confint(m3)


library (broom)
# compare predictive accuracy of each model by R squre statistics and sigma statistics
glance(m1)
glance(m2)
glance(m3)


# Run monte carlo simulation
# Make a data frame for predictor data

# stat = tibble(
#   date = "2021-01-01",
#   start_high_income = c("yes", "no"),
#   end_high_income = c("yes", "no"),
#   rush = c("am", "pm")
# ) %>% 
#   mutate(
#     yhat = predict(m3, newdata = .))

# Run monte carlo simulation
# Make a data frame for predictor data
stat = tibble(
  date = sample(seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "day"), size = 1000, replace = TRUE),
  start_high_income = sample(c("yes", "no"), size = 1000, replace = TRUE),
  end_high_income = sample(c("yes", "no"), size = 1000, replace = TRUE),
  rush = sample(c("am", "pm"), size = 1000, replace = TRUE)
)%>%
    mutate(
      yhat = predict(m3, newdata = .))
  
# View it
stat

# get error
stat = stat %>% 
  # add sigma, the residual standard error, in units of the outcome - number of rides per station per day
  mutate(sigma = broom::glance(m3)$sigma)

# simulate with 1000 samples
sims = stat %>%
  mutate(ysim = rnorm(n=1000, mean = yhat, sd = sigma))

# Simulate outcomes per row, generating 1000 samples per row

sims


# Estimate mean effect among groups for each simulation, totaling 1,000 mean effects.

effects = sims %>%
  group_by(date, start_high_income) %>%
  summarise(mean_ysim = mean(ysim), .groups = "drop") %>%
  # reshape data so we have start_yes and start_ no column for "yes" and "no" in start_high_income 
  pivot_wider(
    names_from = start_high_income,
    values_from = mean_ysim,
    names_prefix = "start_"
  ) %>%
  mutate(
    diff = start_no - start_yes,
    percent_change = (start_no - start_yes) / start_yes,
    z = scale(start_no - start_yes)[, 1]
  )

# View it
effects

# Calculate confidence interval (lower ci and upper ci) for simulated data ###########################
sim_stat <- sims %>%
  group_by(date, start_high_income) %>%
  summarise(
    mean_rides = mean(ysim),
    se = sd(ysim) / sqrt(n()),
    lower_ci = mean_rides - 1.96 * se,
    upper_ci = mean_rides + 1.96 * se,
    .groups = "drop"
  ) %>% 
  drop_na(mean_rides, lower_ci, upper_ci)

# View it

sim_stat


# Test simulated data with a linear model
m4 = lm(mean_rides ~ start_high_income + se, data = sim_stat)

# View it
glance(m4)
