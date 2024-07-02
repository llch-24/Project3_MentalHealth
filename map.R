library(sf)
library(janitor)
library(tidyverse)
library(viridis)

theme_set(theme_minimal())

# LOAD AND CLEAN DATA --------------------------------
df <- read_csv("analyticdata2024 - clean_data (1).csv")
df <- df |> 
  janitor::clean_names()

df <- df[grepl(" County", df$name), ] # take out states, keep only counties

names(df) <- gsub("_raw_value$", "", names(df)) # take out "_raw_value"

counties <- st_read("continental_us_counties_2022.geojson")
counties <- clean_names(counties) # load geojson

# filter data to include only continental us
continental_us_fips <- c("01", "04", "05", "06", "08", "09", "10", "11", "12", "13", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56") #fips code for all continental states

counties_continental <- counties[counties$statefp %in% continental_us_fips, ]

# fixing fips code format
counties_continental$statefp <- as.numeric(counties_continental$statefp) 
counties_continental$x5_digit_fips_code <- paste0(counties_continental$statefp, counties_continental$countyfp)

counties_continental$x5_digit_fips_code <- as.numeric(counties_continental$x5_digit_fips_code)

df$x5_digit_fips_code <- as.numeric(df$x5_digit_fips_code)

# join data frames
joined_counties_continental <- left_join(counties_continental, df, by = "x5_digit_fips_code")

joined_counties_continental <- st_simplify(joined_counties_continental, preserveTopology = TRUE, dTolerance = 0.01) #make geometry simpler


#PLOT MAP ----------------------------------------------------
num_breaks <- 10  # adjust num breaks

# colors using viridis
colors <- rev(viridis_pal(option = "plasma")(num_breaks))

#  map 
joined_counties_continental |> 
  ggplot(aes(fill = poor_mental_health_days)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_gradientn(colors = colors, na.value = "grey90", n.breaks = num_breaks) +
  theme_minimal() +
  labs(title = "Choropleth Map of Poor Mental Health Days", fill = "Poor Mental Health Days") +
  theme(legend.position = "right")

# Plot States -----------------------------------------------

# North Dakota
joined_counties_continental |> 
  filter(statefp == "38") |> 
  ggplot(aes(fill = poor_mental_health_days)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_gradientn(colors = colors, na.value = "grey90", n.breaks = num_breaks) +
  theme_minimal() +
  labs(title = "Choropleth Map of Poor Mental Health Days", fill = "Poor Mental Health Days") +
  theme(legend.position = "right")

# Arkansas
joined_counties_continental |> 
  filter(statefp == "5") |> 
  ggplot(aes(fill = poor_mental_health_days)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_gradientn(colors = colors, na.value = "grey90", n.breaks = num_breaks) +
  theme_minimal() +
  labs(title = "Choropleth Map of Poor Mental Health Days", fill = "Poor Mental Health Days") +
  theme(legend.position = "right")

# California
joined_counties_continental |> 
  filter(statefp == "6") |> 
  ggplot(aes(fill = poor_mental_health_days)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_gradientn(colors = colors, na.value = "grey90", n.breaks = num_breaks) +
  theme_minimal() +
  labs(title = "Choropleth Map of Poor Mental Health Days", fill = "Poor Mental Health Days") +
  theme(legend.position = "right")


