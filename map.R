library(sf)
library(ggplot2)
library(janitor)
library(tidyverse)
library(viridis)

counties <- st_read("continental_us_counties_2022.geojson")
counties <- clean_names(counties)

# filter data to include only continental us
continental_us_fips <- c("01", "04", "05", "06", "08", "09", "10", "11", "12", "13", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56") #fips code for all continental states

counties_continental <- counties[counties$statefp %in% continental_us_fips, ] # filter out


# plot map (preliminary, no data)
ggplot(data = counties_continental) +
  geom_sf(color = "black", lwd = 0.2) +
  theme_minimal() +
  labs(title = "Continental US Counties")

# in case we want to remove "county" from the names
df_nocounty <- as_tibble(df$name <- gsub(" County", "", df$name))

# fixing fips code format
counties_continental$statefp <- as.numeric(counties_continental$statefp)
counties_continental$x5_digit_fips_code <- paste0(counties_continental$statefp, counties_continental$countyfp)

counties_continental$x5_digit_fips_code <- as.numeric(counties_continental$x5_digit_fips_code)

df$x5_digit_fips_code <- as.numeric(df$x5_digit_fips_code)

# joining dataframes
joined_counties_continental <- left_join(counties_continental, df, by = "x5_digit_fips_code")

# Check the joined dataframe
head(joined_counties_continental)

# remove "_raw_value"
names(joined_counties_continental) <- gsub("_raw_value$", "", names(joined_counties_continental))

joined_counties_continental <- st_simplify(joined_counties_continental, preserveTopology = TRUE, dTolerance = 0.01)


#PLOT MAP ----------------------------------------------------
num_breaks <- 10  # Adjust the number of breaks as needed

# Define colors using a sequential color palette (e.g., from `viridis` package)
colors <- rev(viridis_pal(option = "plasma")(num_breaks))

# plot map again
joined_counties_continental |> 
  ggplot(aes(fill = poor_mental_health_days)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_gradientn(colors = colors, na.value = "grey90", n.breaks = num_breaks) +
  theme_minimal() +
  labs(title = "Choropleth Map of Poor Mental Health Days", fill = "Poor Mental Health Days") +
  theme(legend.position = "right")


