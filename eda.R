library(tidyverse)
library(reshape)
library(ggcorrplot)
library(patchwork)

theme_set(theme_minimal())

# LOAD AND CLEAN DATA --------------------------------
df <- read_csv("analyticdata2024 - clean_data (1).csv")
df <- df |> 
  janitor::clean_names()

df <- df[grepl(" County", df$name), ] # take out states, keep only counties

names(df) <- gsub("_raw_value$", "", names(df)) # take out "_raw_value"

df <- df |> 
  drop_na(poor_mental_health_days)

# EDA ------------------------------------------------------

# first, visualize the response variable
df |> 
ggplot(aes(poor_mental_health_days)) + 
  geom_histogram(fill="hotpink") #normal distribution, assume linearity

# then visualize response and predictor
df |> 
  ggplot(aes(x=poor_mental_health_days,
             y=ratio_of_population_to_mental_health_providers)) + 
  geom_point(fill = "hotpink") #not well distributed! no relationship here

# now find cor
cor(df$poor_mental_health_days, df$ratio_of_population_to_mental_health_providers, use="complete.obs")

# Correlation ------------------------------------------------------------------
possible_predictors <- df |> 
  select(frequent_mental_distress, food_insecurity, adult_smoking , physical_inactivity , adult_obesity , excessive_drinking , high_school_completion , broadband_access , poor_mental_health_days , ratio_of_population_to_mental_health_providers) #select the ones we want, using correlations from Nikhil's code

full_cor <- possible_predictors |>
  drop_na() |> 
  cor() |> 
  ggcorrplot()

cor_matrix <- cor(possible_predictors)
cor_matrix_vertical <- t(cor_matrix["poor_mental_health_days", ])
response_cor <- ggcorrplot(cor_matrix_vertical, lab = TRUE, title = "Correlation with Poor Mental Health Days") + theme(axis.text.x = element_blank())

full_cor + response_cor

# Exploring LM Variables -------------------------------------------------------------------
# LM Cor
lm_predictors <- df |>  # first, select predictors
  select(frequent_mental_distress,
         adult_smoking,
         physical_inactivity,
         adult_obesity,
         ratio_of_population_to_mental_health_providers,
         poor_mental_health_days)

lm_predictors |> #correlation matrix
  drop_na() |> 
  cor() |> 
  ggcorrplot(lab=T, type = "lower")

#Variable Scatterplots
plot <- function(y_var) {
  d_f <- lm_predictors
  x_var <- "poor_mental_health_days"
  
  ggplot(d_f, aes_string(x = x_var, y = y_var)) +
    geom_point(alpha=0.5) +
    labs(title = paste("Scatter Plot of", x_var, "vs", y_var))
}


mental_distress <- plot("frequent_mental_distress")
smoking <- plot("adult_smoking")
inactivity <- plot("physical_inactivity")
obesity <- plot("adult_obesity")

mental_distress + smoking + inactivity + obesity # thought: add correlation matrix to this plot
