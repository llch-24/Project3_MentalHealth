library(tidyverse)
df <- read_csv("analyticdata2024 - clean_data.csv")
df <- df |> 
  janitor::clean_names()

df_numeric <- df |> 
  select(where(is.numeric))

corr <- cor(df_numeric)
corr <- melt(corr, na.rm=T)

corr_unique <- corr |> 
  distinct(value, .keep_all = T) |> 
  filter( (value > 0.7 & value < 1) | value < -0.7)

corr_unique

df_numeric_predictors <- df |> 
  select(poor_physical_health_days_raw_value, physical_inactivity_raw_value, high_school_completion_raw_value, food_insecurity_raw_value, poor_mental_health_days_raw_value, adult_smoking_raw_value, frequent_mental_distress_raw_value, population_raw_value, percent_not_proficient_in_english_raw_value, adult_obesity_raw_value, severe_housing_problems_raw_value, percent_hispanic_raw_value, percentage_of_households_with_high_housing_costs, poor_or_fair_health_raw_value) 


df_numeric_predictors <- df_numeric_predictors |>
  dplyr::rename(
    poor_physical_health_days = poor_physical_health_days_raw_value,
    physical_inactivity = physical_inactivity_raw_value,
    high_school_completion = high_school_completion_raw_value,
    food_insecurity = food_insecurity_raw_value,
    poor_mental_health_days = poor_mental_health_days_raw_value,
    adult_smoking = adult_smoking_raw_value,
    frequent_mental_distress = frequent_mental_distress_raw_value,
    population = population_raw_value,
    percent_no_english_proficiency = percent_not_proficient_in_english_raw_value,
    adult_obesity = adult_obesity_raw_value,
    severe_housing_problems = severe_housing_problems_raw_value,
    percent_hispanic = percent_hispanic_raw_value,
    percent_high_housing_costs = percentage_of_households_with_high_housing_costs,
    poor_fair_health = poor_or_fair_health_raw_value
  )


df_numeric_predictors |> 
  cor() |> 
  ggcorrplot()


