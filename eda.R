library(tidyverse)
library(reshape)
<<<<<<< HEAD
library(ggcorrplot)
library(patchwork)
=======
  >>>>>>> 663b75b781225f48e8bd9f065b1bf86293d81b0a
theme_set(theme_minimal())
df <- read_csv("analyticdata2024 - clean_data.csv")
df <- df |> 
  janitor::clean_names()

<<<<<<< HEAD
df <- df[grepl(" County", df$name), ]

=======
  >>>>>>> 663b75b781225f48e8bd9f065b1bf86293d81b0a
df_numeric_plus_county <- df |> 
  select(x5_digit_fips_code, state_abbreviation, name, where(is.numeric)) |> 
  drop_na(ratio_of_population_to_mental_health_providers)

<<<<<<< HEAD
names(df_numeric_plus_county) <- gsub("_raw_value$", "", names(df_numeric_plus_county))

=======
  >>>>>>> 663b75b781225f48e8bd9f065b1bf86293d81b0a
# EDA --------------------------------------------------------------------------

# first, the response variable
df_numeric_plus_county |> 
  <<<<<<< HEAD
ggplot(aes(poor_mental_health_days)) + 
  geom_histogram(fill="hotpink") #visualize og

df_numeric <- df |> 
  select(where(is.numeric)) |> 
  drop_na()
=======
  ggplot(aes(ratio_of_population_to_mental_health_providers)) + 
  geom_histogram() #visualize og

# hmm looks skewed right, let's transform
df_numeric_plus_county <- df_numeric_plus_county |> 
  filter(ratio_of_population_to_mental_health_providers > 0) |> #some values were less than zero?? 
  mutate(log_response = log(ratio_of_population_to_mental_health_providers)) 

# lets visualize again
df_numeric_plus_county |> 
  ggplot(aes(log_response)) + 
  geom_histogram() #much better, but does it have a linear relationship?

possible_predictors <- df_numeric_plus_county |> 
  select(x5_digit_fips_code, state_abbreviation, name, poor_or_fair_health_raw_value, poor_physical_health_days_raw_value, physical_inactivity_raw_value, adult_obesity_raw_value, homeownership_raw_value, percent_rural_raw_value, log_response, high_school_completion_raw_value, severe_housing_problems_raw_value, severe_housing_cost_burden_raw_value, suicides_raw_value, child_care_cost_burden_raw_value, school_segregation_raw_value, school_funding_adequacy_raw_value, ratio_of_population_to_primary_care_providers_other_than_physicians, limited_access_to_healthy_foods_raw_value, ratio_of_population_to_mental_health_providers, poor_mental_health_days_raw_value)

possible_predictors |> 
  select(poor_or_fair_health_raw_value, poor_physical_health_days_raw_value, physical_inactivity_raw_value, adult_obesity_raw_value, homeownership_raw_value, percent_rural_raw_value, log_response, high_school_completion_raw_value, severe_housing_problems_raw_value, severe_housing_cost_burden_raw_value, suicides_raw_value, child_care_cost_burden_raw_value, school_segregation_raw_value, school_funding_adequacy_raw_value, ratio_of_population_to_primary_care_providers_other_than_physicians, limited_access_to_healthy_foods_raw_value, ratio_of_population_to_mental_health_providers, poor_mental_health_days_raw_value) |> 
  drop_na() |> 
  cor() |> 
  ggcorrplot(lab=T) 


# Correlation ------------------------------------------------------------------
>>>>>>> 663b75b781225f48e8bd9f065b1bf86293d81b0a

# now, the predictors
possible_predictors <- df_numeric_plus_county |> 
  select(x5_digit_fips_code, state_abbreviation, name, frequent_mental_distress, food_insecurity, adult_smoking , physical_inactivity , adult_obesity , excessive_drinking , high_school_completion , broadband_access , poor_mental_health_days , ratio_of_population_to_mental_health_providers) #select the ones we want, using correlations from Nikhil's code

<<<<<<< HEAD
# Select and preprocess the data
cor_matrix <- possible_predictors |> 
  select(
    frequent_mental_distress , 
    food_insecurity , 
    adult_smoking , 
    physical_inactivity , 
    adult_obesity , 
    excessive_drinking , 
    high_school_completion , 
    broadband_access , 
    poor_mental_health_days , 
    ratio_of_population_to_mental_health_providers
  ) |>  #i am removing name, state, etc from possible_predictors
  drop_na() |> 
  cor() #make correlation matrix 


full_cor <- possible_predictors |> 
  select(
    frequent_mental_distress , 
    food_insecurity , 
    adult_smoking , 
    physical_inactivity , 
    adult_obesity , 
    excessive_drinking , 
    high_school_completion , 
    broadband_access , 
    poor_mental_health_days , 
    ratio_of_population_to_mental_health_providers
  ) |>  #i am removing name, state, etc from possible_predictors
  drop_na() |> 
  =======
  corr_unique <- corr |> 
  distinct(value, .keep_all = T) |> 
  filter( (value > 0.7 & value < 1) | value < -0.7)

corr_unique

df_numeric_predictors <- df_numeric |> 
  select(!c(poor_physical_health_days_raw_value, physical_inactivity_raw_value, high_school_completion_raw_value, food_insecurity_raw_value, poor_mental_health_days_raw_value, adult_smoking_raw_value, frequent_mental_distress_raw_value, population_raw_value, percent_not_proficient_in_english_raw_value, adult_obesity_raw_value, severe_housing_problems_raw_value, percent_hispanic_raw_value, percentage_of_households_with_high_housing_costs, poor_or_fair_health_raw_value)) 


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
    poor_fair_health = poor_or_fair_health_raw_value,
    pop_to_mental_health_providers = ratio_of_population_to_mental_health_providers
  )

library(ggcorrplot)
df_numeric_predictors |> 
  >>>>>>> 663b75b781225f48e8bd9f065b1bf86293d81b0a
cor() |> 
  ggcorrplot(lab=T, type="lower")


#make it for one variable only
row_of_interest <- "poor_mental_health_days"

masked_cor_matrix <- matrix(NA, nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
rownames(masked_cor_matrix) <- rownames(cor_matrix)
colnames(masked_cor_matrix) <- colnames(cor_matrix)

masked_cor_matrix[row_of_interest, ] <- cor_matrix[row_of_interest, ]

diag(masked_cor_matrix) <- diag(cor_matrix)

variable_labels <- names(possible_predictors)
variable_labels <- tools::toTitleCase(variable_labels)

response_cor <- ggcorrplot(
  masked_cor_matrix,
  type="lower",
  lab=TRUE)



full_cor + response_cor 


# now, scatterplots!

broadband <- possible_predictors |> 
  ggplot(aes(x=poor_mental_health_days , y= broadband_access )) +
  geom_point(alpha=0.5, color = "hotpink")

mental_distress <- possible_predictors |> 
  ggplot(aes(x=poor_mental_health_days , y=frequent_mental_distress  )) +
  geom_point(alpha=0.5, color = "hotpink")

food_insecurity <- possible_predictors |> 
  ggplot(aes(x=poor_mental_health_days , y= food_insecurity )) +
  geom_point(alpha=0.5, color = "hotpink")

smoking <- possible_predictors |> 
  ggplot(aes(x=poor_mental_health_days , y= adult_smoking )) +
  geom_point(alpha=0.5, color = "hotpink")

inactivity <- possible_predictors |> 
  ggplot(aes(x=poor_mental_health_days , y= physical_inactivity )) +
  geom_point(alpha=0.5, color = "hotpink")

obesity <- possible_predictors |> 
  ggplot(aes(x=poor_mental_health_days , y=adult_obesity  )) +
  geom_point(alpha=0.5, color = "hotpink")

drinking <- possible_predictors |> 
  ggplot(aes(x=poor_mental_health_days , y= excessive_drinking )) +
  geom_point(alpha=0.5, color = "hotpink")

library(patchwork)
broadband + mental_distress + food_insecurity + smoking + inactivity + obesity + drinking
