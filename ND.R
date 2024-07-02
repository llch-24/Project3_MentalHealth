library(janitor)
library(tidyverse)


theme_set(theme_minimal())

# LOAD AND CLEAN DATA --------------------------------
df <- read_csv("analyticdata2024 - clean_data (1).csv")
df <- df |> 
  janitor::clean_names()

df <- df[grepl(" County", df$name), ] # take out states, keep only counties

names(df) <- gsub("_raw_value$", "", names(df)) # take out "_raw_value"

# # in case we need only state data:
# states_to_exclude <- c("New Hampshire", "New Jersey", "New York", "New Mexico", "North Carolina", "North Dakota", "Rhode Island", "South Carolina", "South Dakota", "West Virginia", "United States")
# 
# counties_only <- df |> 
#   filter(grepl(" ", name) | name %in% states_to_exclude)

# North Dakota k-folds
ND <- df |> 
  filter(state_abbreviation == "ND") 

library(caret)
set.seed(24)

formula <- poor_mental_health_days ~  frequent_mental_distress + adult_smoking + physical_inactivity + adult_obesity

train_control <- trainControl(method = "cv",
                              number = 5)

model <- train(formula, data = ND,
               trControl = train_control,
               method = "lm")
print(model)

predictions <- predict(model, newdata = ND)

predictions

# what is the issue.......
results <- data.frame(
  Actual = ND$poor_mental_health_days,
  Predicted = predictions,
  Fold = rep(1:5, each = length(predictions) / 5)  # Assuming 5-fold cross-validation
)
