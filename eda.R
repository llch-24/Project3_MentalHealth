library(tidyverse)
library(reshape)
library(ggcorrplot)
library(patchwork)
library(viridis)

theme_set(theme_bw() + theme(panel.border = element_blank(),
                             panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(),
                             axis.line = element_line(colour = "black"),
                             text = element_text(family = "Times New Roman")))

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
  geom_histogram(fill="#94111f", color = "#c51230", bins = 23) + #normal distribution, assume linearity
  labs(x= "\nPoor Mental Health Days",
       y = "Count",
       title = "Response Variable Appears Normally Distributed",
       subtitle = "Assumption of Normality") + 
  theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
        axis.title = element_text(size=40),
        axis.text.x = element_text(size=30),
        axis.title.y = element_blank(),
        plot.subtitle = element_text(size=20, face = "italic", hjust = 0.5))


summary(df$poor_mental_health_days)

# then visualize response and predictor
response <- df |> 
  ggplot(aes(x=poor_mental_health_days,
             y=ratio_of_population_to_mental_health_providers)) + 
  geom_point(color = "#94111f", alpha=0.45) +
  labs(title = "No Correlation Between Variables", subtitle = "Poor Mental Health Days vs Ratio of Population to Mental Health Providers", x= "Poor Mental Health Days", y= "Ratio of Population to Mental Health Providers") +
  theme(plot.title = element_text(size = 30, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size=20, face = "italic", hjust = 0.5),
        axis.text = element_text(size=20),
        axis.title = element_text(size=25),
        axis.title.x = element_text(margin = margin(t=15)),
        axis.title.y = element_text(margin = margin(r=15)))

response

df |> 
  filter(state_abbreviation == "ND") |> 
  ggplot(aes(x=poor_mental_health_days,
             y=ratio_of_population_to_mental_health_providers)) +
  geom_point()

df |> 
  filter(state_abbreviation == "AR") |> 
  ggplot(aes(x=poor_mental_health_days,
             y=ratio_of_population_to_mental_health_providers)) +
  geom_point()



# now find cor
cor(df$poor_mental_health_days, df$ratio_of_population_to_mental_health_providers, use="complete.obs")

# Correlation ------------------------------------------------------------------
possible_predictors <- df |> 
  select(frequent_mental_distress, food_insecurity, adult_smoking , physical_inactivity , adult_obesity , excessive_drinking , high_school_completion , broadband_access , poor_mental_health_days , ratio_of_population_to_mental_health_providers)

full_cor <- possible_predictors |>
  drop_na() |> 
  cor() |> 
  ggcorrplot(type = "lower") + 
  scale_x_discrete(labels = c("poor_mental_health_days" = "Poor Mental Health Days",
                              "broadband_access" = "Broadband Access",
                              "high_school_completion" = "High School Completion",
                              "excessive_drinking" = "Excessive Drinking",
                              "adult_obesity" = "Adult Obesity",
                              "physical_inactivity" = "Physical Inactivity",
                              "adult_smoking" = "Adult Smoking",
                              "food_insecurity" = "Food Insecurity",
                              "frequent_mental_distress" = "Frequent Mental Distress",
                              "ratio_of_population_to_mental_health_providers" = "Ratio")) + 
  scale_y_discrete(labels = c("poor_mental_health_days" = "Poor Mental Health Days",
                              "broadband_access" = "Broadband Access",
                              "high_school_completion" = "High School Completion",
                              "excessive_drinking" = "Excessive Drinking",
                              "adult_obesity" = "Adult Obesity",
                              "physical_inactivity" = "Physical Inactivity",
                              "adult_smoking" = "Adult Smoking",
                              "food_insecurity" = "Food Insecurity",
                              "frequent_mental_distress" = "Frequent Mental Distress")) +
  theme(legend.text = element_text(size=15),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(2, "cm"),
        plot.title = element_text(size=30, vjust = 1),
        legend.title = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(family = "Times New Roman"))

full_cor


cor_matrix <- cor(possible_predictors)
cor_matrix_vertical <- t(cor_matrix["poor_mental_health_days", ])
response_cor <- ggcorrplot(cor_matrix_vertical,
                           lab = TRUE) +
  theme(legend.text = element_text(size=15),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=20),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(2, "cm"),
        plot.title = element_text(size=30),
        legend.title = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(family = "Times New Roman")) +
  scale_y_discrete(labels = c("poor_mental_health_days" = "Poor Mental Health Days",
                              "broadband_access" = "Broadband Access",
                              "high_school_completion" = "High School Completion",
                              "excessive_drinking" = "Excessive Drinking",
                              "adult_obesity" = "Adult Obesity",
                              "physical_inactivity" = "Physical Inactivity",
                              "adult_smoking" = "Adult Smoking",
                              "food_insecurity" = "Food Insecurity",
                              "frequent_mental_distress" = "Frequent Mental Distress"))

full_cor + response_cor & labs(title = "High Correlation Amongst Possible Predictors") & theme(plot.title = element_text(hjust = -0.5, face = "bold", size = 30))


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
    geom_point(alpha=0.38) +
    geom_smooth(method="lm")
}



mental_distress <- lm_predictors |> 
  ggplot(aes(x=frequent_mental_distress,
             y=poor_mental_health_days)) +
  geom_point(alpha=0.38) + 
  geom_smooth(method="lm") +
  labs(x="Frequent Mental Distress", y="Poor Mental Health Days") +
  
  mental_distress

smoking <- lm_predictors |> 
  ggplot(aes(x=adult_smoking,
             y=poor_mental_health_days)) +
  geom_point(alpha=0.38) + 
  geom_smooth(method="lm") +
  labs(x="Adult Smoking", y="Poor Mental Health Days")

smoking

inactivity <- lm_predictors |> 
  ggplot(aes(x=physical_inactivity,
             y=poor_mental_health_days)) +
  geom_point(alpha=0.38) + 
  geom_smooth(method="lm") +
  labs(x="Physical Inactivity", y="Poor Mental Health Days")

inactivity

obesity <- lm_predictors |> 
  ggplot(aes(x=adult_obesity,
             y=poor_mental_health_days)) +
  geom_point(alpha=0.38) + 
  geom_smooth(method="lm") +
  labs(x="Adult Obesity", y="Poor Mental Health Days")


mental_distress + smoking + inactivity + obesity +
  plot_annotation(title = "Linear Relationship between Response and Predictor Variables") &
  theme(plot.title = element_text(size=30, face = "bold"),
        axis.title = element_text(size=20), 
        axis.text = element_text(size=20))
