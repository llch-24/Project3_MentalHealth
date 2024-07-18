library(tidyverse)
lm <- lm(poor_mental_health_days ~  frequent_mental_distress + adult_smoking + physical_inactivity + adult_obesity,
         data = counties_only)

lm_alt <- lm(poor_mental_health_days ~  frequent_mental_distress + adult_smoking + physical_inactivity + adult_obesity + ratio_of_population_to_mental_health_providers,
             data = counties_only)

library(broom)

extract_metrics <- function(mod, mod_name)
{
  broom::glance(mod) |>  mutate(mod_name = mod_name)
}


all_metrics <- purrr::map2_dfr(list(lm, lm_alt),
                               as.character(1:2),
                               extract_metrics)

all_metrics |>  glimpse()

all_metrics |> 
  select(mod_name, df, adj.r.squared, BIC) %>%
  pivot_longer(!c("mod_name")) %>%
  mutate(mod_name = factor(mod_name, levels = 1:10)) %>%
  ggplot(mapping = aes(x = mod_name, y = value)) +
  geom_point(size = 3) +
  facet_wrap(~name, scales = "free_y", ncol = 3) +
  scale_x_discrete(breaks = as.character(1:10)) +
  theme_bw()