library(tidyverse)
library(scales)

states_orig <- read_csv("data/counties.csv")

# reorder
states_df <- states_orig |> 
  mutate(state = recode_factor(state, AL = "Alabama", PA = "Pennsylvania", CT = "Connecticut"))

# top 5 counties
states_subset <- states_df |> 
  group_by(state) |> 
  slice_max(summary_est, n = 5) |> 
  ungroup()

# plot
states_df |> 
  ggplot(aes(estimate, biden_vshare)) +
  facet_wrap(~ state) +
  geom_point() +
  geom_text(data = states_subset, aes(label = name), vjust = -1, 
            size = 2) +
  scale_x_continuous(labels = ~ dollar(.x, scale = 0.001, suffix = "k")) +
  scale_y_continuous(labels = percent) +
  theme(panel.spacing.x = unit(0.5, "in")) +
  labs(x = "Median Income",
       y = "Biden Voteshare",
       caption = "Source: 2020 ACS and Election Results by VEST. Five largest counties in state are named.")

ggsave("figures/substate.png", w = 8.5, h = 3)  
