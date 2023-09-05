library(tidyverse)
library(scales)

dat <- read_csv("data/states.csv")

dat |> 
  ggplot(aes(x = inc_med, y = biden_vshare)) +
  geom_text(aes(label = st)) +
  scale_x_continuous(labels = ~ dollar(.x, scale = 0.001, suffix = "k")) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Median Income",
    y = "Biden Voteshare"
  ) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "in"))

ggsave("figures/states.png", width = 5, height = 4.5)
