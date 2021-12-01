library(tidyverse)
library(here)
library(gt)
library(gtsummary)
library(viridis)
library(hrbrthemes)
library(ggridges)
library(ggplot2)

p_state <-results_2 %>%
  select(condition, dispro_state) %>%
  group_by(condition) %>%
  count(dispro_state) %>%
  mutate(cumulative = cumsum(n),
         proportion = n/sum(n))

ggplot(p_state, aes(x = proportion, y = dispro_state, fill = dispro_state)) +
  geom_density_ridges() +
  scale_x_continuous(labels = scales::percent) +
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(x = "Response %", y = NULL,
       title = "Is your district better off to handle disproportionality this year compared to last?")
