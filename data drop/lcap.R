library(tidyverse)
library(here)
library(gt)
library(gtsummary)
library(viridis)
library(hrbrthemes)

p_lcap <-results_2 %>%
  select(condition, dispro_lcap) %>%
  group_by(condition) %>%
  count(dispro_lcap) %>%
  mutate(cumulative = cumsum(n),
         proportion = n/sum(n)) %>%
  filter(!is.na(dispro_lcap))

ggplot(p_lcap, aes(x = proportion, y = dispro_lcap, fill = dispro_lcap)) +
  geom_density_ridges() +
  scale_x_continuous(labels = scales::percent) +
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(x = "Response %", y = NULL,
       title = "Does your district have an LCAP goal around disproportionality?")
