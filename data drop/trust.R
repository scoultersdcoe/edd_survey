library(tidyverse)
library(here)
library(gt)
library(gtsummary)
library(viridis)
library(hrbrthemes)

p_trust <-results_2 %>%
  select(condition, spp_trust) %>%
  group_by(condition) %>%
  count(spp_trust) %>%
  mutate(cumulative = cumsum(n),
         proportion = n/sum(n)) %>%
  filter(!is.na(spp_trust))

ggplot(p_trust, aes(x = proportion, y = spp_trust, fill = spp_trust)) +
  geom_density_ridges() +
  scale_x_continuous(labels = scales::percent) +
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(x = "Response %", y = NULL,
       title = "Do you trust the risk ratio scores you receive from the state?")
