library(tidyverse)
library(here)
library(gt)
library(gtsummary)
library(viridis)
library(hrbrthemes)


# Load data created in ED&D survey folder
results_2 <- readRDS(file = here('../ED&D/Survey/output/results_2.rds'))

p <-results_2 %>%
  select(condition, dis_labor) %>%
  group_by(condition) %>%
  count(dis_labor) %>%
  mutate(cumulative = cumsum(n),
         proportion = n/sum(n))

ggplot(p,
       mapping = aes(
         x = dis_labor, y = condition, fill = proportion)
       ) +
  geom_tile() +
  scale_fill_distiller(palette = "RdPu") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90))

library(ggridges)
library(ggplot2)

ggplot(p, aes(x = proportion, y = dis_labor, fill = dis_labor)) +
  geom_density_ridges() +
  scale_x_continuous(labels = scales::percent) +
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(x = "Response %", y = NULL,
       title = "To what do attribute differences in job market outcomes for students with disabilities?")


  
  

