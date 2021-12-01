library(tidyverse)
library(here)
library(gt)
library(gtsummary)
library(viridis)
library(hrbrthemes)

p_ach <-results_2 %>%
  select(condition, dis_ach_gap) %>%
  mutate(dis_ach_gap = factor(dis_ach_gap),
         dis_ach_gap = fct_relevel(dis_ach_gap, "I don't know", "Decreased", "Stayed the same", "Increased")) %>%
  group_by(condition) %>%
  count(dis_ach_gap) %>%
  mutate(cumulative = cumsum(n),
         proportion = n/sum(n))
  
ggplot(p_ach, aes(x = proportion, y = dis_ach_gap, fill = dis_ach_gap)) +
  geom_density_ridges() +
  scale_x_continuous(labels = scales::percent) +
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(x = "Response %", y = NULL,
       title = "Has the achievement gap for students with disabilities grown over the past 10 years?")
