library(tidyverse)
library(here)
library(gt)
library(gtsummary)
library(viridis)
library(hrbrthemes)
# convert to numeric values
data_intuition <- results_2 %>%
  mutate(risk_ratio = ifelse(risk_ratio == TRUE, 1, 0),
         his_pct = case_when(
           his_pct == "10.1%-15%" ~ 4,
           his_pct == "20.1%-25%" ~ 1,
           his_pct == "5.1%-10%" ~ 2,
           his_pct == "15.1%-20%" ~ 3,
           his_pct == "0%-5%" ~ 0,
           TRUE ~ 0),
         dis_ach_gap = case_when(
           dis_ach_gap == "Increased" ~ 2,
           dis_ach_gap == "Stayed the same" ~1,
           dis_ach_gap == "Decreased" ~ 0,
           dis_ach_gap == "I don't know" ~ 0),
         lowinc_dispro = case_when(
           lowinc_dispro == "Excellent" ~ 4, 
           lowinc_dispro == "Good" ~ 3,
           lowinc_dispro == "Fair" ~ 2,
           lowinc_dispro == "Not so good" ~ 1,
           lowinc_dispro == "Poor" ~ 0
         )) %>%
  mutate(data_intuition_num = risk_ratio + his_pct + dis_ach_gap +
           lowinc_dispro) %>% # combine into single index score
  drop_na(data_intuition_num) %>%
  mutate(data_intuition_label = case_when(
    data_intuition_num > 8 ~ "Strong", # create 3 categories of intuition
    data_intuition_num < 6 ~ "Weak",
    TRUE ~ "Moderate"
  )) %>%
  mutate(data_intuition_label = factor(data_intuition_label),
         data_intuition_label = fct_relevel(data_intuition_label, "Weak", "Moderate", "Strong")) %>%
  select(condition, risk_ratio, his_pct, dis_ach_gap, lowinc_dispro, data_intuition_num,
         data_intuition_label)


p_intuition <-data_intuition %>%
  select(condition, data_intuition_label) %>%
  group_by(condition) %>%
  count(data_intuition_label) %>%
  mutate(cumulative = cumsum(n),
         proportion = n/sum(n)) %>%
  filter(!is.na(data_intuition_label))

ggplot(p_intuition, aes(x = proportion, y = data_intuition_label, fill = data_intuition_label)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal() + 
  scale_fill_viridis_d() +
  theme(legend.position = "none") +
  labs(x = "Response %", y = NULL,
       title = "How strong are your data intuition skills regarding disproportionality?") +
  facet_wrap(vars(condition))

