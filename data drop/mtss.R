library(tidyverse)
library(here)
library(gt)
library(gtsummary)
library(viridis)
library(hrbrthemes)
library(ggridges)
library(ggplot2)

results_mtss <- results_2 %>%
  mutate(across(matches("^mtss.*_t$"), recode, 
                "I don't know" = 0, "Not true at all" = 1,  "Somewhat true" = 2, "Definitely true" = 3),
         across(matches("^gen_ed.*_t$"), recode, 
                "I don't know" = 0, "Not true at all" = 1,  "Somewhat true" = 2, "Definitely true" = 3),
         across(matches("^mtss.*_i$"), recode, 
                "Low" = 0, "Moderate" = 1,  "High" = 2),
         across(matches("^gen_ed.*_i$"), recode, 
                "Low" = 0, "Moderate" = 1,  "High" = 2)) %>%
  rowwise() %>% # create numeric index of mtss and gen_ed process
  mutate(gened_total = sum(c_across(gen_ed_need_t:gen_ed_evidence_i), na.rm = TRUE),
         mtss_total = sum(c_across(mtss_outcomes_t:mtss_routines_i), na.rm = TRUE)) %>%
  mutate(mtss_pct = mtss_total/20,
         gened_pct = gened_total/25) %>%
  mutate(gen_ed_label = case_when(
    gened_pct > .8 ~ "Strong", # create 3 categories of mtss/gened
    gened_pct < .4 ~ "Weak", 
    TRUE ~ "Moderate"),
    mtss_label = case_when(
      mtss_pct > .8 ~ "Strong", # create 3 categories of mtss/gened
      mtss_pct < .4 ~ "Weak",
      TRUE ~ "Moderate")
  ) %>%
select(condition, mtss_total, gened_total, mtss_pct, gened_pct, gen_ed_label, mtss_label) %>%
  mutate(mtss_label = factor(mtss_label),
         gen_ed_label = factor(gen_ed_label)) %>%
  mutate(mtss_label = fct_relevel(mtss_label, "Weak", "Moderate", "Strong"),
         gen_ed_label = fct_relevel(gen_ed_label, "Weak", "Moderate", "Strong"))

p_mtss <-results_mtss %>%
  select(condition, mtss_label) %>%
  group_by(condition) %>%
  count(mtss_label) %>%
  mutate(cumulative = cumsum(n),
         proportion = n/sum(n)) %>%
  filter(!is.na(mtss_label))

ggplot(p_mtss, aes(x = proportion, y = mtss_label, fill = mtss_label)) +
  geom_density_ridges() +
  scale_x_continuous(labels = scales::percent) +
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(x = "Response %", y = NULL,
       title = "How strong is your district's Multi-tiered System of Support?")

ggplot(p_mtss, aes(x = proportion, y = mtss_label, fill = mtss_label)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal() + 
  scale_fill_viridis_d() +
  theme(legend.position = "none") +
  labs(x = "Response %", y = NULL,
       title = "How strong is your district's Multi-tiered System of Support?") +
  facet_wrap(vars(condition))
