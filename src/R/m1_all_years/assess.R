library(tidyverse)
library(cmdstanr)
library(boot)
## Data
# Region indicators
us_regions <- read_csv("data/us_input/polling_error/us census bureau regions and divisions.csv")
# Polls
df <- read_csv("data/us_input/polling_error/polls_pres_dataset_00_20.csv")
state_abb <- df %>%
  pull(state) %>%
  unique() %>%
  sort()
df <- df %>%
  group_by(state, electionDate) %>%
  mutate(i = cur_group_id(),
         s = match(state, state_abb)) %>%
  ungroup() %>%
  arrange(year) %>%
  group_by(year) %>%
  mutate(t = cur_group_id(),
         outcome = finalTwoPartyVSDemocratic / 100,
         n = floor((republican + democratic)/100 * numberOfRespondents),
         y = floor(democratic/100 * numberOfRespondents)) %>%
  left_join(us_regions, by = c("state" = "State Code")) %>%
  group_by(Region) %>%
  mutate(r = cur_group_id()) %>%
  group_by(Division) %>%
  mutate(d = cur_group_id()) %>%
  group_by(Region, year) %>%
  mutate(rt = cur_group_id()) %>%
  group_by(Division, year) %>%
  mutate(dt = cur_group_id()) %>%
  ungroup() %>%
  left_join(data.frame(x = 1:300,
                       t = sort(rep(1:6, 50)),
                       s = rep(1:50, 6))) %>%
  group_by(pollName, t, s) %>%
  mutate(p = cur_group_id()) %>%
  ungroup()
results <- read_csv("data/us_background/potus_results_76_20.csv") %>%
  left_join(df %>% distinct(state, State), by = c("state_po" = "state"))
states_2020_ordered <- results %>%
  filter(year == 2020) %>%
  mutate(pos = dem/(dem + rep)) %>%
  arrange(pos) %>%
  pull(state_po)
results_2020 <- results %>%
  filter(year == 2020) %>%
  mutate(finalTwoPartyVSRepublican = rep/(dem + rep) * 100,
         finalTwoPartyVSDemocratic = dem/(dem + rep) * 100) %>%
  dplyr::select(-year)
states_2020_ordered_lower <- results_2020 %>% filter(!is.na(State)) %>%
  arrange(finalTwoPartyVSDemocratic) %>% pull(State)
state_abb_full <- results_2020 %>%
  pull(State)

fit <- readRDS("data/us_input/polling_error/polling_error_polling_house.RDS")
## Plot
index_p <- df %>%
  distinct(p, pollName, state, .keep_all = TRUE) %>%
  arrange(p) %>%
  pull(pollName)
index_t <- df %>%
  distinct(p, pollName, state, .keep_all = TRUE) %>%
  arrange(p) %>%
  pull(year)
index_s <- df %>%
  distinct(p, pollName, state, .keep_all = TRUE) %>%
  arrange(p) %>%
  pull(state)


zeta_hat <- fit$draws("zeta") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "p",
               values_to = "draw",
               names_pattern = "(\\d+)") %>%
  mutate(p = as.integer(p),
         draw = 100 * (boot::inv.logit(draw) - 0.5),
         pollster = index_p[p],
         year = index_t[p],
         state = index_s[p]) %>%
  filter(!is.na(p)) %>%
  group_by(pollster, year, state) %>%
  summarize(
    q50 = quantile(draw, 0.5),
    q25 = quantile(draw, 0.25),
    q75 = quantile(draw, 0.75),
    q10 = quantile(draw, 0.10),
    q90 = quantile(draw, 0.90)
  ) %>%
  group_by(pollster, state) %>%
  filter(n() > 3) %>%
  mutate(year = factor(year - 2000, levels = seq(0, 20, 4)))
ggplot(zeta_hat, aes(x = year, y = q50)) +
  geom_point() +
  geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75, width = 0) +
  geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5, width = 0) +
  geom_point() +
  geom_hline(aes(yintercept = 0), linetype = 2, color = "red") +
  facet_wrap(pollster + state ~.) +
  theme_light()


zeta_hat_draws <- fit$draws("zeta") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "p",
               values_to = "draw",
               names_pattern = "(\\d+)") %>%
  mutate(p = as.integer(p),
         draw = 100 * (boot::inv.logit(draw) - 0.5),
         pollster = index_p[p],
         year = index_t[p],
         state = index_s[p]) %>%
  filter(!is.na(p)) %>%
  group_by(pollster, state) %>%
  filter(n() > 4 * 8000)

ggplot(zeta_hat_draws, aes(x = draw, fill = as.factor(year))) +
  geom_histogram(position = "identity", alpha = 0.3, bins = 100) +
  facet_wrap(pollster + state ~.) +
  theme_light()















