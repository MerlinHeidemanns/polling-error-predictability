library(tidyverse)
library(cmdstanr)
library(boot)
## Data
# Region indicators
us_regions <- read_csv("dta/us census bureau regions and divisions.csv")
# Polls
df <- read_csv("dta/polls_pres_dataset_00_20.csv")
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
results <- read_csv("dta/potus_results_76_20.csv") %>%
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
turnout <- read_csv("data/us_background/potus_turnout.csv") %>%
  mutate(s = match(state_po, state_abb),
         t = (year - 2000)/4 + 1) %>%
  left_join(results, by = c("year" = "year",
                            "state_full_lower" = "State",
                            "state_po" = "state_po"))
###############################################################################
## Model for scales
m <- file.path("src/stan/m01_all_years.stan")
mod <- cmdstan_model(m)

rt_index <- df %>% distinct(r, t, rt) %>% arrange(r, t)
dt_index <- df %>% distinct(d, t, dt) %>% arrange(d, t)
r_index <- df %>% distinct(s, r) %>% arrange(s) %>% pull(r)
d_index <- df %>% distinct(s, d) %>% arrange(s) %>% pull(d)
indexes <- data.frame(x = 1:50,
                      s = 1:50
) %>%
  mutate(r = r_index[s],
         d = d_index[s])


data_list <- list(
  N = nrow(df),
  R = df %>% pull(r) %>% max(),
  D = df %>% pull(d) %>% max(),
  T = df %>% pull(t) %>% max(),
  S = df %>% pull(s) %>% max(),
  P = df %>% pull(p) %>% max(),
  rt = df %>% pull(rt),
  dt = df %>% pull(dt),
  p = df %>% pull(p),
  t = df %>% pull(t),
  x = df %>% pull(x),
  y = df %>% pull(y),
  n = df %>% pull(n),
  outcome = df %>% pull(outcome),
  pred_d = indexes %>% pull(d),
  pred_r = indexes %>% pull(r)
)
fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_sampling = 1000,
  iter_warmup = 3000,
  refresh = 500
)
fit$save_object(file = "fit/saved_fit_m1_all_years.RDS")
