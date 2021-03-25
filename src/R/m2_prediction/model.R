#####
## Predicting code
#####

## Libraries
library(boot)
library(cmdstanr)
library(tidyverse)
df <- read_csv("data/us_input/polling_error/polls_pres_dataset_00_20.csv")
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
  dplyr::select(-year) %>%
  left_join(df %>% distinct(state, State), by = c("state_po" = "state"))
states_2020_ordered_lower <- results_2020 %>% filter(!is.na(State)) %>%
  arrange(finalTwoPartyVSDemocratic) %>% pull(State)
state_abb_full <- results_2020 %>%
  pull(State)
states_2020_ordered_full <- results_2020 %>%
  arrange(dem) %>%
  filter(!is.na(State)) %>%
  pull(State)
us_regions <- read_csv("data/us_input/polling_error/us census bureau regions and divisions.csv")
###############################################################################
## Model for scales
m <- file.path("code/stan/obs/input/polling_error/polling_error_additive_ar",
               "polling_error_additive_ar.stan")
mod <- cmdstan_model(m)
state_abb <- df %>%
  pull(state) %>%
  unique() %>%
  sort()


prediction_df <- data.frame()
for (miss_t in seq(2008, 2020, 4)){
  print(miss_t)
  obs <- (miss_t - 2000)/4

  df_subset <- df %>%
    filter(year < miss_t) %>%
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
    ungroup()
  # This code adjusts for the edge case of Wyoming missing in 2000, 2004
  max_s <- df_subset %>%
    pull(s) %>%
    max()
  df_subset <- df_subset %>%
    left_join(data.frame(x = 1:(max_s * obs),
                         t = sort(rep(1:obs, max_s)),
                         s = rep(1:max_s, obs)))


  r_index <- df_subset %>% distinct(s, r) %>% arrange(s) %>% pull(r)
  d_index <- df_subset %>% distinct(s, d) %>% arrange(s) %>% pull(d)
  indexes <- data.frame(x = 1:max_s
  ) %>%
    mutate(r = r_index[x],
           d = d_index[x])

  data_list <- list(
    N = nrow(df_subset),
    R = df_subset %>% pull(r) %>% max(),
    D = df_subset %>% pull(d) %>% max(),
    T = df_subset %>% pull(t) %>% max(),
    S = df_subset %>% pull(s) %>% max(),
    rt = df_subset %>% pull(rt),
    dt = df_subset %>% pull(dt),
    t = df_subset %>% pull(t),
    x = df_subset %>% pull(x),
    y = df_subset %>% pull(y),
    n = df_subset %>% pull(n),
    outcome = df_subset %>% pull(outcome),
    pred_x = indexes %>% pull(x),
    pred_d = indexes %>% pull(d),
    pred_r = indexes %>% pull(r)
  )
  fit <- mod$sample(
    data = data_list,
    seed = 123,
    chains = 4,
    parallel_chains = 4,
    refresh = 0,
    init = 0.2
  )

  # Clean prediction
  prediction_loo <- fit$draws("pred") %>%
    posterior::as_draws_df() %>%
    mutate(jj = 1:n()) %>%
    filter(jj <= 1000) %>%
    pivot_longer(everything(),
                 names_to = "s",
                 values_to = "val") %>%
    mutate(missing = miss_t,
           s = as.integer(str_match(s,"(\\d+)")[,2])) %>%
    filter(!is.na(s))
  prediction_df <- bind_rows(prediction_df, prediction_loo)
}
