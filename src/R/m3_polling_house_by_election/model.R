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
  group_by(pollName, year) %>%
  mutate(p = cur_group_id()) %>%
  group_by(pollName, year, state) %>%
  mutate(pt = cur_group_id(),
       outcome = finalTwoPartyVSDemocratic / 100,
       n = floor((republican + democratic)/100 * numberOfRespondents),
       y = floor(democratic/100 * numberOfRespondents),
       total = n()) %>%
  group_by(pollName, year) %>%
  mutate(total2 = n()) %>%
  filter(total != total2) %>%
  ungroup() %>%
  group_by(pollName, year) %>%
  mutate(p = cur_group_id()) %>%
  group_by(pollName, year, state) %>%
  mutate(pt = cur_group_id())
results <- read_csv("dta/potus_results_76_20.csv")

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

###############################################################################
## Model for scales
m <- file.path("src/stan/m02_polling_houses.stan")
mod <- cmdstan_model(m)

data_list <- list(
  N = nrow(df),
  P = df %>% pull(p) %>% max(),
  PT = df %>% pull(pt) %>% max(),
  pt = df %>% pull(pt),
  p = df %>% distinct(pt, p) %>% pull(p),
  y = df %>% pull(y),
  n = df %>% pull(n),
  outcome = df %>% pull(outcome)
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
fit$save_object(file = "fit/saved_fit_m2_polling_houses.RDS")
