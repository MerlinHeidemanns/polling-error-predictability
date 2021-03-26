## Libraries
library(cmdstanr)
library(tidyverse)
library(boot)
## Load
fit <- read_rds(file = "fit/saved_fit_m2_polling_houses.RDS")
##
zeta_hat <- fit$draws("zeta") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "pt",
               values_to = "draw",
               names_pattern = "(\\d+)") %>%
  mutate(pt = as.integer(pt)) %>%
  filter(!is.na(pt)) %>%
  left_join(df %>%
              ungroup() %>%
              distinct(pt, pollName, year, state)) %>%
  group_by(pollName, year, state) %>%
  summarize(
    q50 = quantile(draw, 0.5),
    q25 = quantile(draw, 0.25),
    q75 = quantile(draw, 0.75),
    q10 = quantile(draw, 0.1),
    q90 = quantile(draw, 0.9)
  )
zeta_hat <- zeta_hat %>%
  group_by(pollName, state) %>%
  filter(n() > 3) %>%
  ungroup()

out_list <- list()
for (i in unique(zeta_hat$pollName)){
  out_list[[i]] <- zeta_hat %>%
    filter(pollName == i) %>%
    select(-pollName) %>%
    mutate(year = as.character(year)) %>%
    pivot_wider(id_cols = state,
                names_from = c(year),
                values_from = q50) %>%
    select(-state) %>%
    as.matrix() %>%
    cor(use = "pairwise.complete.obs")
  rownames(out_list[[i]]) <- colnames(out_list[[i]])
}

ggcorrplot::ggcorrplot(out_list[[4]], lab = TRUE)





zeta_sigma_hat <- fit$draws("sigma_zeta") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "p",
               values_to = "draw",
               names_pattern = "(\\d+)") %>%
  mutate(p = as.integer(p)) %>%
  filter(!is.na(p)) %>%
  left_join(df %>%
              ungroup() %>%
              distinct(p, pollName, year)) %>%
  group_by(pollName, year) %>%
  summarize(
    q50 = quantile(draw, 0.5),
    q25 = quantile(draw, 0.25),
    q75 = quantile(draw, 0.75),
    q10 = quantile(draw, 0.1),
    q90 = quantile(draw, 0.9)
  )

zeta_mu_hat <- fit$draws("mu_zeta") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "p",
               values_to = "draw",
               names_pattern = "(\\d+)") %>%
  mutate(p = as.integer(p),
         draw = (inv.logit(draw) - 0.5) * 100) %>%
  filter(!is.na(p)) %>%
  left_join(df %>%
              ungroup() %>%
              distinct(p, pollName, year)) %>%
  group_by(pollName, year) %>%
  summarize(
    q50 = quantile(draw, 0.5),
    q25 = quantile(draw, 0.25),
    q75 = quantile(draw, 0.75),
    q10 = quantile(draw, 0.1),
    q90 = quantile(draw, 0.9)
  )

zeta_mu_hat %>%
  group_by(pollName) %>%
  ggplot(aes(x = year, y = q50)) +
    geom_jitter(width = 1) +
    geom_smooth(method = "lm") +
    theme_light() +
    labs(y = "Polling error (%, favoring Democrats)",
         caption = "Average error by pollster") +
    theme(axis.title.x = element_blank()) +
    geom_hline(aes(yintercept = 0), color = "red", linetype = 2)

zeta_sigma_hat %>%
  group_by(pollName) %>%
  ggplot(aes(x = year, y = q50)) +
  geom_jitter(width = 1) +
  geom_smooth()

zeta_mu_hat %>%
  rename(q50_mu = q50,
         q25_mu = q25,
         q75_mu = q75,
         q10_mu = q10,
         q90_mu = q90
         ) %>%
  left_join(zeta_sigma_hat %>%
              rename(q50_sigma = q50,
                     q25_sigma = q25,
                     q75_sigma = q75,
                     q10_sigma = q10,
                     q90_sigma = q90
              )) %>%
  ggplot(aes(x = q50_sigma,
             y = q50_mu,
             color = as.factor(year))) +
    geom_point()


zeta_mu_hat %>%
  group_by(pollName) %>%
  arrange(year) %>%
  mutate(lag_q50 = ifelse((year - lag(year) - 4) == 0, lag(q50), NA)) %>%
  lm(q50 ~ lag_q50, data = .) %>%
  summary()




