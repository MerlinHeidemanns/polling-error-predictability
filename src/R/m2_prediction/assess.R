

## Full
prediction_df <- read_rds(file = "fit/m1_predictions.RDS")

## Full
fit <- read_rds(file = "fit/m00_reference_model.RDS")

full_est <- fit$draws("mu_matrix") %>%
  posterior::as_draws_df() %>%
  mutate(jj = 1:n()) %>%
  filter(jj <= 1000) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "val") %>%
  mutate(t = as.integer(str_match(variable,",(\\d+)")[,2]),
         s = state_abb[as.integer(str_match(variable,"(\\d+).")[,2])],
         t = 2000 + (t - 1) * 4) %>%
  filter(t > 2004) %>%
  rename(full_dist = val) %>%
  select(-variable) %>%
  filter(!is.na(s)) %>%
  group_by(s, t) %>%
  mutate(draw = 1:n())



### Compare
loo_pred <- read_rds(file = "fit/m1_predictions.RDS")
loo_pred <- loo_pred %>%
  as_tibble() %>%
  mutate(s = state_abb[as.integer(s)]) %>%
  rename(t = missing) %>%
  group_by(s, t) %>%
  mutate(draw = 1:n())

full_est <- full_est %>%
  left_join(loo_pred) %>%
  mutate(full_dist = (inv.logit(full_dist) - 0.5) * 100,
         val = (inv.logit(val) - 0.5) * 100)
write_rds(full_est, file = "dta/saved_output_m01_prediction.Rds")

s_subset <- sample(full_est$s %>% unique(), 30)
plt <- ggplot(full_est %>% filter(s %in% s_subset, draw <= 500),
              aes(x = full_dist, y = val, color = as.factor(t))) +
  geom_point(size = 0.3, alpha = 0.3) +
  geom_abline(aes(intercept = 0, slope = 1), size = 0.5) +
  facet_wrap(s~.) +
  theme_light() +
  labs(x = "Full model",
       y = "Loo prediction")


plt <- ggplot(full_est,
              aes(x = full_dist - val)) +
  geom_histogram(size = 0.5, bins = 100) +
  geom_vline(aes(xintercept = 0), size = 0.5) +
  facet_grid(t~.) +
  theme_light() +
  labs(x = "Full estimate - out of sample prediction from loo model",
       caption = "Positive: Underestimate
                  Negative: Overestimate")


full_est %>%
  group_by(s, t) %>%
  summarize(p_bigger = mean(full_dist > val)) %>%
  ggplot(aes(x = as.factor(t), y = p_bigger)) +
    geom_violin()

full_est %>%
  group_by(s) %>%
  summarize(p_bigger = mean(full_dist > val)) %>%
  arrange(p_bigger) %>%
  mutate(s = factor(s, levels = results_2020 %>%
                      arrange(finalTwoPartyVSDemocratic) %>%
                      pull(state_po))) %>%
  ggplot(aes(x = s, y = p_bigger)) +
    geom_point()

full_est %>%
  group_by(s, t) %>%
  summarize(p_bigger = mean(full_dist > val)) %>%
  mutate(s = factor(s, levels = results_2020 %>%
                      arrange(finalTwoPartyVSDemocratic) %>%
                      pull(state_po))) %>%
  ggplot(aes(x = s, y = p_bigger)) +
    geom_point() +
    facet_wrap(t~.)


