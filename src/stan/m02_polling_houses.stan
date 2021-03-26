data {
  int N;
  int P;
  int PT;
  int p[PT];
  int pt[N];
  int y[N];
  int n[N];
  vector[N] outcome;
}
transformed data {
  vector[N] logit_outcome;
  logit_outcome = logit(outcome);
}
parameters {
  vector[P] mu_zeta;
  vector[PT] raw_zeta;
  vector<lower = 0>[P] sigma_zeta;
}
transformed parameters {
  vector[PT] zeta;
  zeta = mu_zeta[p] + raw_zeta .* sigma_zeta[p];
}
model {
  mu_zeta ~ normal(0, 1);
  raw_zeta ~ normal(0, 1);
  sigma_zeta ~ normal(0, 1);
  y ~ binomial_logit(n, logit_outcome +
    zeta[pt]);
}
