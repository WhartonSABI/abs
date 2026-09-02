data {
  int<lower=1> N;
  int<lower=1> P;
  int<lower=0> K;
  int<lower=1> S;
  array[N] int<lower=0, upper=1> swing;
  array[N] int<lower=1, upper=P> player;
  array[N] int<lower=1, upper=3> strike_group;
  vector[N] edge_distance;
  vector[N] normal_x;
  vector[N] normal_z;
  int<lower=0, upper=1> estimate_anisotropy;
  matrix[N, K] X;
  matrix[N, S] sector_basis;
  vector[3] lower_prior_mean;
  vector[3] range_prior_mean;
}

parameters {
  real mu_log_sigma;
  real<lower=0> tau_log_sigma;
  vector[P] z_log_sigma;
  real log_anisotropy_raw;

  real mu_threshold;
  real<lower=0> tau_threshold;
  vector[P] z_threshold;

  vector[K] beta_context;
  matrix[3, S] beta_sector;
  vector[3] lower_logit;
  vector[3] range_logit;
}

transformed parameters {
  vector<lower=0>[P] sigma_player =
    exp(mu_log_sigma + tau_log_sigma * z_log_sigma);
  real log_anisotropy = estimate_anisotropy * log_anisotropy_raw;
  real<lower=0> anisotropy_ratio = exp(log_anisotropy);
  vector[P] threshold_player =
    mu_threshold + tau_threshold * z_threshold;
  vector<lower=0, upper=1>[3] lower_swing = inv_logit(lower_logit);
  vector<lower=0, upper=1>[3] upper_swing = lower_swing +
    (1 - lower_swing) .* inv_logit(range_logit);
}

model {
  mu_log_sigma ~ normal(log(3), 0.75);
  tau_log_sigma ~ normal(0, 0.5);
  z_log_sigma ~ std_normal();
  log_anisotropy_raw ~ normal(0, 0.35);

  mu_threshold ~ normal(0, 3);
  tau_threshold ~ normal(0, 2);
  z_threshold ~ std_normal();
  beta_context ~ normal(0, 1.5);
  to_vector(beta_sector) ~ normal(0, 1);
  lower_logit ~ normal(lower_prior_mean, 1);
  range_logit ~ normal(range_prior_mean, 1);

  for (n in 1:N) {
    real context_shift = dot_product(X[n], beta_context);
    real sector_shift = dot_product(
      sector_basis[n], beta_sector[strike_group[n]]
    );
    real normal_transition_sd = sigma_player[player[n]] * sqrt(
      square(normal_x[n] * anisotropy_ratio) +
      square(normal_z[n] / anisotropy_ratio)
    );
    real perceived_inside = Phi(
      (threshold_player[player[n]] + context_shift + sector_shift -
       edge_distance[n]) / normal_transition_sd
    );
    real probability = lower_swing[strike_group[n]] +
      (upper_swing[strike_group[n]] - lower_swing[strike_group[n]]) *
      perceived_inside;
    swing[n] ~ bernoulli(probability);
  }
}
