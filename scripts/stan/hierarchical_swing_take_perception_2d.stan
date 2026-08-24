data {
  int<lower=1> N;
  int<lower=1> P;
  int<lower=1> K;
  int<lower=1> S;
  array[N] int<lower=0> swings;
  array[N] int<lower=1> trials;
  array[N] int<lower=1, upper=P> player;
  array[N] int<lower=1, upper=3> strike_group;
  vector[N] edge_distance;
  matrix[N, K] X;
  matrix[N, S] spatial_basis;
  vector[3] lower_prior_mean;
  vector[3] range_prior_mean;
}

parameters {
  real mu_log_sigma;
  real<lower=0> tau_log_sigma;
  vector[P] z_log_sigma;

  real mu_threshold;
  real<lower=0> tau_threshold;
  vector[P] z_threshold;

  vector[K] beta;
  matrix[3, S] spatial_shift_beta;
  vector[3] lower_logit;
  vector[3] range_logit;
}

transformed parameters {
  vector<lower=0>[P] sigma_player;
  vector[P] threshold_player;
  vector<lower=0, upper=1>[3] lower_swing;
  vector<lower=0, upper=1>[3] upper_swing;

  sigma_player = exp(mu_log_sigma + tau_log_sigma * z_log_sigma);
  threshold_player = mu_threshold + tau_threshold * z_threshold;
  lower_swing = inv_logit(lower_logit);
  upper_swing = lower_swing +
    (1 - lower_swing) .* inv_logit(range_logit);
}

model {
  vector[N] spatial_shift;
  vector[N] decision_margin;
  vector[N] perceived_swing_side;
  vector[N] swing_probability;

  mu_log_sigma ~ normal(log(3), 0.75);
  tau_log_sigma ~ normal(0, 0.5);
  z_log_sigma ~ std_normal();

  mu_threshold ~ normal(0, 3);
  tau_threshold ~ normal(0, 2);
  z_threshold ~ std_normal();
  beta ~ normal(0, 1.5);

  // The surface is measured in inches and only represents 2D deviations
  // from the known ABS-boundary distance. Its basis was orthogonalized
  // against distance before entering Stan, protecting sigma's scale.
  to_vector(spatial_shift_beta) ~ normal(0, 1.5);

  lower_logit ~ normal(lower_prior_mean, 1);
  range_logit ~ normal(range_prior_mean, 1);

  for (n in 1:N) {
    spatial_shift[n] = dot_product(
      spatial_basis[n], spatial_shift_beta[strike_group[n]]
    );
  }
  decision_margin = threshold_player[player] + X * beta +
    spatial_shift - edge_distance;
  perceived_swing_side = Phi(
    decision_margin ./ sigma_player[player]
  );
  swing_probability = lower_swing[strike_group] +
    (upper_swing[strike_group] - lower_swing[strike_group]) .*
      perceived_swing_side;
  swings ~ binomial(trials, swing_probability);
}
