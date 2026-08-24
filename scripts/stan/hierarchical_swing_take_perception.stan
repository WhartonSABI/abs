data {
  int<lower=1> N;
  int<lower=1> P;
  int<lower=0> K;
  array[N] int<lower=0> swings;
  array[N] int<lower=1> trials;
  array[N] int<lower=1, upper=P> player;
  vector[N] edge_distance;
  matrix[N, K] X;
}

parameters {
  real mu_log_sigma;
  real<lower=0> tau_log_sigma;
  vector[P] z_log_sigma;

  real mu_threshold;
  real<lower=0> tau_threshold;
  vector[P] z_threshold;

  vector[K] beta;
}

transformed parameters {
  vector<lower=0>[P] sigma_player;
  vector[P] threshold_player;

  sigma_player = exp(mu_log_sigma + tau_log_sigma * z_log_sigma);
  threshold_player = mu_threshold + tau_threshold * z_threshold;
}

model {
  vector[N] center;
  vector[N] core;
  vector[N] swing_probability;

  // Sigma is the width, in inches, of the batter's latent Gaussian location
  // uncertainty. Player effects are non-centered for stable shrinkage.
  mu_log_sigma ~ normal(log(3), 0.75);
  tau_log_sigma ~ normal(0, 0.5);
  z_log_sigma ~ std_normal();

  // Threshold and context terms represent swing strategy, separately from
  // the Gaussian transition width.
  mu_threshold ~ normal(0, 3);
  tau_threshold ~ normal(0, 2);
  z_threshold ~ std_normal();
  beta ~ normal(0, 1.5);

  center = threshold_player[player] + X * beta;
  core = Phi((center - edge_distance) ./ sigma_player[player]);
  swing_probability = core;
  swings ~ binomial(trials, swing_probability);
}
