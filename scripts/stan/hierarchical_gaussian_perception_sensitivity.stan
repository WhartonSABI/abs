data {
  int<lower=1> N;
  int<lower=1> P;
  int<lower=1> R;
  array[N] int<lower=0, upper=1> y;
  array[N] int<lower=1, upper=P> player;
  array[P] int<lower=1, upper=R> role_of_player;
  vector[N] tracking_z;
  vector<lower=0>[N] spatial_scale;
  vector<lower=0>[N] stake_G_positive;
  vector<lower=0>[N] sampling_offset;
  real<lower=0> ev_reference;
  real<lower=0> epsilon_ev;
}

parameters {
  vector[R] mu_log_sigma;
  real<lower=0> tau_log_sigma;
  vector[P] z_log_sigma;

  vector[R] mu_alpha;
  real<lower=0> tau_alpha;
  vector[P] z_alpha;

  // How sharply each role converts perceived expected value into a decision.
  // A value of one is the original model; values above one mean a more
  // threshold-like response.
  vector<lower=0>[R] beta_role;
}

transformed parameters {
  vector[P] log_sigma_player;
  vector<lower=0>[P] sigma_player;
  vector[P] alpha_player;

  for (p in 1:P) {
    log_sigma_player[p] =
      mu_log_sigma[role_of_player[p]] + tau_log_sigma * z_log_sigma[p];
    sigma_player[p] = exp(log_sigma_player[p]);
    alpha_player[p] = mu_alpha[role_of_player[p]] + tau_alpha * z_alpha[p];
  }
}

model {
  vector[N] eta;

  mu_log_sigma ~ normal(log(3), 0.75);
  tau_log_sigma ~ normal(0, 0.5);
  z_log_sigma ~ std_normal();

  mu_alpha ~ normal(-4, 2);
  tau_alpha ~ normal(0, 1);
  z_alpha ~ std_normal();

  // Median one retains the original fixed-slope model as the prior center,
  // while still allowing the data to reveal a sharper or softer response.
  beta_role ~ lognormal(0, 0.75);

  for (n in 1:N) {
    real attenuation = sqrt(
      1 + square(sigma_player[player[n]] / spatial_scale[n])
    );
    real p_perceived = Phi(tracking_z[n] / attenuation);
    real perceived_ev = fmax(
      p_perceived * stake_G_positive[n], epsilon_ev
    );
    eta[n] = alpha_player[player[n]] +
      beta_role[role_of_player[player[n]]] *
        log(perceived_ev / ev_reference) + sampling_offset[n];
  }
  y ~ bernoulli_logit(eta);
}
