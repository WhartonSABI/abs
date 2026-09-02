data {
  int<lower=1> N;
  int<lower=1> Q;
  int<lower=1> P;
  int<lower=1> T;
  int<lower=0> K;
  array[N] int<lower=0, upper=1> challenged;
  array[N] int<lower=1, upper=P> player;
  array[N] int<lower=1, upper=T> team;
  vector<lower=0>[N] gain;
  vector<lower=0>[N] inventory_loss;
  matrix<lower=0, upper=1>[N, Q] q_signal;
  matrix<lower=0, upper=1>[N, Q] signal_weight;
  matrix[N, K] X;
  vector[N] sampling_offset;
}
parameters {
  real mu_player;
  real<lower=0> tau_player;
  vector[P] z_player;
  real<lower=0> tau_team;
  vector[T] z_team;
  real<lower=0> decision_slope;
  vector[K] gamma;
}
transformed parameters {
  vector[P] alpha_player = mu_player + tau_player * z_player;
  vector[T] alpha_team = tau_team * z_team;
}
model {
  mu_player ~ normal(-5, 2);
  tau_player ~ normal(0, 1);
  z_player ~ std_normal();
  tau_team ~ normal(0, 0.5);
  z_team ~ std_normal();
  decision_slope ~ lognormal(0, 0.75);
  gamma ~ normal(0, 1);
  for (n in 1:N) {
    real base = alpha_player[player[n]] + alpha_team[team[n]] +
      sampling_offset[n];
    real p_challenge = 0;
    if (K > 0) base += dot_product(row(X, n), gamma);
    for (q in 1:Q) {
      real utility = q_signal[n, q] * gain[n] -
        (1 - q_signal[n, q]) * inventory_loss[n];
      p_challenge += signal_weight[n, q] *
        inv_logit(base + decision_slope * utility);
    }
    challenged[n] ~ bernoulli(fmin(1 - 1e-9, fmax(1e-9, p_challenge)));
  }
}
