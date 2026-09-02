// Shared trust in the observed call, estimated only from challenge/pass choices.
//
// Upstream code supplies q[n, s, g]: the success probability for decision n,
// latent player signal s, and fixed trust knot omega_grid[g]. This program does
// not receive ABS truth, overturn results, or challenge success. It linearly
// interpolates q along omega and averages the Bernoulli decision likelihood over
// the supplied signal weights.
//
// Piecewise-linear interpolation is continuous but has derivative changes at
// fixed knots. Those points have probability zero under a continuous posterior;
// the R-side grid-convergence diagnostic must pass before an estimated omega is
// eligible for promotion.

data {
  int<lower=1> N;
  int<lower=1> S;
  int<lower=3> G;
  int<lower=1> P;
  int<lower=1> T;
  int<lower=0> K;

  array[N] int<lower=0, upper=1> challenged;
  array[N] int<lower=1, upper=P> player;
  array[N] int<lower=1, upper=T> team;
  vector<lower=0>[N] gain;
  vector<lower=0>[N] inventory_loss;
  matrix[N, K] X;
  vector[N] sampling_offset;

  vector[G] omega_grid;
  // R flattens an N x S x G array with row n + (s - 1) * N.
  matrix<lower=0, upper=1>[N * S, G] q_grid;
  matrix<lower=0>[N, S] signal_weight;

  int<lower=0, upper=1> estimate_omega;
  real<lower=0, upper=1.5> omega_fixed;
  real<lower=0, upper=1.5> omega_prior_mean;
  real<lower=0> omega_prior_sd;
}

transformed data {
  if (abs(omega_grid[1]) > 1e-10 || abs(omega_grid[G] - 1.5) > 1e-10) {
    reject("omega_grid must span [0, 1.5]");
  }
  for (g in 2:G) {
    if (omega_grid[g] <= omega_grid[g - 1]) {
      reject("omega_grid must be strictly increasing");
    }
  }
  for (n in 1:N) {
    if (abs(sum(signal_weight[n]) - 1) > 1e-8) {
      reject("Each signal-weight row must sum to one");
    }
  }
}

parameters {
  // A zero-length vector makes the same program usable for fixed-omega stages
  // without introducing an unidentified nuisance parameter.
  vector<lower=0, upper=1.5>[estimate_omega] omega_free;

  real mu_player;
  real<lower=0> tau_player;
  vector[P] z_player;
  real<lower=0> tau_team;
  vector[T] z_team;
  real<lower=0> decision_slope;
  vector[K] gamma;
}

transformed parameters {
  real<lower=0, upper=1.5> omega_shared = omega_fixed;
  vector[P] alpha_player = mu_player + tau_player * z_player;
  vector[T] alpha_team = tau_team * z_team;

  if (estimate_omega == 1) omega_shared = omega_free[1];
}

model {
  mu_player ~ normal(-5, 2);
  tau_player ~ normal(0, 1);
  z_player ~ std_normal();
  tau_team ~ normal(0, 0.5);
  z_team ~ std_normal();
  decision_slope ~ lognormal(0, 0.75);
  gamma ~ normal(0, 1);
  if (estimate_omega == 1) {
    omega_free[1] ~ normal(omega_prior_mean, omega_prior_sd);
  }

  // Keep interpolated row values local so N*S transformed parameters are not
  // written to every posterior draw. The knot and fraction are shared by all
  // rows because omega itself is shared.
  {
    int lower_knot = 1;
    real interpolation_fraction;
    for (g in 1:(G - 1)) {
      if (omega_shared >= omega_grid[g]) lower_knot = g;
    }
    interpolation_fraction =
      (omega_shared - omega_grid[lower_knot]) /
      (omega_grid[lower_knot + 1] - omega_grid[lower_knot]);
    for (n in 1:N) {
      vector[S] log_component;
      real eta_base = alpha_player[player[n]] + alpha_team[team[n]] +
        sampling_offset[n];
      if (K > 0) eta_base += dot_product(X[n], gamma);
      for (s in 1:S) {
        int row_index = n + (s - 1) * N;
        real q =
          (1 - interpolation_fraction) * q_grid[row_index, lower_knot] +
          interpolation_fraction * q_grid[row_index, lower_knot + 1];
        real utility = q * gain[n] - (1 - q) * inventory_loss[n];
        real eta = eta_base + decision_slope * utility;
        log_component[s] = log(signal_weight[n, s]) +
          bernoulli_logit_lpmf(challenged[n] | eta);
      }
      target += log_sum_exp(log_component);
    }
  }
}
