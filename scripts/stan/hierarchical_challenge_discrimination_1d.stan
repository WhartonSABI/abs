data {
  int<lower=1> N;
  int<lower=1> P;
  int<lower=1> T;
  int<lower=1> U;
  int<lower=1> C;
  int<lower=0> K;
  array[N] int<lower=0, upper=1> challenged;
  array[N] int<lower=1, upper=P> player;
  array[N] int<lower=1, upper=T> team;
  array[N] int<lower=1, upper=U> umpire;
  array[N] int<lower=1, upper=C> catcher;
  vector[N] margin;
  matrix[N, K] X;
  int<lower=0, upper=1> use_player_sigma;
}

parameters {
  real mu_threshold;
  real mu_log_sigma;
  vector<lower=0>[2] tau_player;
  cholesky_factor_corr[2] L_player;
  matrix[2, P] z_player;

  real<lower=0> tau_team;
  vector[T] z_team;
  real<lower=0> tau_umpire;
  vector[U] z_umpire;
  real<lower=0> tau_catcher;
  vector[C] z_catcher;

  vector[K] beta_context;
}

transformed parameters {
  matrix[2, P] player_offset =
    diag_pre_multiply(tau_player, L_player) * z_player;
  vector[P] threshold_player;
  vector[P] log_sigma_player;
  vector<lower=0>[P] sigma_player;
  vector[T] team_shift = tau_team * z_team;
  vector[U] umpire_shift = tau_umpire * z_umpire;
  vector[C] catcher_shift = tau_catcher * z_catcher;
  real rho_threshold_log_sigma =
    use_player_sigma * L_player[2, 1];

  for (p in 1:P) {
    threshold_player[p] = mu_threshold + player_offset[1, p];
    log_sigma_player[p] = mu_log_sigma +
      use_player_sigma * player_offset[2, p];
    sigma_player[p] = exp(log_sigma_player[p]);
  }
}

model {
  // Strong regularization is intentional: threshold and discrimination width
  // can be weakly separated for batters with few challenge opportunities.
  mu_threshold ~ normal(4, 2);
  mu_log_sigma ~ normal(log(3), 0.4);
  tau_player[1] ~ normal(0, 1.25);
  tau_player[2] ~ normal(0, 0.30);
  L_player ~ lkj_corr_cholesky(4);
  to_vector(z_player) ~ std_normal();

  tau_team ~ normal(0, 0.35);
  z_team ~ std_normal();
  tau_umpire ~ normal(0, 0.50);
  z_umpire ~ std_normal();
  tau_catcher ~ normal(0, 0.35);
  z_catcher ~ std_normal();
  beta_context ~ normal(0, 1);

  for (n in 1:N) {
    real threshold = threshold_player[player[n]] +
      team_shift[team[n]] + umpire_shift[umpire[n]] +
      catcher_shift[catcher[n]];
    real eta;
    if (K > 0) threshold += dot_product(X[n], beta_context);

    // The signed-margin coefficient is fixed at one. Consequently sigma is
    // an effective discrimination width in physical inches, not a free slope.
    eta = (margin[n] - threshold) / sigma_player[player[n]];
    if (challenged[n] == 1) {
      target += std_normal_lcdf(eta);
    } else {
      target += std_normal_lccdf(eta);
    }
  }
}
