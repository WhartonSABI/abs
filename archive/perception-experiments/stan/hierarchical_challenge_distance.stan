data {
  int<lower=1> N;
  int<lower=1> P;
  int<lower=1> R;
  vector[N] adverse_margin;
  array[N] int<lower=1, upper=P> player;
  array[P] int<lower=1, upper=R> role_of_player;
}

parameters {
  vector[R] mu_role;
  vector[R] log_sigma_role;
  vector<lower=0>[R] tau_mu_role;
  vector<lower=0>[R] tau_log_sigma_role;
  vector[P] z_mu_player;
  vector[P] z_log_sigma_player;
}

transformed parameters {
  vector[P] mu_player;
  vector<lower=0>[P] sigma_player;

  for (p in 1:P) {
    int r = role_of_player[p];
    mu_player[p] = mu_role[r] + tau_mu_role[r] * z_mu_player[p];
    sigma_player[p] = exp(
      log_sigma_role[r] + tau_log_sigma_role[r] * z_log_sigma_player[p]
    );
  }
}

model {
  mu_role ~ normal(0, 2);
  log_sigma_role ~ normal(log(1.5), 0.6);
  tau_mu_role ~ normal(0, 1);
  tau_log_sigma_role ~ normal(0, 0.5);
  z_mu_player ~ std_normal();
  z_log_sigma_player ~ std_normal();

  adverse_margin ~ normal(mu_player[player], sigma_player[player]);
}
