data {
  int<lower=1> N;
  int<lower=1> U;
  int<lower=1> C;
  int<lower=1> E;
  int<lower=1> R;
  int<lower=0> K;
  array[N] int<lower=0, upper=1> called_strike;
  array[N] int<lower=1, upper=U> umpire;
  array[N] int<lower=1, upper=C> catcher;
  matrix[N, E] edge_basis;
  matrix[N, R] residual_basis;
  matrix[N, K] X;
}

parameters {
  real intercept;
  vector[E] beta_edge;
  vector[R] beta_residual;
  vector[K] beta_context;

  real<lower=0> sigma_umpire;
  real<lower=0> sigma_catcher;
  vector[U] z_umpire;
  vector[C] z_catcher;
}

transformed parameters {
  vector[U] umpire_effect = sigma_umpire * z_umpire;
  vector[C] catcher_effect = sigma_catcher * z_catcher;
}

model {
  intercept ~ normal(0, 2.5);
  beta_edge ~ normal(0, 1.5);
  beta_residual ~ normal(0, 0.75);
  beta_context ~ normal(0, 0.75);
  sigma_umpire ~ normal(0, 0.5);
  sigma_catcher ~ normal(0, 0.5);
  z_umpire ~ std_normal();
  z_catcher ~ std_normal();

  called_strike ~ bernoulli_logit(
    intercept + edge_basis * beta_edge + residual_basis * beta_residual +
    X * beta_context + umpire_effect[umpire] + catcher_effect[catcher]
  );
}
