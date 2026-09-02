data {
  int<lower=1> N;
  int<lower=1> P;
  int<lower=0> D;
  int<lower=1> K;
  array[N] int<lower=1, upper=P> pitcher;
  matrix[N, 2] location;
  matrix[N, D] X;
  matrix[K, 2] anchor_mean;
  matrix<lower=0>[K, 2] anchor_scale;
}

parameters {
  vector[K - 1] weight_intercept;
  matrix[D, K - 1] weight_context;
  vector<lower=0>[K - 1] pitcher_scale;
  matrix[P, K - 1] z_pitcher;

  matrix[K, 2] component_mean;
  matrix<lower=0>[K, 2] component_scale;
  array[K] cholesky_factor_corr[2] component_correlation_cholesky;
}

transformed parameters {
  matrix[P, K - 1] pitcher_effect = rep_matrix(0, P, K - 1);
  vector[K] component_rho;
  if (K > 1) {
    pitcher_effect = z_pitcher * diag_matrix(pitcher_scale);
  }
  for (k in 1:K) {
    matrix[2, 2] correlation = multiply_lower_tri_self_transpose(
      component_correlation_cholesky[k]
    );
    component_rho[k] = correlation[1, 2];
  }
}

model {
  weight_intercept ~ normal(0, 1.5);
  to_vector(weight_context) ~ normal(0, 0.75);
  pitcher_scale ~ normal(0, 0.5);
  to_vector(z_pitcher) ~ std_normal();

  for (k in 1:K) {
    component_mean[k] ~ normal(anchor_mean[k], 4);
    component_scale[k] ~ lognormal(log(anchor_scale[k]), 0.4);
    component_correlation_cholesky[k] ~ lkj_corr_cholesky(4);
  }

  for (n in 1:N) {
    vector[K] log_weight = rep_vector(0, K);
    vector[K] contribution;
    if (K > 1) {
      log_weight[1:(K - 1)] = weight_intercept +
        weight_context' * X[n]' + pitcher_effect[pitcher[n]]';
    }
    log_weight = log_softmax(log_weight);
    for (k in 1:K) {
      matrix[2, 2] component_cholesky = diag_pre_multiply(
        component_scale[k]', component_correlation_cholesky[k]
      );
      contribution[k] = log_weight[k] + multi_normal_cholesky_lpdf(
        location[n]' | component_mean[k]', component_cholesky
      );
    }
    target += log_sum_exp(contribution);
  }
}
