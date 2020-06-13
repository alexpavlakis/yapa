data {
  int n_states;
  int n_options;
  vector[n_options] d1[n_states];
  vector[n_options] d2[n_states];
  vector[n_options] d3[n_states];
  vector[n_options] d4[n_states];
  //vector[n_options] d5[n_states];
}
parameters {
  vector[n_options] mu;
  corr_matrix[n_options] Omega;
  vector<lower = 0>[n_options] tau;
  matrix<lower = 0>[n_states, n_options] sigma;
  
}
transformed parameters {
  matrix[n_options, n_options] Sigma;
  Sigma = quad_form_diag(Omega, tau);
}
model {
  Omega ~ lkj_corr(0.9);
  tau ~ normal(0, 0.1);
  mu ~ normal(0, 0.1);
  d1 ~ multi_normal(mu, Sigma);
  d2 ~ multi_normal(mu, Sigma);
  d3 ~ multi_normal(mu, Sigma);
  d4 ~ multi_normal(mu, Sigma);
  //d5 ~ multi_normal(mu, Sigma);
  for(o in 1:n_options) {
    for(s in 1:n_states) {
      d1[s][o] ~ normal(mu[o], sigma[s, o]);
      d2[s][o] ~ normal(mu[o], sigma[s, o]);
      d3[s][o] ~ normal(mu[o], sigma[s, o]);
      d4[s][o] ~ normal(mu[o], sigma[s, o]);
      //d5[s][o] ~ normal(mu[o], sigma[s, o]);
      sigma[s, o] ~ normal(0, 0.1);
    }
  }
}
