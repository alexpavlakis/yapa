data {
  int n_polls;
  int n_options;
  vector[n_options] polls[n_polls];
}
parameters {
  vector[n_options] mu;
  corr_matrix[n_options] Omega;
  vector<lower = 0>[n_options] tau;
  
}
transformed parameters {
  matrix[n_options, n_options] Sigma;
  Sigma = quad_form_diag(Omega, tau);
}
model {
  Omega ~ lkj_corr(0.9);
  tau ~ normal(0, 0.1);
  mu ~ normal(0, 0.1);
  polls ~ multi_normal(mu, Sigma);
}
