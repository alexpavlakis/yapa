functions {
  int real_to_int(real x) {
    int out = 0;
    while(out < round(x)) {
      out += 1;
    }
    return out;
  }
}
data {
  int n_polls_g;
  int n_polls_r;
  int n_options;
  int n_regions;
  matrix<lower = 0>[n_polls_g, n_options] y_g;
  matrix<lower = 0>[n_polls_r, n_options] y_r;
  int region_id[n_polls_r];
  matrix[n_regions, n_options] lean;
  vector[n_polls_g] days_out_g;
  vector[n_polls_r] days_out_r;
  vector[n_options] non_samp_error;
  matrix[n_options, n_options] non_samp_corr;
  vector<lower = 0>[n_regions] region_error;
  int decay_param;
  vector<lower = 0, upper = 1>[n_options] prior_g;
  vector<lower = 0, upper = 1>[n_options] prior_sd_g;
}
transformed data {
  int y_wt_g[n_polls_g, n_options];
  int n_wt_g[n_polls_g];
  int y_wt_r[n_polls_r, n_options];
  int n_wt_r[n_polls_r];
  for(o in 1:n_options) {
    for(i in 1:n_polls_g) {
      y_wt_g[i, o] = real_to_int(exp(-days_out_g[i]/decay_param)*(y_g[i, o]));
      n_wt_g[i] = sum(y_wt_g[i, ]);
    }
    for(i in 1:n_polls_r) {
      y_wt_r[i, o] = real_to_int(exp(-days_out_r[i]/decay_param)*(y_r[i, o]));
      n_wt_r[i] = sum(y_wt_r[i, ]);
    }
  }
}
parameters {
  simplex[n_options] theta_g;
  simplex[n_options] theta_r[n_regions];
}
model {
  for(o in 1:n_options) {
    theta_g[o] ~ normal(prior_g[o], prior_sd_g[o]);
    for(i in 1:n_polls_g) {
      y_wt_g[i, o] ~ binomial(n_wt_g[i], theta_g[o]);
    }
    for(i in 1:n_polls_r) {
      y_wt_r[i, o] ~ binomial(n_wt_r[i], theta_r[region_id[i]][o]);
      theta_r[region_id[i]][o] ~ normal(theta_g[o] + lean[region_id[i], o], 0.1);
    }
  }
}
generated quantities {
  vector[n_options] res_g;
  matrix[n_regions, n_options] res_r;
  vector[n_options] non_samp_sim;
  matrix[n_regions, n_options] epsilon;
  non_samp_sim = multi_normal_rng(non_samp_error, non_samp_corr);
  for(o in 1:n_options) {
    for(r in 1:n_regions) {
      epsilon[r, o] = non_samp_sim[o] + region_error[r];
      res_r[r, o] = theta_r[r][o] + epsilon[r, o];
      if(res_r[r, o] < 0.01) res_r[r, o] = 0.01;
      if(res_r[r, o] > 0.99) res_r[r, o] = 0.99;
    }
    res_g[o] = theta_g[o] + non_samp_sim[o];
    if(res_g[o] < 0.01) res_g[o] = 0.01;
    if(res_g[o] > 0.99) res_g[o] = 0.99;
  }
}
