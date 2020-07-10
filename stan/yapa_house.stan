functions {
  // Helper to convert weighted counts from real to int for binomial model
  int real_to_int(real x) {
    int out = 0;
    while(out < floor(x)) {
      out += 1;
    }
    return out;
  }
}
data {
  int N;                  // number of polls
  int N_gb;
  int n_districts;
  int n_options;          // number of candidates
  matrix[N, n_options] y; // matrix of counts for each candidate in each poll
  matrix[N_gb, n_options] y_gb;
  vector[N] days_out;     // days until election (for weighting)
  vector[N_gb] days_out_gb;
  int district_id[N];
  matrix[n_districts, n_options] p_lean;
  matrix[n_districts, n_options] inc;
  real<lower = 0> decay_param;  // parameter for exponential decay weighting
  vector<lower = 0>[n_options] tau;
  vector<lower = 0>[n_districts] any_data;
}
transformed data {
  // Weight each poll by recency, according to exp decay model
  int<lower = 0> y_wt[N, n_options]; // wieghted counts in each poll
  int n_wt[N];                       // weighted sample
  int<lower = 0> y_wt_gb[N_gb, n_options]; // wieghted counts in each poll
  int n_wt_gb[N_gb];                       // weighted sample
  for(i in 1:N) {
    for(o in 1:n_options) {
      y_wt[i, o] = real_to_int(exp(-days_out[i]/decay_param)*(y[i, o]));
    }
    n_wt[i] = sum(y_wt[i, ]);
  } 
  for(i in 1:N_gb) {
    for(o in 1:n_options) {
      y_wt_gb[i, o] = real_to_int(exp(-days_out_gb[i]/decay_param)*(y_gb[i, o]));
    }
    n_wt_gb[i] = sum(y_wt_gb[i, ]);
  } 
}
parameters {
  simplex[n_options] theta_gb; // simplex of simulated poll averages
  simplex[n_options] theta[n_districts];
}
model {
  for(o in 1:n_options) {
    for(i in 1:N_gb) {
      y_wt_gb[i, o] ~ binomial(n_wt_gb[i], theta_gb[o]);  // binomial model
    }
    for(i in 1:N) {
      y_wt[i, o] ~ binomial(n_wt[i], theta[district_id[i]][o]);
    }
    for(d in 1:n_districts) {
      theta[d][o] ~ normal(0.91*(theta_gb[o] + p_lean[d, o]) + 0.06*inc[d, o], tau[o]);
    }
  }
  theta_gb[1] ~ beta(40, 50);
  theta_gb[2] ~ beta(60, 50);
  theta_gb[3] ~ beta(1, 50);
}
