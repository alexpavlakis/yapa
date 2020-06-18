// This model combines polls with prior information to produce
// distributions of vote share in US presidential elections

functions {
  // Helper to convert weighted counts from real to int for binomial model
  int real_to_int(real x) {
    int out = 0;
    while(out < round(x)) {
      out += 1;
    }
    return out;
  }
}
data {
  int N;                  // number of polls
  int n_states;           // number of states 
  int n_options;          // number of candidates
  int state_id[N];        // state identifier
  matrix[N, n_options] y; // matrix of counts for each candidate in each poll
  vector[N] days_out;     // days until election (for weighting)
  matrix[n_states, n_options] p_lean; // prior rates
  real<lower = 0> decay_param;
}
transformed data {
  // Weight each poll by recency, according to exp decay model
  int<lower = 0> y_wt[N, n_options]; // wieghted counts in each poll
  int n_wt[N];                       // weighted sample
  for(i in 1:N) {
    for(o in 1:n_options) {
      y_wt[i, o] = real_to_int(exp(-days_out[i]/decay_param)*(y[i, o]));
    }
    n_wt[i] = sum(y_wt[i, ]);
  } 
}
parameters {
  simplex[n_options] theta[n_states]; // simplex of simulated poll averages
  simplex[n_options] mu;
  vector<lower = 0>[n_states] tau;
}
model {
  // Aggregate weighted polls for state-candidate poll distributions
  for(o in 1:n_options) {
    for(i in 1:N) {
      y_wt[i, o] ~ binomial(n_wt[i], theta[state_id[i], o]);  // binomial model
    }
  }
  // Prior is poll-adjusted previous election results. 
  for(o in 1:n_options) {
    for(s in 1:n_states) {
      theta[s][o] ~ normal(mu[o] + p_lean[s, o], tau[s]);
      tau[o] ~ normal(0, 0.1);
    }
  }
  mu[1] ~ normal(0.48, 0.1);
  mu[2] ~ normal(0.45, 0.1);
  mu[3] ~ normal(0.06, 0.1);
}
