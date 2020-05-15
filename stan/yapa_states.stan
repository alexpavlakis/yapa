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
  matrix<lower = 0, upper = 1>[n_states, n_options] priors; // prior rates
  real<lower = 0> decay_param;
  vector[n_options] mu_swing;
  matrix[n_options, n_options] sigma_swing;
  matrix[n_states, n_options] sd_swing_state;
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
      theta[s][o] ~ normal(priors[s, o], 0.06); 
    }
  }
}
generated quantities {
  // Simulate election results from poll averages by:
  // - first, sampling a national swing based on historical bias from poll averages;
  // - second, sampling state-level swings based on historical variance in state poll bias;
  // - third, adding simulated swing to modeled poll averages.
  matrix[n_states, n_options] mu; // simulated election outcome
  vector[n_options] natl_swing;   // simulated national swing
  matrix[n_states, n_options] epsilon; // simulated state-candidate non-sampling error
  natl_swing = multi_normal_rng(mu_swing, sigma_swing); 
  for(s in 1:n_states) {
    for(o in 1:n_options) {
      epsilon[s, o] = natl_swing[o] + normal_rng(0, sd_swing_state[s, o]);
      mu[s, o] = theta[s][o] + epsilon[s, o];
      if(mu[s, o] < 0.01) mu[s, o] = 0.01;
      if(mu[s, o] > 0.99) mu[s, o] = 0.99;
    }
  }
}

