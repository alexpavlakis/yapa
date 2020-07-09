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
  int N;                  // number of state polls
  int N_natl;             // number of national polls
  int n_states;           // number of states 
  int n_options;          // number of candidates
  int state_id[N];        // state identifier
  matrix[N, n_options] y; // matrix of counts for each candidate in each state poll
  matrix[N_natl, n_options] y_natl; 
  vector[N] days_out;     // days until election (for weighting)
  vector[N_natl] days_out_natl;     
  matrix[n_states, n_options] p_lean; // prior partisan lean in each state
  real<lower = 0> decay_param;  // parameter for exponential decay weighting
  vector[n_options] mu_swing;   // average state poll miss
  matrix[n_options, n_options] sigma_swing; // covariance matrix for state poll miss
  matrix[n_states, n_options] sd_swing_state; // variance in miss for each state
  vector[n_options] mu_swing_ge; // average national poll miss
  matrix[n_options, n_options] sigma_swing_ge; // covariance matrix for national poll miss
}
transformed data {
  // Weight each poll by recency, according to exp decay model
  int<lower = 0> y_wt[N, n_options]; // wieghted counts in each poll
  int<lower = 0> y_wt_natl[N_natl, n_options]; 
  int n_wt[N];                       // weighted sample
  int n_wt_natl[N];                       
  for(i in 1:N) {
    for(o in 1:n_options) {
      y_wt[i, o] = real_to_int(exp(-days_out[i]/decay_param)*(y[i, o]));
    }
    n_wt[i] = sum(y_wt[i, ]);
  } 
  for(i in 1:N_natl) {
    for(o in 1:n_options) {
      y_wt_natl[i, o] = real_to_int(exp(-days_out_natl[i]/decay_param)*(y_wt_natl[i, o]));
    }
    n_wt_natl[i] = sum(y_wt_natl[i, ]);
  } 
}
parameters {
  simplex[n_options] theta[n_states]; // simplex of simulated poll averages
  simplex[n_options] theta_natl;       
}
model {
  // Aggregate weighted polls for state-candidate poll distributions
  for(o in 1:n_options) {
    for(i in 1:N_natl) {
      y_wt_natl[i, o] ~ binomial(n_wt_natl[i], theta_natl[o]);
    }
    for(i in 1:N) {
      y_wt[i, o] ~ binomial(n_wt[i], theta[state_id[i], o]);  // binomial model
    }
  }
  // Prior is poll-adjusted previous election results. 
  for(o in 1:n_options) {
    for(s in 1:n_states) {
      theta[s][o] ~ normal(theta_natl[o] + p_lean[s, o], 0.1);
    }
  }
  theta_natl[1] ~ normal(0.46, 0.1);
  theta_natl[2] ~ normal(0.48, 0.1);
  theta_natl[3] ~ normal(0.06, 0.1);
}
generated quantities {
  // Simulate election results from poll averages by:
  // - first, sampling a national swing based on historical bias from poll averages;
  // - second, sampling state-level swings based on historical variance in state poll bias;
  // - third, adding simulated swing to modeled poll averages.
  matrix[n_states, n_options] mu; // simulated election outcome
  vector[n_options] mu_natl;      // simulated natl election outcome
  vector[n_options] natl_swing;   // simulated national swing
  vector[n_options] natl_swing_ge;   // simulated national swing
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
  natl_swing_ge = multi_normal_rng(mu_swing_ge, sigma_swing_ge); 
  for(o in 1:n_options) {
    mu_natl[o] = theta_natl[o] + natl_swing[o];
    if(mu_natl[o] < 0.01) mu_natl[o] = 0.01;
    if(mu_natl[o] > 0.99) mu_natl[o] = 0.99;
  }
}
