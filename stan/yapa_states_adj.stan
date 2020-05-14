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
  matrix[n_states, n_options] bias;
  matrix[n_states, n_options] sd_bias;
}
transformed data {
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
  //simplex[n_options] mu[n_states];    // simplex of simulated election proportions
}
model {
  for(o in 1:n_options) {
    for(i in 1:N) {
      y_wt[i, o] ~ binomial(n_wt[i], theta[state_id[i], o]);  // binomial model
    }
  }
  for(o in 1:n_options) {
    for(s in 1:n_states) {
      //theta[s][o] ~ student_t(3, mu[s][o] - bias[s, o], sd_bias[s, o]);
      //mu[s][o] ~ student_t(1, priors[s, o], 0.06);
      theta[s][o] ~ student_t(3, priors[s, o], 0.06);
    }
  }
}
generated quantities {
  matrix[n_states, n_options] mu;
  matrix[n_states, n_options] error;
  for(o in 1:n_options) {
    error[, o] = multi_normal_rng(bias[, o], sd_bias[, o]);
    for(s in 1:n_states) {
      mu[s, o] = theta[s][o] + error[s, o];
    }
  }
}
