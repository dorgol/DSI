data {
  int<lower=1> N; //sample size - maybe replaced later with N1 and N2
  int Y[N]; // outcome variable
  vector[N] C; // confounders
  vector[2] Z1[N]; // twice repeated measures 
  vector[N] Z2; // gold standard measures
  real mu[2];
  cov_matrix[2] Sigma;  //covariance matrices only need a single var -> 2 x 2
  real B0; // coefficients of outcome model
  real B1;
  real B2;
  real psi2;
  real phi2;
}

transformed data {
  vector[N] mux_c;
  real sigmax_c;
  for (i in 1:N)
    mux_c[i] = mu[1] + (C[i]-mu[2])*sqrt(Sigma[1, 2])*sqrt(Sigma[1, 1])/sqrt(Sigma[2, 2]);
  sigmax_c = sqrt(1 - Sigma[1, 2])*Sigma[1,1];
}

parameters {
  vector[N] X; // risk factors
  real<lower=0, upper=10> theta1; // parameters for Z1|X
  real<lower=0, upper=10> theta2; // parameters for Z2|X
  // real psi2;
  // real phi2;
}

transformed parameters {
  real<lower=0> precision1;
  real<lower=0> precision2;
  precision1 = 1/theta1;
  precision2 = 1/theta2;
}

model {
  X ~ normal(mux_c, sigmax_c); //X|C, nu, Sigma
  Z1[,1] ~ normal(X, precision1); //vectorized?
  Z1[,2] ~ normal(X, precision1); //vectorized?
  Z2 ~ normal(phi2 + X*psi2, precision2);
  Y ~ bernoulli_logit(B0 + B1*X + B2*C);
}

