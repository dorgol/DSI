data {
  int<lower=1> N1; //sample size for part 1
  int<lower=1> N2; //sample size for part 2
  int Y[N1+N2]; // outcome variable
  vector[N1+N2] C; // confounders
  vector[2] Z1[N1]; // Insrument 1 twice 
  vector[N1] Z2; // Instrument 2 (high precision, biased) once
  vector[N2] Z3; // Instrument 1 once
  real mu[2];
  cov_matrix[2] Sigma;  //covariance matrices only need a single var -> 2 x 2
}
// question to investigate: is there a reason to use a vector created like Y vs. like C?

transformed data {
  vector[N1+N2] mux_c;
  real sigmax_c;

  for (i in 1:(N1+N2))
    mux_c[i] = mu[1] + (C[i]-mu[2])*sqrt(Sigma[1, 2])*sqrt(Sigma[1, 1])/sqrt(Sigma[2, 2]);
  sigmax_c = sqrt(1 - Sigma[1, 2])*Sigma[1,1];
}

parameters {
  vector[N1+N2] X; // risk factors
  real<lower=0, upper=10> theta1; // parameters for Z1|X
  real<lower=0, upper=10> theta2; // parameters for Z2|X
  real psi2;
  real phi2;
  real B0; // coefficients of outcome model
  real B1;
  real B2;
}

transformed parameters {
  real<lower=0> precision1;
  real<lower=0> precision2;
  precision1 = 1/theta1;
  precision2 = 1/theta2;
  
}

model {
  X ~ normal(mux_c, sigmax_c); //X|C, nu, Sigma
  Z1[,1] ~ normal(X[1:N1], precision1); //vectorized?
  Z1[,2] ~ normal(X[1:N1], precision1); //vectorized?
  Z2 ~ normal(phi2 + X[1:N1]*psi2, precision2);
  Z3 ~ normal(X[(N1+1):(N1+N2)], precision1); //vectorized?
  Y ~ bernoulli_logit(B0 + B1*X + B2*C);
}

generated quantities{
real baselinerisk;
real rrx;
real rrc;

baselinerisk = exp(B0);
rrx = exp(B1);
rrc = exp(B2);
}


