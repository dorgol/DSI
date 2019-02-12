// more complicated version of lisa_model.stan with species-specific variances for individual effects

data {
  int S; //number of species
  int I; //number of individuals per species
  int J; //number of replications per individual
  int Y[S*I*J];
}

parameters {
  real beta[S,I]; //individual effects
  real alpha_s[S]; //species means
  real <lower = 0> sigma_s[S];
  real alpha0; //overall mean
  real <lower = 0> sigma0; //overall variance
  real <lower = 0> phi; //overdispersion - see stan manual section 52.2
}

transformed parameters {
  vector[S*I*J] lambda;
  for (s in 1:S) {
    for (i in 1:I) {
      for (j in 1:J) {
       lambda[(s-1)*I*J+(i-1)*J+j] = beta[s][i] + alpha_s[s]; // Y arranged by species then ind.
      }
    }
  }
  lambda = exp(lambda); 
}

model {
  alpha0 ~ normal(0, 5); #lowered to 5 due to unrealistic outliers with 10
  sigma0 ~ cauchy(0, 5);
  alpha_s ~ normal(alpha0, sigma0);
  sigma_s ~ cauchy(0, 10);
  for (s in 1:S) {
    beta[s] ~ normal(0, sigma_s[S]);
  }
  phi ~ cauchy(0, 5);
  // Y ~ poisson(lambda); //poisson doesn't fit well
  Y ~ neg_binomial_2(lambda, phi);
}

