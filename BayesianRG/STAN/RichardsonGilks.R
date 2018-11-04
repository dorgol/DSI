setwd("/Users/janecarlen/Documents/DSI/BayesianRG/STAN")

library(MASS)
library(rstan)
library(boot)

Sys.setenv(USE_CXX14 = 1)
rstan_options(auto_write = T)

#Use true values to generate data that would have been observed
N1 = 200
N2 = 1000
RRx = 2.45
RRc = 3.32
BR = 0.45
B0 = log(BR) # -0.8
B1 = log(RRx) # 0.89
B2 = log(RRc) # 1.19
theta1 = 0.9 #sd 1.11
theta2 = 5.0 #sd .2
psi2 = .4
phi2 = .8
mu = c(.5,-.5)
Sigma = matrix(c(1.02, .555, .555, .96), 2, 2)

set.seed(100)
XC = mvrnorm((N1+N2), mu, Sigma)
C = XC[,2]
X = XC[,1] ; mean(X)
X1 = X[1:N1]
X2 = X[(N1+1):(N1+N2)]
Y = rbinom((N1+N2), 1, prob = inv.logit(B0 + XC%*%c(B1,B2)))
Z1 = cbind(rnorm(N1, X1, sd=1/theta1), rnorm(N1, X1, sd=1/theta1)); mean(Z1); sd(Z1-X1)
Z2 = rnorm(N1, X1*psi2 + phi2, 1/theta2); mean(Z2); sd(Z2 - X1*psi2)
Z3 = rnorm(N2, X2, sd=1/theta1)

stanc("~/Documents/DSI/DSI_repo/BayesianRG/STAN/RichardsonGilks.stan")

fit <- stan(file = "~/Documents/DSI/DSI_repo/BayesianRG/STAN/RichardsonGilks.stan",
            model_name = "RG2", control = list(max_treedepth = 15))

print(fit, pars = c("baselinerisk", "rrx", "rrc", "theta1", "psi2", "phi2", "theta2"))
pairs(fit, pars = c("baselinerisk", "rrx", "rrc", "theta1", "psi2", "phi2", "theta2"))
fit2 = extract(fit)

 