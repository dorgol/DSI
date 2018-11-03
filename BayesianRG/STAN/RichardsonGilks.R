setwd("/Users/janecarlen/Documents/DSI/BayesianRG/STAN")

library(MASS)
library(rstan)
library(boot)

Sys.setenv(USE_CXX14 = 1)
rstan_options(auto_write = T)

#Use true values to generate data that would have been observed
set.seed(1)
N = 10000
RRx = 2.46
RRc = 3.32
BR = 0.45
B0 = log(0.45) # -0.8
B1 = log(2.45) # 0.89
B2 = log(3.32) # 1.19
psi2 = .4
phi2 = .8
theta2 = 5.0
theta1 = 0.9
set.seed(1)
X = rnorm(N, mean = c(.5), sd = sqrt(.96))
set.seed(1)
C = rnorm(N, mean = c(-.5), sd = sqrt(1.02))
XC = cbind(X,C)
set.seed(1)
Y = rbinom(N, 1, prob = inv.logit(B0 + XC%*%c(B1,B2)))
Z1 = cbind(rnorm(X, 1/theta1), rnorm(X, 1/theta1))
Z2 = rnorm(X*psi2 + phi2, 1/theta2)
mu = c(.5,-.5)
Sigma = matrix(c(1.02, .555, .555, .96), 2, 2)
psi2 = 0.4
phi2 = 0.8

stanc("RichardsonGilks.stan")

fit <- stan(file = "RichardsonGilks.stan", model_name = "RG2")
fit = extract(fit)
lapply(fit, mean)

 # test ####
schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(file = '8schools.stan', data = schools_dat, 
            iter = 1000, chains = 4)
