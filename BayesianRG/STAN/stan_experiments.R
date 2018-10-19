
# 0. Setup ####

#following: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#how-to-use-rstan

Sys.setenv(USE_CXX14 = 1)
library("rstan") 
#"If you are using rstan locally on a multicore machine and have plenty of RAM to estimate your model in parallel":
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

rats = read.csv("https://raw.githubusercontent.com/wiki/stan-dev/rstan/rats.txt", sep = " ")

# 1. 

