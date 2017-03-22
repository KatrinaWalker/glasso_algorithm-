
#########################
# GLASSO test           #
#                       #
# Adam Lee, 20/03/2017  #
#########################


# Libraries ---------------------------------------------------------------

library(MASS)
library(tibble)
library(magrittr)
library(Matrix)


# Import GLASSO -----------------------------------------------------------

dir <- "C:/Users/Adam Lee/Google Drive/Education/Economics/UPF/MRes/Term 2/Financial Econometrics/Computational Project"
setwd(dir)
source('glasso.R')

# Simulate normal data to test -----------------------------------

# set num of simulated data points
n <-  50000
p <-  100

# set moments
mu <-  runif(n = p, min = 0, max = 1)
Ident <-  diag(p)

# simulate MV norm
Y <-  mvrnorm(n = n, mu = mu, Sigma = Ident) %>% as.tibble()

# Run GLASSO
Theta_ident <- GLASSO(Y)

# Needs testing on non-identity.