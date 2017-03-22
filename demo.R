
######## DEMO ######## 
# applying the GLASSO algorithm to a panel of financial returns 

# Load panel of financial data


# Construct a covariance matrix with data to test -----------------------------------
M <- as.matrix(returns)

k <- ncol(M) # number of variables
n <- nrow(M) # number of subjects

# Create means for each column
M_mean <- matrix(data=1, nrow=n) %*% cbind(mean(amzn),mean(ba),mean(ibm),mean(orcl),mean(aapl), mean(nke)) 

# Create a difference matrix
D <- M - M_mean

# Create the covariance matrix
Y <- (n-1)^-1*t(D) %*% D

# Check Y using cov function for comparison 
Y2 <- cov(M) 

# Run GLASSO -----------------------------------
print(Theta_ident <- GLASSO(Y))