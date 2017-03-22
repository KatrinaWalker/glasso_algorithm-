GLASSO <- function(Y, lambda = 0.05, t = 0.001, max_iter = 5000, beta_tol = 0.001){
  
  # Functions ---------------------------------------------------------------
  
  # Soft thresholding
  S_func <- function(x, t){
    sign(x) * max((abs(x) - t), 0)
  }
  
  # update beta
  update_beta <- function(u, V, beta, lambda){
    for (i in seq(1, (p - 1))){
      arg <- u[j] - V[-i,i] %*% beta[-i]
      beta[i] <- S_func(arg, lambda)
    }
    return(beta)
  }

  # GLASSO ------------------------------------------------------------------
  
  # determine dimension
  p <- ncol(Y)
  
  # compute sample covariance matrix
  S <-  cov(Y)
  
  # compute tolerance
  S_temp <- S
  diag(S_temp) <- rep(0, p)
  tol <- t * mean(abs(S_temp))
  
  # initialise W & W_new for loop
  W <- S + lambda * diag(p)
  W_new <- W
  
  # intialise B, Theta
  B <- matrix(data=NA, nrow = (p - 1), ncol = p)
  Theta <- matrix(data = NA, nrow = p, ncol = p)
  
  # set intial error and iteration counter
  err <-  1
  it  <-   0
  beta_err <- 1
  # loop
  while (err > tol & it < max_iter){
    
    for (j in seq(1, p)){
      
      V <- W[-j, -j] 
      u <- S[-j, j]
      
      # initalise beta
      beta <- rep(1, (p - 1))
      
      # cyclical co-ordinate-descent
      while (beta_err > beta_tol){
        beta_new <- update_beta(u, V, beta, lambda)
        beta_err = norm(as.matrix(beta_new - beta), type = 'f')
        beta <- beta_new
      }
      
      # update w_12
      W_new[-j, j] <- V %*% beta_new
      B[,j] <- beta_new
    }
    
    # update error and iteration counter
    err <- mean(abs(W-W_new))
    it <- it + 1 
    
    # update W
    W <- W_new
  }

  # recover Theta
  for (j in seq(1, p)){
    Theta[j, j] <- 1 / (W[j, j] - W[-j, j] %*% B[, j])
    Theta[-j, j] <- -B[, j] * Theta[j, j]
  }
  
  return(Theta)
}



