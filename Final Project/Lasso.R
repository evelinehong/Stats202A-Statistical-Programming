#############################################################
## Stat 202A - Homework 7
## Author:
## Date :
## Description: This script implements the lasso
#############################################################

# 1) Write R code using the included script 'Lasso.R' for computing the Lasso solution path using coordinate descent. Please include but do not penalize the intercept term (as we did for ridge regression). 

# 2) Use epsilon-boosting technique. Compare the difference.

# 2) For Lasso, plot the estimation error over the different values of lambda.

#############################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names,
## function inputs or outputs. You can add examples at the
## end of the script (in the "Optional examples" section) to
## double-check your work, but MAKE SURE TO COMMENT OUT ALL
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not use the function "setwd" anywhere
## in your code. If you do, I will be unable to grade your
## work since R will attempt to change my working directory
## to one that does not exist.
#############################################################

#####################################
## Function 1: Lasso solution path ##
#####################################

myLasso <- function(X, Y, lambda_all){

  # Find the lasso solution path for various values of
  # the regularization parameter lambda.
  #
  # X: n x p matrix of explanatory variables.
  # Y: n dimensional response vector
  # lambda_all: Vector of regularization parameters. Make sure
  # to sort lambda_all in decreasing order for efficiency.
  #
  # Returns a matrix containing the lasso solution vector
  # beta for each regularization parameter.

  #######################
  ## FILL IN CODE HERE ##
  #######################

  n = nrow(X)
  X = cbind(rep(1, n), X)
  p = ncol(X)
  S = 10
  len = length(lambda_all)
  beta_all = matrix(rep(0,len*(p)), nrow = p)

  R = Y
  beta = rep(0,p)
  SS = rep(0, p)
  for (j in 1:p)
    SS[j] = sum(X[,j]^2)
  for (l in 1:len) {
    lambda = lambda_all[l]
    for (t in 1:S)
      db = sum(R * X[,1]) / SS[1]
      b = beta[1] + db
      b = sign(b) * max(0, abs(b))
      db = b - beta[1]
      R = R - X[,1] * db
      beta[1] = b
      for (k in 2:p) {
        # R = R + X[,k] * beta[k]
        # db = sum(R * X[,k]);
        # beta[k] = sign(db) * max(0, (abs(db)-lambda)/SS[k])
        # R = R - X[,k] * beta[k]
        db = sum(R * X[,k]) / SS[k]
        b = beta[k] + db
        b = sign(b) * max(0, abs(b)-lambda/SS[k])
        db = b - beta[k]
        R = R - X[,k] * db
        beta[k] = b
      }
    beta_all[,l] = beta
  }

  ## Function should output the matrix beta_all, the
  ## solution to the lasso regression problem for all
  ## the regularization parameters.
  ## beta_all is (p+1) x length(lambda_all)
  return(beta_all)

}

myLasso_epsilon <- function(X, Y, lambda_all){
  n = nrow(X)
  X = cbind(rep(1, n), X)
  p = ncol(X)
  len = length(lambda_all)
  beta_all = matrix(rep(0,len*(p)), nrow = p)
  epsilon=0.01

  R = Y
  beta = rep(0,p)

  for (i in 1:len){
    R = Y - X %*% beta
    temp = t(R)%*%X
    j = which.max(temp)

    beta[j] = beta[j] + epsilon*sign(t(R)%*%X[,j])
    beta_all[,i]=beta
  }
  return (beta_all)
}

test <- function() {
  set.seed(10086)
  n = 50
  p = 200
  lambda_all = (100:1) * 10
  X = matrix(rnorm(n*p), nrow = n)
  beta_true = matrix(rep(0,p), nrow = p)
  beta_true[1:5] = 1:5
  Y = 1 + X %*% beta_true + rnorm(n)
  beta_all <- myLasso(X, Y, lambda_all)

  X = cbind(rep(1, n), X);
  Y_hat = X %*% beta_all;
  estimation_error = rep(0,100)
  for (i in 1:100)
    estimation_error[i] = sum((Y-Y_hat[,i])^2)
  matplot(t(matrix(rep(1,p+1),nrow=1)%*%abs(beta_all)), t(beta_all), type = 'l')

  matplot(estimation_error,type = 'l')
}

test_epsilon <-function() {
  set.seed(10086)
  n = 50
  p = 200
  lambda_all = (100:1) * 10
  X = matrix(rnorm(n*p), nrow = n)
  beta_true = matrix(rep(0,p), nrow = p)
  beta_true[1:5] = 1:5
  Y = 1 + X %*% beta_true + rnorm(n)
  beta_all <- myLasso_epsilon(X, Y, lambda_all)

  X = cbind(rep(1, n), X);
  Y_hat = X %*% beta_all;
  estimation_error = rep(0,100)
  for (i in 1:100)
    estimation_error[i] = sum((Y-Y_hat[,i])^2)
  matplot(t(matrix(rep(1,p+1),nrow=1)%*%abs(beta_all)), t(beta_all), type = 'l')

  matplot(estimation_error,type = 'l')
}
test_epsilon()

