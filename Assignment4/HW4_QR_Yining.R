#########################################################
## Stat 202A - Homework 4
## Author:
## Date :
## Description: This script implements QR decomposition,
## linear regression, and eigen decomposition / PCA 
## based on QR.
#########################################################

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

##################################
## Function 1: QR decomposition ##
##################################

myQR <- function(A){
  
  ## Perform QR decomposition on the matrix A
  ## Input: 
  ## A, an n x m matrix
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################  
  
  n = dim(A)[1]
  m = dim(A)[2]
  
  R = A
  Q = diag(n)
  
  for (k in 1 : (m-1)){
    x = matrix(0,n,1)
    x[k:n,1] = R[k:n, k]
    v = x
    v[k] = x[k] + sign(x[k,1])*norm(x, type = "F")
    s = norm(v, "F")
    
    if(s != 0){
      u = v/s
      R = R - 2*(u %*% (t(u) %*% R))
      Q = Q - 2*(u %*% (t(u) %*% Q))
    }
  }
  ## Function should output a list with Q.transpose and R
  ## Q is an orthogonal n x n matrix
  ## R is an upper triangular n x m matrix
  ## Q and R satisfy the equation: A = Q %*% R
  return(list("Q" = t(Q), "R" = R))
  
}

###############################################
## Function 2: Linear regression based on QR ##
###############################################

myLinearRegression <- function(X, Y){
  
  ## Perform the linear regression of Y on X
  ## Input: 
  ## X is an n x p matrix of explanatory variables
  ## Y is an n dimensional vector of responses
  ## Do NOT simulate data in this function. n and p
  ## should be determined by X.
  ## Use myQR inside of this function
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################  
  
  n = dim(X)[1]
  p = dim(X)[2]

  Z = cbind(rep(1,n), X, Y)

  R = myQR(Z)$R

  R1 = R[1:(p + 1), 1:(p + 1)]
  Y1 = R[1:(p + 1), p + 2]
  error_list = matrix(0, (n - p - 1), 1)
  error_list[,1] = R[(p + 2):n, p + 2]
  error = norm(error_list, type = "F")

  beta_hat = solve(R1) %*% Y1

  ## Function returns the 1 x (p + 1) vector beta_ls, 
  ## the least squares solution vector
  return(list(beta_hat=beta_hat, error=error))
  
}

testing <- function(){
  
  ## This function is not graded; you can use it to 
  ## test out the 'myLinearRegression' function 

  # Define parameters
  n    <- 100
  p    <- 3
  
  ## Simulate data from our assumed model.
  ## We can assume that the true intercept is 0
  X    <- matrix(rnorm(n * p), nrow = n)
  beta <- matrix(1:p, nrow = p)
  Y    <- X %*% beta + rnorm(n)
  
  ## Save R's linear regression coefficients
  R_coef  <- coef(lm(Y ~ X))
  print(R_coef)
  
  ## Save our linear regression coefficients
  my_coef <- myLinearRegression(X, Y)$beta_hat
  print(my_coef)
  
  ## Are these two vectors different?
  sum_square_diff <- sum((R_coef - my_coef)^2)
  if(sum_square_diff <= 0.001){
    return('Both results are identical')
  }else{
    return('There seems to be a problem...')
  }
  
}
