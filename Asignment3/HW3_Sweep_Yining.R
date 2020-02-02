############################################################# 
## Stat 202A - Homework 3
## Author: 
## Date : 
## Description: This script implements linear regression 
## using the sweep operator
#############################################################

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
##
## Do not use the following functions for this assignment,
## except when debugging or in the optional examples section:
## 1) lm()
## 2) solve()
#############################################################

 
################################
## Function 1: Sweep operator ##
################################

mySweep <- function(A, k){
  
  # Perform a SWEEP operation on A with the pivot element A[k,k].
  # 
  # A: a square matrix.
  # m: the pivot element is A[k, k].
  # Returns a swept matrix B (which is k by k).
  
  #############################################
  ## FILL IN THE BODY OF THIS FUNCTION BELOW ##
  #############################################
  
    n <- dim(A)[1]
    for (m in 1:k){
      for (i in 1:n){
        for (j in 1:n){
          if (i!=m & j!=m){
            A[i,j] = A[i,j] - (A[i,m]%*%A[m,j]/A[m,m])
          }
        }
      }

      for (i in 1:n){
        if (i!=m) {
          A[m,i] = A[m,i]/A[m,m]
          A[i,m] = A[i,m]/A[m,m]
        }
      }

      A[m,m] = - 1/A[m,m]

    }


  ## The function outputs the matrix B
  return(A) 
}


############################################################
## Function 2: Linear regression using the sweep operator ##
############################################################

myLinearRegression <- function(X, Y){
  
  # Find the regression coefficient estimates beta_hat
  # corresponding to the model Y = X * beta + epsilon
  # Your code must use the sweep operator you coded above.
  # Note: we do not know what beta is. We are only 
  # given a matrix X and a vector Y and we must come 
  # up with an estimate beta_hat.
  # 
  # X: an 'n row' by 'p column' matrix of input variables.
  # Y: an n-dimensional vector of responses

  #############################################
  ## FILL IN THE BODY OF THIS FUNCTION BELOW ##
  #############################################
    library(Rcpp)
    sourceCpp("HW3_Sweep.cpp")
    append = matrix(1, nrow = nrow(X))
    X = cbind(append,X)
    A = rbind (cbind (t(X)%*%X, t(X)%*%Y), cbind(t(Y)%*%X, t(Y)%*%Y)) 
    n <- nrow(X)
    p <- ncol(X)
    B =  mySweep(A, p)
    #B =  mySweepC(A, p)
    c = c(1:p)
    beta_hat = B[c,p+1]
  ## Function returns the (p+1)-dimensional vector 
  ## beta_hat of regression coefficient estimates
  return(beta_hat)
  
  
}

########################################################
## Optional examples (comment out before submitting!) ##
########################################################

testing_Linear_Regression <- function(){
  ## This function is not graded; you can use it to 
  ## test out the 'myLinearRegression' function 

  # Define parameters
  n <- 100
  p <- 3
  
  ## Simulate data from our assumed model.
  ## We can assume that the true intercept is 0
  X    <- matrix(rnorm(n * p), nrow = n)
  beta <- matrix(1:p, nrow = p)
  Y    <- X %*% beta + rnorm(n)
  
  ## Save R's linear regression coefficients
  R_coef  <- coef(lm(Y ~ X))
  # c = c(2:(p+1))
  # R_coef = R_coef[c]
  ## Save our linear regression coefficients
  my_coef <- myLinearRegression(X, Y)
  print ("result from built-in lm:")
  print (R_coef)
  print ("result by R")
  #print ("result by Cpp")
  print (my_coef)
  ## Are these two vectors different?
  sum_square_diff <- sum((R_coef - my_coef)^2)
  if(sum_square_diff <= 0.001){
    return('Both results are identical')
  }else{
    return('There seems to be a problem...')
  }
  
}

testing_Linear_Regression()
