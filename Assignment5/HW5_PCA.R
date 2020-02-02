#########################################################
## Stat 202A - Homework 5
## Author: 
## Date : 
## Description: This script implements logistic regression
## using iterated reweighted least squares using the code 
## we have written for linear regression based on QR 
## decomposition
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

##################################################
## Function 3: Eigen decomposition based on QR  ##
##################################################
myEigen_QR <- function(A, numIter = 1000) {
  
  ## Perform PCA on matrix A using your QR function, myQR or Rcpp myQRC.
  ## Input:
  ## A: Square matrix
  ## numIter: Number of iterations
  
  ########################
  ## FILL IN CODE BELOW ##
  ######################## 
  r = dim(A)[1]
  c = dim(A)[2]
  
  V = matrix(runif(r*r), nrow = r)
  
  for (i in 1 : numIter){
    op = myQR(V)
    Q = op$Q
    V  = A %*% Q
  }
  
  op = myQR(V)
  
  Q = op$Q
  R = op$R
  ## Function should output a list with D and V
  ## D is a vector of eigenvalues of A
  ## V is the matrix of eigenvectors of A (in the 
  ## same order as the eigenvalues in D.)
  
  return(list("D" = diag(R), "V" = Q))
}

###################################################
## Function 4: PCA based on Eigen decomposition  ##
###################################################
myPCA <- function(X) {
  
  ## Perform PCA on matrix A using your eigen decomposition.
  ## Input:
  ## X: Input Matrix with dimension n * p

  n1=nrow(X)
  p1=ncol(X)

  X1 = X
  for(i in 1:p1)
  {
    X1[,i]=X[,i]-mean(X[,i])
  }
  epsilon = t(X1) %*% X1 / (n1 - 1)
  pca_answer <- myEigen_QR (epsilon)
  Q = pca_answer$V
  Z = X %*% Q

  ## Output : 
  ## Q : basis matrix, p * p which is the basis system.
  ## Z : data matri x with dimension n * p based on the basis Q.
  ## It should match X = Z %*% Q.T. Please follow lecture notes.

  return(list("Q" = Q, "Z" = Z))
}


