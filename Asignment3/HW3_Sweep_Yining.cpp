/*
####################################################
## Stat 202A - Homework 6
## Author: 
## Date : 
## Description: This script implements QR and Sweep
####################################################
 
###########################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names, 
## function inputs or outputs. MAKE SURE TO COMMENT OUT ALL 
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not change your working directory
## anywhere inside of your code. If you do, I will be unable 
## to grade your work since R will attempt to change my 
## working directory to one that does not exist.
###########################################################
 
*/ 

# include <Rcpp.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace std;

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ 
 Sign function for later use 
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

// [[Rcpp::export()]]
double signC(double d){
  return d<0?-1:d>0? 1:0;
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ 
   Problem 1: Sweep operator 
   ~~~~~~~~~~~~~~~~~~~~~~~~~ */

// [[Rcpp::export()]]
NumericMatrix mySweepC(const NumericMatrix A, int k){
  /*
  Perform a SWEEP operation on A with the pivot element A[k,k].
  
  A: a square matrix (mat).
  m: the pivot element is A[k, k]. 
  Returns a swept matrix B (which is k by k).
  
  Note the "const" in front of mat A; this is so you
  don't accidentally change A inside your code.
  
  #############################################
  ## FILL IN THE BODY OF THIS FUNCTION BELOW ##
  #############################################
  
  */
  NumericMatrix B = clone(A);
  int n = B.nrow();
  for (int m = 0; m < k; m++){
    for (int i = 0; i < n; i++){
      for (int j = 0; j < n; j++){
        if ((i!=m) & (j!=m)){
          B(i,j) = B(i,j) - (B(i,m)*B(m,j)/B(m,m));
        }
      }
    }

    for (int i = 0; i < n; i++){
      if (i!=m ){
        B(m,i) = B(m,i)/B(m,m);
        B(i,m) = B(i,m)/B(m,m);
      }
    }

    B(m,m) = -(1/B(m,m));
  }
  // Return swept matrix B
  return(B);
  
}

