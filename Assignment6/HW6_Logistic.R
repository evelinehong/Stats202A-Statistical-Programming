#########################################################
## Stat 202A - Homework 6
## Author: 
## Date : 
## Description: See CCLE
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

myLM <- function(X, Y){
  
  ## Perform the linear regression of Y on X
  ## Input: 
  ## X is an n x p matrix of explanatory variables
  ## Y is an n dimensional vector of responses
  ## Use myQR inside of this function
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################  
  n = dim(X)[1]
  p = dim(X)[2]

  Z = cbind(X, Y)

  R = myQR(Z)$R

  R1 = R[1:p, 1:p]
  Y1 = R[1:p, p + 1]

  beta_ls = solve(R1) %*% Y1
  
  ## Function returns the 1 x p vector beta_ls, notice this version do not add intercept.
  ## the least squares solution vector
  return(beta_ls)
  
}

######################################
## Function 3: Logistic regression  ##
######################################

## Expit/sigmoid function
expit <- function(x){
  1 / (1 + exp(-x))
}

myLogisticSolution <- function(X, Y){

  ########################
  ## FILL IN CODE BELOW ##
  ########################
  r= dim(X)[1]
  c= dim(X)[2]
  
  Xcopy=X
  Ycopy=Y
  
  beta <- rep(0,c)
  epsilon = 10^(-6)
  
  err=10
  while(err>epsilon){
    eta <- Xcopy %*% beta
    p <- expit(eta)
    w= p *(1-p)
    z=eta+(Ycopy-p)/w
    sw=sqrt(w)
    mw=rep(sw,c)
    
    x_new=mw*Xcopy
    y_new=sw*z
    
    new_beta <- myLM(x_new, y_new)
    err=sum(abs(new_beta-beta))
    beta=new_beta
  }

  return(beta)
    
}

###################################################
## Function 4: Adaboost  ##
###################################################

myAdaboost <- function(x1, x2, y) {
  num_samples = length(x1)
  num_training <- round(num_samples*0.8)
  indices = sample(1:num_samples, size = num_samples)
  training_idx <- indices[1:num_training]
  testing_idx <- indices[-(1:num_training)]

  x1_train = x1[training_idx]
  x2_train = x2[training_idx]
  y_train = y[training_idx]
  y_test = y[testing_idx]

  num_iterations=100

  w = rep(1/num_samples, num_samples)
  s=rep(0,num_samples)

  train_error=rep(0,num_iterations)
  test_error=rep(0,num_iterations)

  for (iter in 1:num_iterations){
    wtrain = w[training_idx]
    direction = 0 #column or row
    cutpoint = 0
    t=100 #number of slices
    x1list=rep(0,t-1)
    x2list=rep(0,t-1)

    for (i in 1:(t-1)) {
      x1list[i]=sum(wtrain*(y_train != as.integer(x1_train<=(i/t))))
      x2list[i]=sum(wtrain*(y_train != as.integer(x2_train<=(i/t))))
    }

    num=which.min(c(x1list,x2list))
    error=0
    h=rep(0,num_samples)

    if(num<=(t-1)){
      cutpoint=num*(1/t)
      error = x1list[num]
      h = as.integer(x1<=(cutpoint))*2-1
    }
    else{
      cutpoint=(num-(t-1))*(1/t)
      error = x2list[num-(t-1)]
      h=as.integer(x2<=(cutpoint))*2-1
    }
    
    beta=1/2*log((1-error)/error)
    
    s=s+beta*h
    yi=y*2-1
    w=exp(-yi*s)/sum(exp(-yi*s))

    final = (s > 0)

    train_error[iter]=sum(y_train!=final[training_idx])/num_training
    test_error[iter]=sum(y_test!=final[testing_idx])/(num_samples-num_training)

  }

  plot(train_error,type = "o",col = "red",ylim = c(0,0.3),
       xlab = "epoch",ylab = "error rate",main="Adaboost training error and testing error")
  lines(test_error,type = "o",col = "green")
  legend(60, 0.2, legend=c("Training", "Testing"),
       col=c("red", "green"), lty=1:2, cex=0.8)

  plot(x1,x2,col = ifelse(final==1, "Red", ifelse(final==0, "#0073C2FF", "Black")),pch = 20,main="classification result of Adaboost")

}

# ###################################################
# ## Function 5: XGBoost  ##
# ###################################################

myXGBoost <- function(x1, x2, y) {
  num_samples = length(x1)
  num_training <- round(num_samples*0.8)
  indices = sample(1:num_samples, size = num_samples)
  training_idx <- indices[1:num_training]
  testing_idx <- indices[-(1:num_training)]

  x1_train = x1[training_idx]
  x2_train = x2[training_idx]
  y_train = y[training_idx]
  y_test = y[testing_idx]

  num_iterations=50
  w = rep(0, num_samples)
  s=rep(0,num_samples)
  train_error=rep(0,num_iterations)
  test_error=rep(0,num_iterations)

  for (iter in 1:num_iterations){
    p=exp(s)/(1+exp(s))
    w=p*(1-p)
    r=(y-p)/w

    w_train = w[training_idx]
    r_train = r[training_idx]
    
    t=100
    x1list_c1=rep(0,t-1)
    x1list_c2=rep(0,t-1)
    
    x2list_c1=rep(0,t-1)
    x2list_c2=rep(0,t-1)
    
    R_columns=rep(0,t-1)
    R_rows=rep(0,t-1)
    
    for (i in 1:(t-1)){
      x1list_c1[i]=sum(w_train*r_train*(x1_train<=(i*(1/t))))/sum(w_train*(x1_train<=(i*(1/t))))
      x1list_c2[i]=sum(w_train*r_train*(x1_train>(i*(1/t))))/sum(w_train*(x1_train>(i*(1/t))))
      
      x2list_c1[i]=sum(w_train*r_train*(x2_train<=(i*(1/t))))/sum(w_train*(x2_train<=(i*(1/t))))
      x2list_c2[i]=sum(w_train*r_train*(x2_train>(i*(1/t))))/sum(w_train*(x2_train>(i*(1/t))))
      
      x1list_c=x1list_c1[i]*(x1_train<=(i*(1/t)))+x1list_c2[i]*(x1_train>(i*(1/t)))
      R_columns[i]=sum(w_train*((r_train-x1list_c)^2))
      
      x2list_c=x2list_c1[i]*(x2_train<=(i*(1/t)))+x2list_c2[i]*(x2_train>(i*(1/t)))
      R_rows[i]=sum(w_train*((r_train-x2list_c)^2))
    }
    
    #compare R and find the best cut

    num=which.min(c(R_columns,R_rows))
    if(num<=(t-1)){
      cutpoint=num*(1/t)
      direction = 0
      c = x1list_c1[num]*(x1<=(cutpoint))+x1list_c2[num]*(x1>(cutpoint))
      s = s + c
    }
    else{
      cutpoint=(num-(t-1))*(1/t)
      direction = 1
      c = x2list_c1[num-(t-1)]*(x2<=(cutpoint))+x2list_c2[num-(t-1)]*(x2>(cutpoint))
      s = s + c
    }
    
    #running on test data
    final=(s>0)
    sum(y!=final)/length(y)
    train_error[iter]=sum(y[training_idx]!=final[training_idx])/(num_samples)
    test_error[iter]=sum(y[testing_idx]!=final[testing_idx])/(num_samples-num_training)
  }

  plot(train_error,type = "o",col = "red",ylim = c(0,0.3),
       xlab = "epoch",ylab = "error rate",main="XGboost training error and testing error")
  lines(test_error,type = "o",col = "green")
	legend(30, 0.2, legend=c("Training", "Testing"),
	   col=c("red", "green"), lty=1:2, cex=0.8)
  
  plot(x1,x2,col = ifelse(final==1, "Red", ifelse(final==0, "#0073C2FF", "Black")),pch = 20) 

}

## Simulation

test <- function() {

  Test (1)
  n <- 5000
  p <- 4
  
  X    <- matrix(rnorm(n * p), nrow = n)
  # beta <- c(12, -2,-3, 4)
  beta = rnorm(p)
  Y    <- 1 * (runif(n) < expit(X %*% beta))
  
  ## Our solution
  logistic_beta <- myLogisticSolution(X, Y)
  print (logistic_beta)    
  
  ## R's solution
  print (coef(glm(Y ~ X + 0, family = binomial(link = 'logit'))))


  Test (2, 3)

  num_sample <- 10000

  x1 <- runif(num_sample)
  x2 <- runif(num_sample)
  y <- as.integer((x1^2+x2^2 < 1))
  myAdaboost(x1, x2, y)
  myXGBoost(x1, x2, y)
  
}
test()