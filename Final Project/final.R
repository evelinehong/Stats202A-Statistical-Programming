install.packages(data.table)
library(data.table) # allows us to use function fread

sample_uniform <- function(low=0, high=1){
	random = rep(0,10000)
	randomy = rep (0,10000)
	  
	m = 2 ** 31 - 1
	a = 7 ** 5
	c = 12345

	  # Set the seed using the current system time in microseconds
	d = as.numeric(Sys.time()) * 1000
	  
	for (i in 1:10000) {
	    d = (a * d + c) %% m
	    random[i] = d / m * (high - low) + low
	}

	for (i in 1:10000) {
		randomy[i] = random[i+1]
	}
	
	hist (random, main="the Histogram of X_t", xlab="X_t")
	plot (random, randomy, main="Scatterplot of (X_t, X_{t+1})", xlab="X_t", ylab="X_{t+1}")
}

###############################################
## Function 2: (2b) Exponential				 ##
###############################################

sample_exponential <- function(k=1){
	randomu = rep(0,10000)
	randomx = rep(0,10000)
	r = rep(0,10000)
	  
	m = 2 ** 31 - 1
	a = 7 ** 5
	c = 12345

	  # Set the seed using the current system time in microseconds
	d = as.numeric(Sys.time()) * 1000
	  
	for (i in 1:10000) {
	    d = (a * d + c) %% m
	    randomu[i] = d / m
	    randomx[i] = - (log(randomu[i])) / k
	}

	hist (randomx, main="Histogram of X_t", xlab="X_t")

}
        
###############################################
## Function 3: (2c) Normal	 				 ##
###############################################

sample_normal <- function(mean=0, var=1){
	d1 = as.numeric(Sys.time()) * 1000
	d2 = as.numeric(Sys.time()) * 500


	randomu1 = rep(0,10000)
	randomu2 = rep(0,10000)
	theta = rep(0,10000)
	t = rep(0,10000)
	r = rep(0,10000)
	x = rep(0,10000)
	y = rep(0,10000)

	m = 2 ** 31 - 1
	a = 7 ** 5
	c = 12345

	for (i in 1:10000) {
	    d1 = (a * d1 + c) %% m
	    d2 = (a * d2 + c) %% m
	    randomu1[i] = d1 / m
	    randomu2[i] = d2 / m
	    theta[i] = 2 * pi * randomu1[i]
	    t[i] = - log(randomu2[i])
	    r[i] = (2*var*t[i]) ^ (1/2)
	    x[i] = r[i]*cos(theta[i]) + mean
	    y[i] = r[i]*sin(theta[i]) + mean
	}

	plot (x,y,main="Scatterplot of (X_t, Y_t)", xlab="X_t", ylab="Y_t")
	hist (t, main="Histogram of T = R^2/2",xlab="T")

}

###############################################
## Function 4: (3) Monte Carlo 				 ##
###############################################

monte_carlo <- function(d=2){
	ds = vector()
	random = matrix(0,d,1000000)
	n = 0

	m = 2 ** 31 - 1
	a = 7 ** 5
	c = 12345

	for (i in 1:d){
		ds[i] = as.numeric(Sys.time()) * 500 * exp(i)
	}
	for (i in 1:1000000){
		squaresum = 0
		for (j in 1:d) {
			ds[j] = (a * ds[j] + c) %% m
			random[j,i] = ds[j]/m   #j is dimension, i is iteration
			squaresum = squaresum + random[j,i]**2 # sum of squares
		}
		if (squaresum <= 1){
			n = n + 1 
		}
	}

	volume = n * (2**d) / 1000000
	print ("volume=")
	print (volume)

	if (d == 2){
		pi1 = 4 * n / 1000000
		print ("pi=")
		print (pi1)
	}
}

###############################################
## Function 1C                  			 ##
###############################################

metropolis_simulation <- function(num_chain=1000, chain_length=100, mean=0, var=1){
	install.packages("animation")
	library(animation)
	list_a = c(0, -0.1) # Add other value as you want.
	list_b = c(1, 0.1)
	list_c = c(1, 2)
	for (a in list_a){
		for (b in list_b){
			for (c in list_c){
			x0 = sample_uniform(num_chain, a, b)
			# Run Metropolis
			X_normal = sample_normal_chain(x0, c, chain_length, mean, var)
			saveGIF(for(i in 1:chain_length) hist(X_normal[,i], main=NULL, xlab=NULL), movie.name = paste('R_Metropolis_a,b,c=',a,b,c,'.gif'),interval=0.2)
			}
		}		
	}
}

###############################################
## Function 2A                  			 ##
###############################################

gibbs_sample <- function(x0, y0, rho, num_chain=1000, chain_length=100, mean=0, var=1){
	X = array(0,dim=c(num_chain, chain_length, 2))
	random_normal = rnorm(num_chain * chain_length * 2, mean = mean, sd = var)
	X[,1,1] = x0
	X[,1,2] = y0
	n = 1
	for (i in 1: num_chain){
		for (j in 2: chain_length){
			X[i,j,1] = rho * X[i,j-1,2] + (1-rho**2)**(1/2)*random_normal[n]
			X[i,j,2] = rho * X[i,j,1] + (1-rho**2)**(1/2)*random_normal[n+1]
			n = n + 2
		}
	}
    return(X)
}

# ###############################################
# ## Function 2B                  			 ##
# ###############################################

gibbs_simulation <- function(){
	install.packages("animation")
	library(animation)
	list_rho = c(0, 0.5)
	for (rho in list_rho){
		X_gibbs = gibbs_sample(1, 1, rho)
		saveGIF(for(i in 1:100) plot(X_gibbs[,i,1],X_gibbs[,i,2]), movie.name = paste('R_Gibbs_1000_Chain_rho=',rho,'.gif'),interval=0.2)
	}
	X_gibbs2 = gibbs_sample(1, 1, 0.5)
	plot (X_gibbs2[,50:100,1], X_gibbs2[,50:100,2], main = "discard the first 50 steps, the scatterplot of the footsteps for the rest of the steps", xlab = "X[50:100]", ylab="Y[50:100]")

}

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

mySVM=function(X,Y,Xtest,Ytest,kernel='linear',method='gradient',num_iterations=75,learning_rate=1e-1,lamda=0.01)
{ X = X[1:2000,]
  Y = Y[1:2000]
  Xtest = Xtest[1:500,]
  Ytest = Ytest[1:500]
  training_acc = rep(0,num_iterations)

  n=dim(X)[1]
  p=dim(X)[2]+1
  X1 = cbind(rep(1, n), X)#intercept is included 

  Y=2*Y-1# y=1--->1 y=0--->-1

  ntest = dim(Xtest)[1]
  Xtest1 = cbind(rep(1, ntest), Xtest)
  Ytest=2*Ytest-1
  score_test = matrix(rep(0,ntest),nrow=ntest)

  K = matrix(0,n,n)
  alpha = matrix(rep(0,n),nrow=n)
 for (i in 1:n) {
  for (j in 1:(n)) {
    if (kernel == 'polynomial'){
      K[i,j] = (X1[i,]%*%c(X1[j,]))**2
    }
    else if (kernel == 'sigmoid'){
      K[i,j] = tanh(0.0000001*(X1[i,]%*%c(X1[j,])))
    }
    else if(kernel == 'radial'){
      K[i,j] = exp(-0.0000005*(X1[i,]-X1[j,])%*%c(X1[i,]-X1[j,]))
    }
    else {
      K[i,j] = (X1[i,]%*%c(X1[j,]))
    }
  }
}

  K2 = matrix(0,n,ntest)
  for (i in 1:n) {
    for (j in 1:ntest) {
      if (kernel == 'polynomial'){
        K2[i,j] = (X1[i,]%*%c(Xtest1[j,]))**2
      }
      else if (kernel == 'sigmoid'){
        K2[i,j] = tanh(0.0000001*(X1[i,]%*%c(Xtest1[j,])))
      }
      else if(kernel == 'radial'){
        K2[i,j] = exp(-0.0000005*(X1[i,]-Xtest1[j,])%*%c(X1[i,]-Xtest1[j,]))
      }
      else {
        K2[i,j] = (X1[i,]%*%c(Xtest1[j,]))
      }
    }
  }

  if (method=='gradient'){

    for(it in 1:num_iterations){
      score = matrix(rep(0,n),nrow=n)
      for (i in 1:n){
        for (j in 1:n){
          score[j] = score[j] + alpha[i]*Y[i]*K[i,j]
        }
      }

    dalpha = matrix(rep(0,n),nrow=n)

    for (i in 1:n){
      for (j in 1:n){
        if (score[j]*Y[j]<1){
          dalpha[i] = dalpha[i] + Y[j]*Y[i]*K[i,j]
        }
      }
    }

    dalpha = dalpha/n

    alpha=alpha-lamda*alpha
    alpha=alpha+learning_rate*dalpha
    
    for (i in 1:n){
        for (j in 1:n){
          score[j] = score[j] + alpha[i]*Y[i]*K[i,j]
        }
    }

    predict_trainning_temp=sign(score)
    train_acc_temp=100*mean(predict_trainning_temp*Y>0)
    training_acc[it] = train_acc_temp
    #cat(it,"traning accuracy: ",train_acc_temp,"%\n") 
  }

  for (j in 1:ntest){
      for (i in 1:n){
        score_test[j] = score_test[j] + alpha[i]*Y[i]*K2[i,j]
      }
  }
}
                                                                                                                                            
  if (method=='dual'){
    for (it in 1:num_iterations){
      score = matrix(rep(0,n),nrow=n)
      is = sample(1:n, 3, replace=TRUE)
      for (i in is){
        sum = 0
        for (j in 1:n){
          if (i!=j) {
            sum = sum + alpha[j]*Y[i]*Y[j]*K[i,j]
          }
        }
        alpha[i] = max((1-sum)/(Y[i]*Y[i]*K[i,i]),0)        
      }
      for (i in 1:n){
        for (j in 1:n){
          score[j] = score[j] + alpha[i]*Y[i]*K[i,j]
        }
      }

    predict_trainning_temp=sign(score)
    train_acc_temp=100*mean(predict_trainning_temp*Y>0)
    training_acc[it] = train_acc_temp
    #cat(it,"traning accuracy: ",train_acc_temp,"%\n")
    }
    for (j in 1:ntest){
        for (i in 1:n){
          score_test[j] = score_test[j] + alpha[i]*Y[i]*K2[i,j]
        }
    }
  }


  predict_trainning_final=sign(score_test)
  test_acc=100*mean(predict_trainning_final*Ytest>0)
  #plot (training_acc,type="l",main=paste("training accuracy",kernel,method),xlabel="iteration",ylabel="accuracy")
  #return(beta)
  cat("Final traning accuracy: ",train_acc_temp,"%","\nFinal testing accuracy: ",test_acc,"%")
  return (list(training_acc, test_acc)) 
}
