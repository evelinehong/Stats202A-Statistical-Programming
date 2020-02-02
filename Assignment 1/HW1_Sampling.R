############################################################# 
## Stat 202A 2019 Fall - Homework 01
## Author: 
## Date : 
#############################################################

#############################################################
## INSTRUCTIONS: Please fill in the corresponding function. Do not change function names, 
## function inputs or outputs. Do not write anything outside the function. 
## Do not use any of Python's built in functions for matrix inversion or for linear modeling 
## (except for debugging or in the optional examples section).
#############################################################

###############################################
## Function 1: (2a) Uniform 				 ##
###############################################

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

########################################################
## Optional examples (comment out before submitting!) ##
########################################################

## test()

