############################################################# 
## Stat 202A 2019 Fall - Homework 02
## Author: 
## Date : 
#############################################################

#############################################################
## INSTRUCTIONS: Please fill in the corresponding function. Do not change function names, 
## function inputs or outputs. Do not write anything outside the function. 
## See detailed requirement in python files.
#############################################################

###############################################
## Function 1A                  			 ##
###############################################

sample_uniform <- function(size=10000, low=0, high=1){
	X = rep(0,size)
	  
	m = 2 ** 31 - 1
	a = 7 ** 5
	c = 12345

	  # Set the seed using the current system time in microseconds
	d = as.numeric(Sys.time()) * 1000000 * runif(1)
	  
	for (i in 1:size) {
	    d = (a * d + c) %% m
	    X[i] = d / m * (high - low) + low
	}
    return(X)

}

###############################################
## Function 1B                  			 ##
###############################################

sample_normal_chain <- function(x0, c, chain_length=100, mean=0, var=1){
	X = array(0,dim=c(length(x0), chain_length))
	eps = array(0,dim=c(length(x0), chain_length))
	probs = array(0,dim=c(length(x0), chain_length))
	X[,1] = x0
	for (i in 2:chain_length){
		eps[,i] = sample_uniform(length(x0), -c, c)
		probs[,i] = sample_uniform(length(x0), 0, 1)
		y = X[,i-1] + eps[,i]
		for (j in 1:length(x0)){
			prob = exp((((X[j, i-1]-mean)**2) - ((y[j]-mean)**2))/(2*var))
			acceptance = min(1, prob)
			if (probs[j,i] < acceptance){
				X[j,i] = y[j]
			}
			else {
				X[j,i] = X[j,i-1]
			}
		}
	}
    return(X)
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

########################################################
## Optional examples (comment out before submitting!) ##
########################################################

## test()
# metropolis_simulation()
#gibbs_simulation()
