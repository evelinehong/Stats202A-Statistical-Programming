# -*- coding: utf-8 -*-
"""

 Stat 202A 2019 Fall - Homework 02
 Author: 
 Date : 

 INSTRUCTIONS: Please fill in the corresponding function. Do not change function names, 
 function inputs or outputs. Do not write anything outside the function.
 
"""

import numpy as np

### Part 1 : Metropolis Algorithm


def sample_uniform(size=10000, low=0, high=1):

	"""
	Function 1A: Uniform (Same as HW1)
	Detail : 	This function return a np.ndarray with given size.
	"""
	import random
	X = np.zeros(size)

	m = 2**31 - 1
	a = 7**5
	c = 12345

	# Set the seed using the current system time in microseconds
	import time
	d = int(time.time() * random.random())
	for i in range (0,size):
		d = (a * d + c) % m
		X[i] = d / m * (high - low) + low

	return X

def sample_normal_chain(x0, c, chain_length=100, mean=0, var=1):
	import math
	"""
	Function 1B: Normal by Metropolis
	Detail : 	This function return multiple chains by Metropolis sampling from N(mean, var).
				For every train, the proposal distribution at x is y ~ Uniform[x-c, x+c].
				The input x0 is a 1 dimension np.ndarray. 
				Return a np.ndarray X with size x0.shape[0] * chain_length. In X, each row X[i]
				should be a chain and t-th column X[:, t] corresponding to all different X_t. 
	# """
	X = np.zeros((x0.shape[0], chain_length))
	eps = np.zeros((x0.shape[0], chain_length))
	probs = np.zeros((x0.shape[0], chain_length))
	X[:, 0] = x0
	for i in range (1, chain_length):
		eps[:,i] = sample_uniform(x0.shape[0], -c, c)
		probs[:,i] = sample_uniform(x0.shape[0], 0, 1)
		y = X[:,i-1] + eps[:,i]
		for j in range (0, x0.shape[0]):
			acceptance = min(1, math.exp(((X[j][i-1]-mean)**2-(y[j]-mean)**2)/(2*var)))
			if probs[j][i] < acceptance:
				X[j][i] = y[j]
			else:
				X[j][i] = X[j][i-1]
	return X


def metropolis_simulation(num_chain=1000, chain_length=100, mean=0, var=1):
	import matplotlib.pyplot as plt
	import matplotlib.animation as animation
	"""
	Function 1C: Simulate metropolis with different setting.
	Detail : 	Try different setting and output movie of histgrams.
	"""

	list_a = [0, -0.1, -1, -10] # Add other value as you want.
	list_b = [1, 0.1, 1, 10]
	list_c = [1, 2]

	for a, b, c in [(a, b, c) for a in list_a for b in list_b for c in list_c]:

		# Sample num_chain x0 from uniform[a,b]
		x0 = sample_uniform(num_chain, a, b)

		# Run Metropolis
		X_normal = sample_normal_chain(x0, c, chain_length, mean, var)

		# Define the graph for i-th frame
		def animate(i):
		    plt.cla()
		    plt.title("Metropolis Sampling (a=%s_b=%s_c=%s) Time: %d" % (a,b,c,i))
		    plt.hist(X_normal[:,i], bins=30)

		# Define the graph for first frame
		fig = plt.figure()
		hist = plt.hist(X_normal[:,0], bins=30)

		# Define animation class and save 
		ani = animation.FuncAnimation(fig, animate, frames=chain_length, repeat_delay=3000, repeat=True)
		ani.save('Python_Metropolis_a=%s_b=%s_c=%s.gif' % (a, b, c), writer='imagemagick')

		# Plot movie and save 
		# Here plot chain_length graphs, each of them is a histogram of num_chain point. 
		# You may use matplotlib.animation and matplotlib.rc to save graphs into gif movies.


# ### Part 2 : Gibbs Sampling

def gibbs_sample(x0, y0, rho, num_chain=1000, chain_length=100, mean=0, var=1):
	"""
	Function 2A: Bivariate normal with correlation rho
	Detail : 	This function return multiple chains by Gibbs sampling
				The input x0, y0, rho, num_chain is a number. This time, we use same starting point. 
				Return a np.ndarray X with size num_chain * chain_length * 2. In X, each row X[i]
				should be a chain and t-th column X[:, t] corresponding to all different pair (X_t, Y_t). 
	"""
	X = np.zeros(((num_chain, chain_length, 2)))
	random_normal = np.random.normal(mean, var, (num_chain, chain_length, 2))
	X[:,0,0] = x0
	X[:,0,1] = y0
	for i in range (0, num_chain):
		for j in range (1, chain_length):
			X[i][j][0] = rho * X[i][j-1][1] + (1-rho**2)**(1/2)*random_normal[i][j][0]
			X[i][j][1] = rho * X[i][j][0] + (1-rho**2)**(1/2)*random_normal[i][j][1]
	return X


def gibbs_simulation():
	import matplotlib.pyplot as plt
	import matplotlib.animation as animation
	"""
	Function 2B: Simulate Gibbs with different rho and plot
	Detail : 	Try different setting and output movie of histgrams. 
				Discard first 50 steps and output 50~100 steps only.
	"""
	list_rho = [0, -1, 1, -0.5, 0.5, -0.25, 0.25, -0.75, 0.75] # Add other value as you want.
	for rho in list_rho:
		X_gibbs = gibbs_sample(1, 1, rho)
		fig = plt.figure('Gibbs_Sample_Sccaterplot_rho=%s' % rho, figsize = (6,6)).add_subplot(111)
		fig.scatter(X_gibbs[1,50:,0],X_gibbs[1,50:,1])
		plt.savefig ("Python_Gibbs Single Chain Scatterplot(rho=%s).png" % (rho))

		def animate(i):
		    plt.cla()
		    plt.title("Gibbs 1000 Chain (rho=%s) Time: %d" % (rho,i))
		    plt.scatter(X_gibbs[:,i,0],X_gibbs[:,i,1])

		# Define the graph for first frame
		fig = plt.figure()
		hist = plt.scatter(X_gibbs[:,0,0],X_gibbs[:,0,1])

		# Define animation class and save 
		ani = animation.FuncAnimation(fig, animate, frames=100, repeat_delay=3000, repeat=True)
		ani.save('Python_Gibbs_rho=%s.gif' % (rho), writer='imagemagick')
		# Run Gibbs Sampling

# def main():
# 	metropolis_simulation()
# 	gibbs_simulation()
# main()
