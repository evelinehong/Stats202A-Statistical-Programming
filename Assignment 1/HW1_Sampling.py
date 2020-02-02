# -*- coding: utf-8 -*-
"""

 Stat 202A 2019 Fall - Homework 01
 Author: 
 Date : 

 INSTRUCTIONS: Please fill in the corresponding function. Do not change function names, 
 function inputs or outputs. Do not write anything outside the function. 
 Do not use any of Python's built in functions for matrix inversion or for linear modeling 
 (except for debugging or in the optional examples section).
 
"""

import numpy as np

###############################################
## Function 1: (2a) Uniform 				 ##
###############################################

def sample_uniform(low=0, high=1):
	random = np.zeros(10000)
	randomy = np.zeros(10000)

	m = 2**31 - 1
	a = 7**5
	c = 12345

	  # Set the seed using the current system time in microseconds
	import time
	d = int(time.time()) 
	  
	for i in range (0,10000):
		d = (a * d + c) % m
		random[i] = d / m * (high - low) + low


	for i in range (0,9999):
		randomy[i] = random[i+1]

	import matplotlib.pyplot as plt
	fig1 = plt.figure('Figure1',figsize = (6,4)).add_subplot(111)
	fig1.hist(random)
	fig2 = plt.figure('Figure2',figsize = (6,4)).add_subplot(111)
	fig2.scatter(random,randomy)
	plt.show()
	
  
###############################################
## Function 2: (2b) Exponential				 ##
###############################################

def sample_exponential(k=1):
	import math
	randomu = np.zeros(10000)
	randomx = np.zeros(10000)
	r = np.zeros(10000)
	  
	m = 2 ** 31 - 1
	a = 7 ** 5
	c = 12345

	  # Set the seed using the current system time in microseconds
	import time
	d = int(time.time()) 
	  
	for i in range(0,10000):
	    d = (a * d + c) % m
	    randomu[i] = d / m
	    randomx[i] = - (math.log(randomu[i])) / k

	import matplotlib.pyplot as plt
	plt.hist(randomx)
	plt.show()
  
###############################################
## Function 3: (2c) Normal	 				 ##
###############################################

def sample_normal(mean=0, var=1):
	import math
	import time
	d1 = int(time.time() * 500 * math.exp(2))
	d2 = int(time.time() * 500 * math.exp(1))
	randomu1 = np.zeros(10000)
	randomu2 = np.zeros(10000)
	theta = np.zeros(10000)
	t = np.zeros(10000)
	r = np.zeros(10000)
	x = np.zeros(10000)
	y = np.zeros(10000)

	m = 2 ** 31 - 1
	a = 7 ** 5
	c = 12345

	for i in range (0,10000): 
	    d1 = (a * d1 + c) % m
	    d2 = (a * d2 + c) % m
	    randomu1[i] = d1 / m
	    randomu2[i] = d2 / m
	    theta[i] = 2 * math.pi * randomu1[i]
	    t[i] = - math.log(randomu2[i])
	    r[i] = (2 * var * t[i]) ** (1/2)
	    x[i] = r[i]*math.cos(theta[i]) + mean
	    y[i] = r[i]*math.sin(theta[i]) + mean

	import matplotlib.pyplot as plt
	fig1 = plt.figure('Figure4',figsize = (6,6)).add_subplot(111)
	fig1.scatter(x,y)
	fig2 = plt.figure('Figure5',figsize = (6,4)).add_subplot(111)
	fig2.hist(t)
	plt.show()	
  
###############################################
## Function 4: (3) Monte Carlo 				 ##
###############################################

def monte_carlo(d=2):
	ds = []
	random = []
	n = 0

	m = 2 ** 31 - 1
	a = 7 ** 5
	c = 12345

	for i in range (1, d+1):
		ds.append(int(time.time() * 500 * math.exp(i)))
		random.append(np.zeros(1000000))
	for i in range (0,1000000):
		squaresum = 0
		for j in range (0, d):
			ds[j] = (a * ds[j] + c) % m
			random[j][i] = ds[j]/m   #j is dimension, i is iteration
			squaresum += random[j][i]**2 # sum of squares
		if squaresum <= 1:
			n = n + 1 
	volume = n * (2**d) / 1000000
	print ("volume="+str(volume))
	if d == 2:
		pi1 = 4 * n / 1000000
		print ("pi="+str(pi1))
	
  
########################################################
## Optional examples (comment out before submitting!) ##
########################################################

## test()

