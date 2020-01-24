#some standard and simple functions in R to calculation expectations using Monte Carlo
#n is the quantity of numbers to be generated
runif(4, min=0, max=1) 
#runif generates random deviates.
#The length of the result is determined by n for runif, and 
#is the maximum of the lengths of the numerical arguments for the other functions.
rnorm(3, mean=0, sd=1)
rbeta(n, shape1, shape2)
rgamma(n, shape, rate=1)
rpois(n, lambda)
rbinom(n, size, prob)
rnbinom(n, size, prob)

