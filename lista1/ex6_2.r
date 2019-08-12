x <- c(0.24, 0.34, 0.23, 0.17, 
	0.18, 0.14, 0.27, 0.08, 
	0.20, 0.10, 0.02, 0.22, 
	0.04, 0.02, 0.03, 0.17, 
	0.08, 0.18, 0.01)

axis <- seq(0, 0.35, length = 1000)

exponential <- function(x, lambda){
	return(lambda*exp(-lambda*x))
}

hist(x, breaks=10)
lines(axis, exponential(axis, 1))
lines(axis, exponential(axis, 10))
lines(axis, exponential(axis, 1/mean(x)))