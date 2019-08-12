x <- c(0.24, 0.34, 0.23, 0.17, 
	0.18, 0.14, 0.27, 0.08, 
	0.20, 0.10, 0.02, 0.22, 
	0.04, 0.02, 0.03, 0.17, 
	0.08, 0.18, 0.01)

axis <- seq(0, 0.35, length = 1000)

exponential <- function(x, lambda){
	return(lambda*exp(-lambda*x))
}

pdf("ex6a.pdf")

hist(x, breaks=10, main="")
lines(axis, exponential(axis, 1), lty = "dotted", lwd = 4)
lines(axis, exponential(axis, 10), lty = "longdash", lwd = 4)
lines(axis, exponential(axis, 1/mean(x)), lwd = 4)

legend(x= 0.225, y = 5, 
	legend=c(expression(paste(theta, " = 1")), 
		expression(paste(theta, " = 10")), 
		expression(paste(theta, " = ",frac(1, bar(X))))), 
	lty = c("dotted", "longdash", "solid"),
	bty = "n")



like <- function(theta){
	product = 1
	n <- length(x)
	for(i in x){
		product = product * exp(-theta*i)
	}
	return(theta^n*product)
}

pdf("ex6b.pdf")

axis2 <- seq(0, 20, length = 1000)

plot(axis2, like(axis2), type = "l", 
	xlab=expression(theta), ylab=expression(likelihood))

abline(v = 1/mean(x))
print(1/mean(x))