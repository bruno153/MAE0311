
EQM1 <- function(theta, n){
	return (theta*theta*((1/(12*n))+(1/9)))
}

EQM2 <- function(theta, n){
	return (theta*theta*(((2*n)/(2*n+2))+((1-(4*(n^2)))/((2*n+1)^2))))
}

teta <- seq(0, 5, length.out = 10000)


pdf("ex1.pdf")
plot(teta, EQM1(teta, 10), type = "l", xlab = expression(theta), ylab = "EQM")
lines(teta, EQM2(teta, 10), lty = "dotted")

