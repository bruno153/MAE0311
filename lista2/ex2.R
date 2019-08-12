
EQM1 <- function(theta, n){
	return (theta^2*((4/(12*n))))
}

EQM2 <- function(theta, n){
	c = (n+1)/n
	a = c^2*((n/(n+2))-(n/(n+1))^2)
	return (theta^2*(a))
}

teta <- seq(0, 5, length.out = 10000)


pdf("ex2.pdf")
plot(teta, EQM1(teta, 10), type = "l", xlab = expression(theta), ylab = "EQM")
lines(teta, EQM2(teta, 10), lty = "dotted")

