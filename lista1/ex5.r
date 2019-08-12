traceback()

x <- c(0.61, 0.73, 0.9, 0.11, 0.06, 0.03, 0.60, 0.38, 0.13, 0.1) 
y <- c(0.85, 0.97, 0.41, 0.92, 0.73, 0.66, 0.50, 0.39, 0.50, 0.63)


MLBetab <- function(x){
	n <- length(x)
	x <- log((1-x))
	return(-n/sum(x))
}

MLBetaa <- function(x){
	n <- length(x)
	x <- log(x)
	return(-n/sum(x))
}

LikeA <- function(teta){
	n <- length(y)
	product = 1
	for (i in y){
		product = product*((i)^(teta-1))
	}
	return(teta^n*product)
}

LikeB <- function(teta){
	n <- length(x)
	product = 1
	for (i in x){
		product = product*((1-i)^(teta-1))
	}
	return(teta^n*product)
}

teta <- seq(0, 5, length.out = 10000)


print(MLBetaa(y))
print(MLBetab(x))

pdf("ex5a.pdf")
plot(teta, LikeA(teta), type = "l", xlab = expression(theta), ylab = expression(paste("likelihood for Beta(", theta, " , 1)")))
abline(v = MLBetaa(y))
pdf("ex5b.pdf")
plot(teta, LikeB(teta), type = "l", xlab = expression(theta), ylab = expression(paste("likelihood for Beta(1, ", theta, ")")))
abline(v = MLBetab(x))
