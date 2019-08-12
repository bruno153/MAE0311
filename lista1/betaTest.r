theta = 3
x <- rbeta(100000, 1, theta)
y <- rbeta(100000, theta, 1)

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

print(MLBetab(x))
print(MLBetaa(y))