library(numDeriv)

score <- function(theta){
  value <- c(0, 0)
  x <- c(1.19, 1.33, 1.29, 0.97, 0.57, 0.26, 1.46, 0.73, 0.45, 0.85,
                  1.67, 0.56, 0.45, 0.35, 0.52, 1.32, 1.22, 1.09, 0.27, 0.34,
                  0.59, 0.78, 0.55, 1.29, 1.11, 1.04, 1.21, 0.38, 0.61, 1.12,
                  0.72, 0.55, 0.90, 0.26, 0.90, 0.54, 0.99, 0.67, 1.36, 0.18,
                  0.58, 0.22, 1.38, 1.36, 0.35, 1.43, 0.04, 0.26, 0.86, 1.06,
                  1.47, 0.42, 0.62, 0.58, 0.65, 0.54, 0.76, 0.93, 1.15, 0.92,
                  1.95, 1.29, 0.64, 0.13, 1.70, 1.00, 0.75, 1.09, 1.40, 1.26,
                  0.87, 0.80, 0.67, 0.47, 0.66, 0.33, 0.56, 1.01, 1.54, 0.46,
                  1.39, 1.30, 1.17, 1.60, 1.16, 0.93, 1.27, 0.20, 1.17, 0.42,
                  1.53, 0.31, 1.31, 1.20, 0.75, 0.72, 1.97, 1.26, 0.48, 0.27)
  theta <- exp(theta)
  for (i in x){
    value[1] <- value[1] + (1/theta[1]) - i^theta[2]
    value[2] <- value[2] + (1/theta[2]) + log(i) - theta[1]*i^theta[2]*log(i)
  }
  return(value)
}
'''ass'''




eps = 10^(-5)
theta = c(2, 2)
error = 10

while(error > eps){
  
  thetaBefore = theta
  theta <- theta - score(theta)/grad(score, theta)
  error <- min(abs(theta - thetaBefore))
}

cat("Error = ", error, "\n")
cat("Result as expoent= ", theta, "\n")
cat("Result = ", exp(theta), "\n")

f <- function(x, theta){
  return(theta[1]*theta[2]*x^(theta[2]-1)*exp(-theta[1]*x^theta[2]))
}

hist(sample, probability = TRUE)
lines(seq(from=0, to=2, by=0.01), f(seq(from=0, to=2, by=0.01), theta), col="blue")






