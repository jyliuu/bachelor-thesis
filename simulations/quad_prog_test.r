library(lsei)


X <- matrix(rnorm(4000), ncol = 4)
Y <- X %*% c(0.4, 0.3, 0.2, 0.1)
lsei(a = X, b = Y, c = rep(1, ncol(X)), d = 1, lower = 0) # should return weights

Y <- X %*% c(2, 0.3, 0.2, 0.1)
lsei(a = X, b = Y, c = rep(1, ncol(X)), d = 1, lower = 0) # should weigh Y much more than others
