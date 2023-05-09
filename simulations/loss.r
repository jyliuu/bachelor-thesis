
## Loss function
MSE <- function(y, y_hat) mean((y-y_hat)^2)
logloss <- function(y, p) mean(-(y*log(p)+(1-y)*log(1-p)))

