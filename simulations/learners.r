library(xgboost)

## Fit functions
fit_logreg <- function(dat) glm(y ~ Age + Parasites, family=binomial(), data=dat)
fit_logreg_intercept <- function(dat) glm(y ~ 1, family=binomial(), data=dat)
fit_logreg_true <- function(dat) glm(y ~ Age + Parasites + Age*Parasites, family=binomial(), data=dat)
fit_xgboost <- function(dat) xgboost(data = as.matrix(dat[c('Age', 'Parasites')]), 
                     verbose = 0,
                     label = dat$y,
                     max.depth = 3,
                     eta = 0.3,
                     nthread = 5,
                     nrounds = 50, 
                     objective = 'binary:logistic',
) 


## Predict functions
predict_logreg <- function(mod, dat) predict(mod, newdata=dat, type='response')
predict_logreg_reverse <- function(mod, dat) 1-predict(mod, newdata=dat, type='response')
predict_xgboost <- function(bst, dat) predict(bst, as.matrix(dat[c('Age', 'Parasites')]))


# Library of candidate learners
candidatesLogReg <- list(logReg = c(fit_logreg, predict_logreg), 
                   logRegIntercept = c(fit_logreg_intercept, predict_logreg)
#                   logRegTrue = c(fit_logreg_true, predict_logreg)
)

candidatesTree <- list(xgboost = c(fit_xgboost, predict_xgboost))
candidates <- c(candidatesLogReg, candidatesTree)
candidatesWithTrue <- c(candidatesLogReg, candidatesTree, list(logRegTrue = c(fit_logreg_true, predict_logreg)))

