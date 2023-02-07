library(tidyverse)
library(xgboost)
library(caret)
library(patchwork)

expit <- function(x){exp(x)/(1+exp(x))}
trueModel <- function(Age, Parasites) expit(-3.5-.3*Age+.85*Parasites+0.35*Age*Parasites)
simulateMalariaData <- function(N){
    Age <- runif(N,.5,15)
    Parasites <- rnorm(N,mean=3.5-0.03*Age)
    ps <- trueModel(Age, Parasites)
    Fever <- rbinom(N,1,ps)
    data.frame(Fever, y=Fever,Age,Parasites,ps)
}

N <- 10^5
simDat <- simulateMalariaData(N)

### 2d plot of the true model

ggplot(simDat, aes(x = Age, y = Parasites, color = ps)) + 
  geom_point() + 
  scale_color_gradient(limits = c(0, 1), low = "blue", high = "red") + 
  xlab("Age") + 
  ylab("Parasites") + 
  ggtitle("2D Plot of Age vs Parasites")
# Not quite what I was looking for, we can make a heatmap plot



# create a grid of values for Age and Parasites
grid <- expand.grid(Age = seq(0.5, 15, length.out = 50),
                     Parasites = seq(0, 7, length.out = 50))

# evaluate trueModel at each grid point
grid$ps <- trueModel(grid$Age, grid$Parasites)

# plot the grid
trueplot <- ggplot(grid, aes(x = Age, y = Parasites, fill = ps)) + 
  geom_tile() + 
  scale_fill_gradient(limits = c(0, 1), low = "green", high = "red") + 
  xlab("Age") + 
  ylab("Parasites") + 
  ggtitle("Heatmap of Age vs Parasites")

trueplot

# Fitting logistic regression without interaction term
modLogReg0 <- glm(Fever ~ Age + Parasites, data = simDat, family = binomial()) 
summary(modLogReg0)

# And with interactions
modLogRegTrue <- glm(Fever ~ Age + Parasites + Age*Parasites, data = simDat, family = binomial()) 
summary(modLogRegTrue)
# Estimates the true parameters correctly

grid$psLogReg0 <- predict(modLogReg0, newdata=grid, type='response')
grid$psLogRegTrue <- predict(modLogRegTrue, newdata=grid, type='response')

# modLogReg0 predictions 
misspecifiedplot <- ggplot(grid, aes(x = Age, y = Parasites, fill = psLogReg0)) + 
  geom_tile() + 
  scale_fill_gradient(limits = c(0, 1), low = "blue", high = "red") + 
  xlab("Age") + 
  ylab("Parasites") + 
  ggtitle("Predicted probabilities using misspecified logReg0")
  
truemodplot <- ggplot(grid, aes(x = Age, y = Parasites, fill = psLogRegTrue)) + 
  geom_tile() + 
  scale_fill_gradient(limits = c(0, 1), low = "blue", high = "red") + 
  xlab("Age") + 
  ylab("Parasites") + 
  ggtitle("Predicted probabilities using true identification logRegTrue")

trueplot | misspecifiedplot / truemodplot

## Fitting xgboost model

bstSparse <- xgboost(data = as.matrix(simDat[c('Age', 'Parasites')]), 
                     label = simDat$y,
                     max.depth = 3,
                     eta = 0.2,
                     nthread = 5,
                     nrounds = 100, 
                     objective = 'binary:logistic',
                     booster = 'dart'
) 

grid$xgboostps <- predict(bstSparse, as.matrix(grid[c('Age', 'Parasites')]))


xgboostmodplot <- ggplot(grid, aes(x = Age, y = Parasites, fill = xgboostps)) + 
  geom_tile() + 
  scale_fill_gradient(limits = c(0, 1), low = "blue", high = "red") + 
  xlab("Age") + 
  ylab("Parasites") + 
  ggtitle("Predicted probabilities using XGBoost")

xgboostmodplot 


## Fit functions
fit_logreg <- function(dat) glm(y ~ Age + Parasites, family=binomial(), data=dat)
fit_logreg_intercept <- function(dat) glm(y ~ 1, family=binomial(), data=dat)
fit_logreg_true <- function(dat) glm(y ~ Age + Parasites + Age*Parasites, family=binomial(), data=dat)

fit_xgboost <- function(dat) xgboost(data = as.matrix(dat[c('Age', 'Parasites')]), 
                     verbose = 0,
                     label = dat$y,
                     max.depth = 3,
                     eta = 0.2,
                     nthread = 5,
                     nrounds = 50, 
                     objective = 'binary:logistic',
) 


## Predict functions
predict_logreg <- function(mod, dat) predict(mod, newdata=dat, type='response')
predict_logreg_reverse <- function(mod, dat) 1-predict(mod, newdata=dat, type='response')

predict_xgboost <- function(bst, dat) predict(bst, as.matrix(dat[c('Age', 'Parasites')]))

## Loss function
MSE <- function(y, y_hat) mean((y-y_hat)^2)
logloss <- function(y, p) mean(-(y*log(p)+(1-y)*log(1-p)))

# CV method
cross_validate <- function(model_fun, predict_fun, dataset, k=5, loss_fun=logloss) {
    folds <- createFolds(dataset$y, k = k, list = TRUE)     
    losses <- c()
    for (i in 1:k) {
        test_set <- unlist(folds[i])
        train_set <- unlist(folds[-i])

        fit <- model_fun(dataset[train_set, ])
        out_of_split_preds <- predict_fun(fit, dataset[test_set, ])
        loss <- loss_fun(dataset[test_set, ]$y, out_of_split_preds)
        losses <- c(losses, loss) 
    }
    return(losses)
}

cross_validate(fit_logreg, predict_logreg, simDat)
cross_validate(fit_logreg_true, predict_logreg, simDat)
cross_validate(fit_logreg_true, predict_logreg_reverse, simDat)
cross_validate(fit_logreg_intercept, predict_logreg, simDat)
cross_validate(fit_xgboost, predict_xgboost, simDat)

predict(fit, newdata=simDat)
predict(bstSparse, as.matrix(grid[c('Age', 'Parasites')]))

# Multiple CV

cross_validate_multiple <- function(models_fit_predict, dataset, k=5, loss_fun=logloss) {
    folds <- createFolds(dataset$y, k = k, list = TRUE)     
    p <- length(models_fit_predict)
    losses <- matrix(0, nrow = p, ncol = k) 
    for (i in 1:k) {
        test_set <- dataset[unlist(folds[i]), ]
        train_set <- dataset[unlist(folds[-i]), ]
        
        for (j in 1:p) { 
            model_fun <- models_fit_predict[[j]][[1]]
            predict_fun <- models_fit_predict[[j]][[2]]
            fit <- model_fun(train_set)
            out_of_split_preds <- predict_fun(fit, test_set)
            loss <- loss_fun(test_set$y, out_of_split_preds)
            losses[j, i] <- loss
        }
    }
    losses
}


cvres <- cross_validate_multiple(list(c(fit_logreg_true, predict_logreg)), simDat)
cvres

## The discrete super learner
# The code below selects the learner with the lowest CV error among a list of candidate learners
# dSL returns the CV errors for the candidate learners and the index of the best learner
# For small sample sizes, we observe that the dSL usually selects the XGboost model as the favorite
# For large sample sizes, dSL will tend to select the true model (logRegTrue) 

# Question: how do we measure the performance of dSL against other learners? 
# Assuming that we have a dataset (Y_i, X_i), we would like to determine which model fits best on this data
# 1. We will run k-fold cross validation for each of our candidate learners using the same folds, splitting the dataset into k splits, obtaining the mean loss for each candidate learner
# 2. Using the same folds as above, we will also run CV for our dSL. The dSL, however, takes in as its own parameter k, which specifies the number of CV folds it should run on its candidate learners, call this v
# 2. (a) The dSL will be fitted on the data corresponding to our k-1 folds
# 2. (b) The dSL will run a v-fold cross validation for each candidate learner on the k-1 data
# 2. (c) The dSL fits each candidate learner on the (v-1)/v fraction of our k-1 data 
# 2. (d) The dSL evaluates the candidate learner on the 1/v fraction of our k-1 data 
# 2. (e) Doing this for all candidate learners the dSL will obtain the cv errors for each of the learners
# 2. (f) Pick the learner with the lowest CV error, and fit it on the k-1 data, that will be the dSL model for k-1 fold
# 3. Finally, use the dSL to predict on the left out fold 
dSL <- function (candidates, dataset, k=5, loss_fun=logloss) {
    cv_errors <- cross_validate_multiple(candidates, dataset, k=k)
    avg_cv_errors <- rowMeans(cv_errors)
    return(list(cv_errors, avg_cv_errors, which.min(avg_cv_errors))) 
}

candidatesLogReg <- list(logReg = c(fit_logreg, predict_logreg), 
                   logRegIntercept = c(fit_logreg_intercept, predict_logreg),
                   logRegTrue = c(fit_logreg_true, predict_logreg))

candidatesTree <- list(xgboost = c(fit_xgboost, predict_xgboost))

candidates <- c(candidatesLogReg, candidatesTree)
candidates


res10 <- dSL(candidates, simulateMalariaData(10), k=10)
res100 <- dSL(candidates, simulateMalariaData(100), k=100)
res1k <- dSL(candidates, simulateMalariaData(1000), k=25)
res10k <- dSL(candidates, simulateMalariaData(10000), k=10)

res10
res100
res1k
res10k


# Insane currying 
fit_dSL_with_candidates <- function(candidates) function(dat) {
    cv_res <- dSL(candidates, dat)
    selected <- candidates[[ cv_res[[3]] ]] # return the candidate with lowest CV error
    selected_fitted <- selected[[1]](dat)
    list(fitted_mod = selected_fitted, predict_fun = selected[[2]])
}

fit_dSL <- fit_dSL_with_candidates(candidates)
predict_dSL <- function(sl, dat) sl[[2]](sl[[1]], dat)

# Test this way of fitting works
dSL_mod <- fit_dSL(simDat)
dSL_mod
predict_dSL(dSL_mod, simDat)

# Cross validation of discrete super learner
candidates_with_dSL <- c(candidates, list(dSL = c(fit_dSL, predict_dSL)))
rowMeans(cross_validate_multiple(candidates_with_dSL, simulateMalariaData(100), k=100))
rowMeans(cross_validate_multiple(candidates_with_dSL, simulateMalariaData(2000), k=30))
rowMeans(cross_validate_multiple(candidates_with_dSL, simulateMalariaData(10000), k=10))

# Seems like the discrete super learner is at least as good as the best learner
