library(caret)
library(foreach)
library(tidyverse)

# CV method
# Returns losses on k splits
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

# Multiple CV
# Cross validate multiple return losses in matrix
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


cross_validate_multiple_lvl1 <- function(models_fit_predict, dataset, k=5, loss_fun=logloss) {
    folds <- createFolds(dataset$y, k = k, list = TRUE)     
    p <- length(models_fit_predict)

    res<- foreach (i = 1:k, .combine = 'rbind') %do% {
        test_set <- dataset[unlist(folds[i]), ]
        train_set <- dataset[unlist(folds[-i]), ]
        
        preds <- foreach (j = 1:p, .combine = 'cbind') %do% { 
            model_fun <- models_fit_predict[[j]][[1]]
            predict_fun <- models_fit_predict[[j]][[2]]
            fit <- model_fun(train_set)
            out_of_split_preds <- predict_fun(fit, test_set)

            cbind(out_of_split_preds)
        }

        cbind(Y = test_set$y, preds)
    }
    res
}

