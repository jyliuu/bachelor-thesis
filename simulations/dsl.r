source('learners.r')
source('loss.r')
source('crossvalidation.r')

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
dSL <- function (candidates, dataset, k=5, loss_fun=MSE) {
    cv_errors <- cross_validate_multiple(candidates, dataset, k=k)
    avg_cv_errors <- rowMeans(cv_errors)
    return(list(cv_errors, avg_cv_errors, which.min(avg_cv_errors))) 
}


# Insane currying 
fit_dSL_with_candidates <- function(candidates, k=10) function(dat) {
    cv_res <- dSL(candidates, dat, k=k)
    selected <- candidates[[ cv_res[[3]] ]] # return the candidate with lowest CV error
    selected_fitted <- selected[[1]](dat)
    list(fitted_mod = selected_fitted, predict_fun = selected[[2]])
}


# Candidates including dSL
fit_dSL <- fit_dSL_with_candidates(candidates)
predict_dSL <- function(sl, dat) sl$predict_fun(sl$fitted_mod, dat)
candidates_with_dSL <- c(candidates, list(dSL = c(fit_dSL, predict_dSL)))

