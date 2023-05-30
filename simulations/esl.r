source('learners.r')
source('loss.r')
source('crossvalidation.r')

library(lsei)

# Insane currying once again
fit_eSL_with_candidates <- function(candidates, meta_learning_algorithm, k=10) function(dat) {
    cv_lvl1_and_loss <- cross_validate_multiple_lvl1(candidates, dat, k=k)

    meta_fit <- meta_learning_algorithm[[1]]
    meta_predict <- meta_learning_algorithm[[2]]

    # Fit the meta model on the level 1 data
    meta_model <- meta_fit(cv_lvl1_and_loss)

    # Candidates fitted on dat
    candidates_fitted_predict <- lapply(candidates, 
        function(cand) list(fitted_model = cand[[1]](dat), predict_fun = cand[[2]])
    )
    predict_esl <- function (meta_model, observation) {
        lvl_1_covariates <- sapply(candidates_fitted_predict, function(cand) cand$predict_fun(cand$fitted_model, observation))
        meta_predict(meta_model, lvl_1_covariates)
    }

    list(fitted_meta = meta_model, predict_fun = predict_esl)
}

logistic_meta_fit <- function(cv_lvl1_and_loss) {
    # Fit a logistic regression on the level 1 data
    df <- as.data.frame(cv_lvl1_and_loss$lvl1)
    names(df) <- c("Y", paste0("Z", 1:(ncol(df) - 1)))
    meta_model <- glm(Y ~ ., data = df, family = binomial)
    meta_model
}

logistic_meta_predict <- function(meta_model, lvl_1_covariates) {
    # Predict using the logistic regression
    to_predict <- as.data.frame(lvl_1_covariates)
    names(to_predict) <- paste0("Z", 1:(ncol(to_predict)))
    meta_predict <- predict(meta_model, newdata = to_predict, type = 'response')
    meta_predict
}

loss_weighted_meta_fit <- function(cv_lvl1_and_loss) {
    # Fit a logistic regression on the level 1 data
    losses <- cv_lvl1_and_loss$losses
    weights <- 1-losses
    weights_normalized <- weights / sum(weights)
    weights_normalized
}

loss_weighted_meta_predict <- function(meta_model, lvl_1_covariates) {
    lvl_1_covariates  %*% meta_model
}

quad_prog_meta_fit <- function(cv_lvl1_and_loss) {
    # Solve quadratic programming
    Y <- cv_lvl1_and_loss$lvl1[,1]
    X <- cv_lvl1_and_loss$lvl1[,-1]
    sol <- lsei(a = X, b = Y, c = rep(1, ncol(X)), d = 1, lower = 0)
    sol
}

quad_prog_meta_predict <- function(meta_model, lvl_1_covariates) {
    weights_normalized <- meta_model
    lvl_1_covariates  %*% weights_normalized
}


meta_learning_algorithm <- c(logistic_meta_fit, logistic_meta_predict)
meta_learning_algorithm_loss_weighted <- c(loss_weighted_meta_fit, loss_weighted_meta_predict)
meta_learning_algorithm_quad_prog <- c(quad_prog_meta_fit, quad_prog_meta_predict)

fit_eSL <- fit_eSL_with_candidates(candidates, meta_learning_algorithm)
fit_eSL_loss_weighted <- fit_eSL_with_candidates(candidates, meta_learning_algorithm_loss_weighted)
fit_eSL_quad_prog <- fit_eSL_with_candidates(candidates, meta_learning_algorithm_quad_prog)

predict_eSL <- function(esl, dat) esl$predict_fun(esl$fitted_meta, dat)