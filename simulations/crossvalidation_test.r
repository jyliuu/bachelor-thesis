source('model.r')
source('crossvalidation.r')
source('loss.r')
source('learners.r')

N <- 10^3
simDat <- simulateMalariaData(N)

candidates <- list(logRegTrue = c(fit_logreg_true, predict_logreg), 
                   mainEffects = c(fit_logreg, predict_logreg))

cvres <- cross_validate_multiple(candidates, simDat)
cvres

cross_validate(fit_logreg, predict_logreg, simDat)
cross_validate(fit_logreg_true, predict_logreg, simDat)
cross_validate(fit_logreg_true, predict_logreg_reverse, simDat)
cross_validate(fit_logreg_intercept, predict_logreg, simDat)
cross_validate(fit_xgboost, predict_xgboost, simDat)


level_1 <- cross_validate_multiple_lvl1(candidates, simDat)
MSE(level_1[,1], level_1[,2])
MSE(level_1[,1], level_1[,3])

