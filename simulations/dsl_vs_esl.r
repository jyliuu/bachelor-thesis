source('dsl.r')
source('esl.r')
source('model.r')
source('get_losses_candidates.r')
library(reshape2)


seed <- 23
test_count <- 10^6
super_learners <- c(list(eSL = c(fit_eSl_quad_prog, predict_eSL), dSL = c(fit_dSL, predict_dSL)))
losses <- get_losses_candidates(c(3300, 100, 100, 100), super_learners, test_count = test_count, loss_fun = MSE, seed = seed)
losses
