source('dsl.r')
source('model.r')

get_losses_incl_dsl <- function(obs_counts, models_fit_predict, test_count = 5000, loss_fun=MSE) {
    test_set <- simulateMalariaData(test_count)
    train_set <- simulateMalariaData(1)

    p  <- length(models_fit_predict)
    preds <- foreach (j = obs_counts, .combine = 'rbind') %do% { 
        train_set <- rbind(train_set, simulateMalariaData(j))
        print(paste('Getting losses with train set of size', nrow(train_set)))

        losses <- foreach(i = 1:p, .combine = 'cbind') %do% {
            model_fun <- models_fit_predict[[i]][[1]]
            predict_fun <- models_fit_predict[[i]][[2]]
            fit <- model_fun(train_set)
            out_of_split_preds <- predict_fun(fit, test_set)
            
            loss_fun(test_set$y, out_of_split_preds)
        }
        cbind(losses)
    }

    preds
}

set.seed(19)
jump <- 100
losses_dsl <- get_losses_incl_dsl(c(99, rep(jump, 35)), candidates_with_dSL)
losses_dsl
# Plot losses dsl 2
png(filename = "myplot.png", width = 1000, height = 800, res = 120)
matplot(x = 1:nrow(losses_dsl)*jump, y=losses_dsl, type='l', lty=1, lwd=2, xlab='n', ylab='Loss')
legend('topright', legend=c('Main effects', 'Intercept only', 'XGBoost', 'Discrete super learner'), col=1:ncol(losses_dsl), lty=1, lwd=2, bg='white', border='black')
dev.off()
# Idea: one way to make this more interesting could be to simulate data from a much more complicated distribution, with some of the features having high importance but occurs less frequently? 
# Perhaps the risk can be calculated explicitly, since we have the model formula after all?
