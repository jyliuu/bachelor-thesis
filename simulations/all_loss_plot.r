source('dsl.r')
source('esl.r')
source('model.r')
source('simulation_functions.r')
library(reshape2)


# Seeds 20, 23 is interesting, jumps between logreg and xgboost
# Seed 21 super learner == xgboost
# Seed 22 is perfect, takes min of logreg and xgboost
jump <- 100
seed <- 22
test_count <- 10^6
candidates_with_SLs <- c(candidates, list(eSL = c(fit_eSL_quad_prog, predict_eSL), dSL = c(fit_dSL, predict_dSL)))
candidates_eSL_kmeans <- c(candidates_with_SLs, list(eSL_kmeans = c(fit_eSL_quad_prog, predict_eSL)))

get_losses_and_plot <- function(
    candidates, 
    jumps = 34, 
    labels = c('Main effects', 'Intercept only', 'XGBoost', 'eSL', 'dSL'),
    alpha = c(1, 1, 1, 0.6, 0.6), 
    linetypes = c('dashed', 'dashed', 'dashed',  'solid', 'solid')
) {
    losses <- get_losses_candidates(c(jump-1, rep(jump, jumps)), candidates, test_count = test_count, loss_fun = MSE, seed = seed)
    losses
    # Plot losses dsl 2
    # Idea: one way to make this more interesting could be to simulate data from a much more complicated distribution, with some of the features having high importance but occurs less frequently? 
    # Perhaps the risk can be calculated explicitly, since we have the model formula after all?


    # Convert the row numbers to a column for the x-axis
    losses_df <- as.data.frame(losses)
    losses_df$n <- (1:nrow(losses_df)) * jump
    df_melted <- melt(losses_df, id.vars = "n", variable.name = "Model", value.name = "Loss")

    # Melt the data to long format
    #  'Locally weighted eSL',  0.7, 'solid',
    p <- plot_losses(df_melted, labels = labels, alpha = alpha, linetypes = linetypes)
    p
    # Save the plot
    ggsave("figures/losses_candidates.pdf", plot = p, width = 6, height = 4, units = "in")
    df_melted
}

res <- get_losses_and_plot(candidates, 
    labels = c('Main effects', 'Intercept only', 'XGBoost'),
    alpha = c(1, 1, 1), 
    linetypes = c('dashed', 'dashed', 'dashed')
)

res <- get_losses_and_plot(candidates_eSL_kmeans, 
    labels = c('Main effects', 'Intercept only', 'XGBoost', 'eSL', 'dSL'),
    alpha = c(1, 1, 1, 0.6, 0.6), 
    linetypes = c('dashed', 'dashed', 'dashed', 'solid', 'solid')
)
