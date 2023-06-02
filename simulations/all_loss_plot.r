source('dsl.r')
source('esl.r')
source('model.r')
source('simulation_functions.r')
library(reshape2)


# Seeds 20, 23 is interesting, jumps between logreg and xgboost
# Seed 21 super learner == xgboost
# Seed 22 is perfect, takes min of logreg and xgboost
jump <- 100
seed <- 23
test_count <- 10^6
candidates_with_SLs <- c(candidates, list(eSL_kmeans = c(fit_eSL_kmeans, predict_eSL), eSL = c(fit_eSL_quad_prog, predict_eSL), dSL = c(fit_dSL, predict_dSL)))
losses <- get_losses_candidates(c(3999, rep(jump, 8)), candidates_with_SLs, test_count = test_count, loss_fun = MSE, seed = seed)
losses
# Plot losses dsl 2
# Idea: one way to make this more interesting could be to simulate data from a much more complicated distribution, with some of the features having high importance but occurs less frequently? 
# Perhaps the risk can be calculated explicitly, since we have the model formula after all?


# Convert the row numbers to a column for the x-axis
losses_df <- as.data.frame(losses)
losses_df$n <- (1:nrow(losses_df)) * jump
df_melted <- melt(losses_df, id.vars = "n", variable.name = "Model", value.name = "Loss")

# Melt the data to long format
p <- plot_losses(df_melted,
  labels = c('Main effects', 'Intercept only', 'XGBoost', 'Locally weighted eSL', 'eSL', 'dSL'),
  alpha = c(1, 1, 1, 0.7, 0.6, 0.6),
  linetypes = c('dashed', 'dashed', 'dashed', 'solid', 'solid', 'solid')
)
p
# Save the plot
ggsave("figures/losses_esl_lw.png", plot = p, width = 8, height = 5, dpi = 360, units = "in")

