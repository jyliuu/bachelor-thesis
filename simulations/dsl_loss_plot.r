source('dsl.r')
source('model.r')
source('simulation_functions.r')
library(reshape2)


# Seeds 20, 23 is interesting, jumps between logreg and xgboost
# Seed 21 super learner == xgboost
# Seed 22 is perfect, takes min of logreg and xgboost
jump <- 100
seed <- 23
test_count <- 10^6
candidates_with_dSL <- c(candidates, list(dSL = c(fit_dSL, predict_dSL)))
losses_dsl <- get_losses_candidates(c(99, rep(jump, 34)), candidates_with_dSL, test_count = test_count, loss_fun = MSE, seed = seed)
losses_dsl

# Convert the row numbers to a column for the x-axis
losses_dsl_df <- as.data.frame(losses_dsl)
losses_dsl_df$n <- (1:nrow(losses_dsl_df)) * jump
df_melted <- melt(losses_dsl_df, id.vars = "n", variable.name = "Model", value.name = "Loss")

# Plot losses dsl
p <- plot_losses(df_melted, 
  labels=c('Main effects', 'Intercept only', 'XGBoost', 'Discrete super learner'),
  alpha=c(1, 1, 1, 0.6),
  linetypes=c('dashed', 'dashed', 'dashed', 'solid')
)

# Save the plot
ggsave("figures/losses_s23.png", plot = p, width = 6, height = 4, dpi = 360, units = "in")

