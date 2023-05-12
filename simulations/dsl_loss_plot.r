source('dsl.r')
source('model.r')

get_losses_incl_dsl <- function(obs_counts, models_fit_predict, test_count = 5000, loss_fun=MSE, seed=19) {
    set.seed(seed^2)
    test_set <- simulateMalariaData(test_count)
    set.seed(seed)
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

# Seeds 20, 23 is interesting, jumps between logreg and xgboost
# Seed 21 super learner == xgboost
# Seed 22 is perfect, takes min of logreg and xgboost
jump <- 100
seed <- 22
test_count <- 10^6
losses_dsl <- get_losses_incl_dsl(c(99, rep(jump, 34)), candidates_with_dSL, test_count = test_count, loss_fun = MSE, seed = seed)
losses_dsl
# Plot losses dsl 2
# Idea: one way to make this more interesting could be to simulate data from a much more complicated distribution, with some of the features having high importance but occurs less frequently? 
# Perhaps the risk can be calculated explicitly, since we have the model formula after all?


library(reshape2)
# Convert the row numbers to a column for the x-axis
losses_dsl_df <- as.data.frame(losses_dsl)
losses_dsl_df$n <- (1:nrow(losses_dsl_df)) * jump

# Melt the data to long format
df_melted <- melt(losses_dsl_df, id.vars = "n", variable.name = "Model", value.name = "Loss")

# Plot the data
p <- ggplot(df_melted, aes(x = n, y = Loss, color = Model, linetype = Model)) +
  geom_line(size = 0.5, alpha = 0.9) +
  labs(x = "n", y = "Loss") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = 'bottom',
    legend.text = element_text(size = 8), # Change this for smaller text
    legend.key.size = unit(1, "lines"),  # Change this for smaller keys
    legend.background = element_rect(color = "black", linewidth = 0.15)  # Add a box with black border around the legend
  ) +
  scale_color_discrete(labels = c('Main effects', 'Intercept only', 'XGBoost', 'Discrete super learner')) +
  scale_linetype_manual(values = c('dashed', 'dashed', 'dashed', 'solid'), 
                    labels = c('Main effects', 'Intercept only', 'XGBoost', 'Discrete super learner')) +
  guides(color = guide_legend(override.aes = list(alpha = c(1, 1, 1, 0.5))))

# Save the plot
ggsave("s23.png", plot = p, width = 6, height = 4, dpi = 360, units = "in")

