source('dsl.r')
source('esl.r')
source('model.r')
source('simulation_functions.r')
library(reshape2)

# Losses with truth
seed <- 23
test_count <- 10^6
jump <- 100

fit_eSL_quad_prog <- fit_eSL_with_candidates(candidatesWithTrue, meta_learning_algorithm_quad_prog)

to_eval <- c(list(logRegTrue = c(fit_logreg_true, predict_logreg), eSL = c(fit_eSL_quad_prog, predict_eSL), dSL = c(fit_dSL, predict_dSL)))
losses <- get_losses_candidates(c(99, rep(jump, 34)), to_eval, test_count = test_count, loss_fun = MSE, seed = seed)

losses_df <- as.data.frame(losses)
losses_df$n <- (1:nrow(losses_df)) * jump
df_melted <- melt(losses_df, id.vars = "n", variable.name = "Model", value.name = "Loss")
p <- plot_losses(df_melted, 
  labels=c('True model', 'Ensemble super learner', 'Discrete super learner'),
  alpha=c(0.6, 0.8, 0.8),
  linetypes=c('dashed', 'solid', 'solid')
)
p

# Variances 
obs <- data.frame(
  Age = 4.5,
  Parasites = 2
)
true_prob <- 0.5
training_counts <- c(150, 500, 1000, 1500, 3000)
variable_names <- c("Ensemble super learner")
res_lib_diff_ns <- foreach(n = training_counts) %do% {
    print(n)
    df <- as.data.frame(fit_and_predict_library_on_new_obs(super_learners, obs, n, 1000))
    rownames(df) <- NULL
    df
}


# Bind all dataframes into one and add an id column
df_combined <- bind_rows(res_lib_diff_ns, .id = "id")
df_melted <- melt(df_combined, id.vars = "id")

# Load data from previous run
load('data/learner_vars_w_esl.RData')

boxplots <- ggplot(all_vars, aes(x = id, y = value, fill = variable)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = 'bottom',
    legend.text = element_text(size = 10), # Change this for smaller text
    legend.key.size = unit(2.3, "lines"),  # Change this for smaller keys
    legend.background = element_rect(color = "black", linewidth = 0.15)  # Add a box with black border around the legend
  )+
  geom_hline(aes(yintercept = true_prob), color = "red", linetype = "dashed") +
  scale_x_discrete(labels = training_counts) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  geom_boxplot() +
  labs(x = "n", y = "Predicted probability", fill = "Model")

ggsave("figures/learner_vars_esl_vars_1000.png",
    plot = boxplots,
    width = 10,
    height = 6,
    dpi = 360,
    units = "in")

# Variance histograms for esl


esl_dist <- all_vars |>
    filter(variable == 'Ensemble super learner')


hist_esl_dist <- esl_dist |>
    ggplot(aes(x = value, fill = id, alpha = 0.5)) + 
    geom_histogram(color = "black", bins = 25, position = "identity") +
    theme_bw() +
    theme(
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 10), # Change this for smaller text
        legend.key.size = unit(1.5, "lines"),  # Change this for smaller keys
        legend.background = element_rect(color = "black", linewidth = 0.15),  # Add a box with black border around the legend
        strip.background = element_blank(),  # Remove background from facet labels
        strip.text = element_blank()  # Remove facet labels
    ) +    
    labs(x = "Predicted probability", y = "Count") +
    scale_x_continuous(breaks = seq(0.2, 0.8, by = 0.3)) +
    scale_fill_discrete(labels = c("n = 150", "n = 500", "n = 1000", "n = 1500", "n = 3000")) +
    guides(alpha = "none") +
    facet_wrap(~ id, nrow = 1)

hist_esl_dist
ggsave("figures/hist_esl_dist.png", plot = hist_esl_dist, width = 10, height = 4, dpi = 360, units = "in")
