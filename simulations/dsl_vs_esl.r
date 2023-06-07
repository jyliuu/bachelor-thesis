source('dsl.r')
source('esl.r')
source('model.r')
source('simulation_functions.r')
library(reshape2)

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
    legend.text = element_text(size = 8), # Change this for smaller text
    legend.key.size = unit(1, "lines"),  # Change this for smaller keys
    legend.background = element_rect(color = "black", linewidth = 0.15)  # Add a box with black border around the legend
  )+
  geom_hline(aes(yintercept = true_prob), color = "red", linetype = "dashed") +
  scale_x_discrete(labels = training_counts) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  geom_boxplot(outlier.size = 0.1, linewidth = 0.2) +
  labs(x = "n", y = "Predicted probability", fill = "Model")

ggsave("figures/learner_vars_w_esl_1000.pdf",
    plot = boxplots,
    width = 7,
    height = 4,
    units = "in")

# Variance histograms for esl


esl_dist <- all_vars |> filter(variable == 'Ensemble super learner')

hist_esl_dist <- esl_dist |>
    ggplot(aes(x = value, alpha = 0.5)) + 
    geom_histogram(fill = "purple", color = "black", bins = 25, position = "identity") +
    theme_bw() +
    theme(
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 10), # Change this for smaller text
        legend.key.size = unit(1.5, "lines"),  # Change this for smaller keys
        legend.background = element_rect(color = "black", linewidth = 0.15),  # Add a box with black border around the legend
        strip.background = element_blank(),  # Remove background from facet labels
    ) +    
    labs(x = "Predicted probability", y = "Count") +
    scale_x_continuous(breaks = seq(0.2, 0.8, by = 0.3)) +
    guides(alpha = "none") +
    facet_wrap(~ id, nrow = 1, labeller = labeller(id = function(x) paste0("n = ", training_counts[as.numeric(x)])))

hist_esl_dist
ggsave("figures/hist_esl_dist2.pdf", plot = hist_esl_dist, width = 10, height = 4, units = "in")
