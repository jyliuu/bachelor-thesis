source('dsl.r')
source('model.r')

library(reshape2)
fit_and_predict_on_new_obs <- function(fit_predict, new_obs, n, K = 1000) {
    model_fun <- fit_predict[[1]]
    predict_fun <- fit_predict[[2]]

    preds <- foreach (k = 1:K, .combine = 'c') %do% {
        print(paste('Predicting for k = ', k))
        train_set <- simulateMalariaData(n)
        fit <- model_fun(train_set)
        pred <- predict_fun(fit, new_obs)

        pred
    }
    preds
}

fit_library_on_new_obs <- function(lib, new_obs, n, K = 1000) {
    preds <- foreach (k = 1:K, .combine = 'rbind') %do% {
        print(paste('Predicting for k = ', k))
        train_set <- simulateMalariaData(n)

        row <- foreach(i = seq_along(lib), .combine = 'cbind') %do% {
            fit <- lib[[i]][[1]](train_set)
            pred <- lib[[i]][[2]](fit, new_obs)

            pred
        }
        row
    }

    preds
}

# Test
xgboost_fit_predict <- c(fit_xgboost, predict_xgboost)
logistic_fit_predict <- c(fit_logreg, predict_logreg)

obs <- data.frame(
  Age = 4.5,
  Parasites = 2
)
true_prob <- trueModel(obs$Age, obs$Parasites)
res_xgboost <- fit_and_predict_on_new_obs(xgboost_fit_predict, obs, 1000)
res_logistic <- fit_and_predict_on_new_obs(logistic_fit_predict, obs, 1000)
res_lib <- fit_library_on_new_obs(candidatesLogReg, obs, 1000)

training_counts <- c(150, 500, 1000, 1500, 3000)
variable_names <- c("Main effects", "Intercept only", "XGBoost", "Discrete super learner")

res_lib_diff_ns <- foreach(n = training_counts) %do% {
    print(n)
    df <- as.data.frame(fit_library_on_new_obs(candidates_with_dSL, obs, n, 1000))
    rownames(df) <- NULL
    df
}


# Bind all dataframes into one and add an id column
df_combined <- bind_rows(res_lib_diff_ns, .id = "id")
df_melted <- melt(ndf_combined, id.vars = "id")

# Read old data
load('data/learner_vars_1k_upto_3k.RData')
df_melted <- leaner_vars_1k_upto_3k

boxplots <- ggplot(df_melted, aes(x = id, y = value, fill = variable)) +
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
  scale_fill_discrete(labels = variable_names) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  geom_boxplot() +
  labs(x = "n", y = "Predicted probability", fill = "Model")

ggsave("figures/learner_vars_1000.png",
    plot = boxplots,
    width = 10,
    height = 6,
    dpi = 360,
    units = "in")

all_except_baseline <- df_melted |>
    filter(variable != 'result.2', id == 4)

mean_and_sds <- all_except_baseline |> group_by(variable) |> summarize(mean = mean(value), sd = sd(value))

hist_dsl <- all_except_baseline |>
    ggplot(aes(x = value, fill = variable, alpha = variable)) + 
    geom_histogram(color = "black", bins = 40, position = "identity") +
    geom_vline(aes(xintercept = mean, color = variable), linetype = "dashed", size = 1, data = mean_and_sds, show.legend = FALSE) +  # Grouped vertical lines
    theme_bw() +
    theme(
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 10), # Change this for smaller text
        legend.key.size = unit(1.5, "lines"),  # Change this for smaller keys
        legend.background = element_rect(color = "black", linewidth = 0.15)  # Add a box with black border around the legend
    ) +    
    labs(x = "Predicted probability", y = "Count") +
    scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    scale_fill_discrete(labels = c("Main effects", "XGBoost", "Discrete super learner")) +
    scale_alpha_manual(values = c(0.6, 0.4, 0.7)) +
    guides(alpha = "none")

hist_dsl
ggsave("figures/preds_n1k_dsl.png",
    plot = hist_dsl,
    width = 10,
    height = 6,
    dpi = 360,
    units = "in")

## DSL shift

dSL_shift_dist <- df_melted |>
    filter(variable == 'result.4')


hist_dsl_shift <- dSL_shift_dist |>
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
    scale_x_continuous(breaks = seq(0, 1, by = 0.5)) +
    scale_fill_discrete(labels = c("n = 150", "n = 500", "n = 1000", "n = 1500", "n = 3000")) +
    guides(alpha = "none") +
    facet_wrap(~ id, nrow = 1)

hist_dsl_shift
ggsave("figures/preds_dsl_shift.png",
    plot = hist_dsl_shift,
    width = 10,
    height = 4,
    dpi = 360,
    units = "in")
