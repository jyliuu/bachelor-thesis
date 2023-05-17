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
df_melted <- melt(df_combined, id.vars = "id")

p <- ggplot(df_melted, aes(x = id, y = value, fill = variable)) +
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
  scale_fill_discrete(labels = variable_names) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  geom_boxplot() +
  labs(x = "n", y = "Predicted probability", fill = "Model")

ggsave("learner_vars_1000.png",
    plot = p,
    width = 10,
    height = 6,
    dpi = 360,
    units = "in")


# Read old data

