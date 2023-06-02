
get_losses_candidates <- function(obs_counts, models_fit_predict, test_count = 5000, loss_fun=MSE, seed=19) {
  set.seed(seed^2)
  test_set <- simulateMalariaData(test_count)
  set.seed(seed)
  train_set <- simulateMalariaData(1)

  p  <- length(models_fit_predict)
  losses_all <- foreach (j = obs_counts, .combine = 'rbind') %do% { 
      train_set <- rbind(train_set, simulateMalariaData(j))
      print(paste('Getting losses with train set of size', nrow(train_set)))

      losses <- foreach(i = 1:p, .combine = 'cbind') %do% {
          model_fun <- models_fit_predict[[i]][[1]]
          predict_fun <- models_fit_predict[[i]][[2]]
          fit <- model_fun(train_set)
          out_of_split_preds <- predict_fun(fit, test_set)
          
          loss_fun(test_set$y, out_of_split_preds)
      }
      losses
  }
  colnames(losses_all) <- names(models_fit_predict)
  losses_all
}

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

fit_and_predict_library_on_new_obs <- function(lib, new_obs, n, K = 1000) {
  preds <- foreach (k = 1:K, .combine = 'rbind') %do% {
      print(paste('Predicting for k =', k))
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

plot_losses <- function(df_melted, labels, alpha, linetypes) ggplot(df_melted, aes(x = n, y = Loss, color = Model, alpha = Model, linetype = Model)) +
  geom_line(size = 0.5) +
  labs(x = "n", y = "Validation risk") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = 'bottom',
    legend.text = element_text(size = 8), # Change this for smaller text
    legend.key.size = unit(1, "lines"),  # Change this for smaller keys
    legend.background = element_rect(color = "black", linewidth = 0.15)  # Add a box with black border around the legend
  ) +
  scale_color_discrete(labels = labels) +
  scale_linetype_manual(values = linetypes, 
                    labels = labels) +
  scale_alpha_manual(values=alpha) +
  guides(alpha = "none")
