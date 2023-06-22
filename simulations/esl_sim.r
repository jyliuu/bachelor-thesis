library(tidyverse)
library(patchwork)
source('esl.r')
source('model.r')
source('learners.r')
source('simulation_functions.r')


# Test with truth
plot_xgboost_vs_main_effects_kmeans <- function() {
  simDat <- simulateMalariaData(3000)
  candidatesWithTrue <- c(
    candidatesLogReg, 
    candidatesTree,
    list(truth = c(function (...) {}, function(mod, obs) trueModel(obs$Age, obs$Parasites)))
  )

  fit_eSL_quad_prog <- fit_eSL_with_candidates(candidatesWithTrue, meta_learning_algorithm_quad_prog)
  eSL <- fit_eSL_quad_prog(simDat)
  lvl1 <- as_tibble(eSL$cv_lvl1_and_loss$lvl1)
  head(lvl1)

  clusterRes <- kmeans(lvl1[c("logReg", "xgboost")], centers = 4)

  predictions <- ggplot(aes(x = logReg, y = xgboost, color = truth), data = lvl1) +
    geom_point(size = 1) + 
    theme_bw() +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "Main effects", y = "XGBoost", color = "Probabilities") +
    scale_color_gradientn(limits = c(0, 1), colors = c("blue", "green", "red"))
  predictions
  ggsave("figures/esl_preds_xgboost_vs_main.pdf", plot = predictions, width = 6, height = 4, units = "in")

  predictions_kmeans <- ggplot(aes(x = logReg, y = xgboost, color = truth, shape = as.factor(clusterRes$cluster)), data = lvl1) +
    geom_point(size = 1) + 
    theme_bw() +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "Main effects", y = "XGBoost", color = "Probabilities", shape = "Cluster") +
    scale_color_gradientn(limits = c(0, 1), colors = c("blue", "green", "red"))
  predictions_kmeans
  ggsave("figures/esl_preds_xgboost_vs_main_kmeans.pdf", plot = predictions_kmeans, width = 6, height = 4, units = "in")
}
# Plot predicted values
fit_esl_and_plot <- function(simDat, title, esl_fitter) {

  # create a grid of values for Age and Parasites
  grid <- expand.grid(Age = seq(0.5, 15, length.out = 50),
                      Parasites = seq(0, 7, length.out = 50))

  eSL <- esl_fitter(simDat)
  grid$eSL <- predict_eSL(eSL, grid)                

  eslplot <- ggplot(grid, aes(x = Age, y = Parasites, fill = eSL)) + 
    theme_bw() +
    geom_tile() + 
    scale_fill_gradientn(limits = c(0, 1), colors = c("blue", "green", "red")) + 
    xlab("X1") + 
    ylab("X2") + 
    ggtitle(title) +
    labs(fill = "Predictions")
  eslplot
}
plot_predicted_values_kmeans <- function() {
  N <- 1000
  simDat <- simulateMalariaData(N)
  eslplotKmeans1k <- fit_esl_and_plot(simDat, "Locally weighted eSL predictions (n = 1,000)", fit_eSL_kmeans)


  N <- 10000
  simDat <- simulateMalariaData(N)
  eslplotKmeans10k <- fit_esl_and_plot(simDat, "Locally weighted eSL predictions (n = 10,000)", fit_eSL_kmeans)

  parplot <- eslplotKmeans1k + theme(legend.position = "none") | eslplotKmeans10k
  parplot
  ggsave('figures/esl_preds_lw.pdf', plot = parplot, width = 10, height = 5, units = 'in')
}

plot_predicted_values_quad_prog <- function() {
  N <- 1000
  simDat <- simulateMalariaData(N)
  eslplot1k <- fit_esl_and_plot(simDat, "eSL predictions (n = 1,000)", fit_eSL_quad_prog)


  N <- 10000
  simDat <- simulateMalariaData(N)
  eslplot10k <- fit_esl_and_plot(simDat, "eSL predictions (n = 10,000)", fit_eSL_quad_prog)

  parplot <- eslplot1k + theme(legend.position = "none") | eslplot10k
  parplot
  ggsave('figures/esl_preds_par.pdf', plot = parplot, width = 10, height = 5, units = 'in')
}

# Plot weights against n 
plot_weights_over_n <- function() {
  get_weights_over_obs <- function(obs_counts, seed=19) {
    set.seed(seed)
    train_set <- simulateMalariaData(1)

    fitted_metas <- foreach (j = obs_counts, .combine = 'rbind') %do% { 
        train_set <- rbind(train_set, simulateMalariaData(j))
        esl <- fit_eSL_quad_prog(train_set)
        esl$fitted_meta
    }

    fitted_metas
  }

  obs_counts <- c(99, rep(100, 34))
  res <- get_weights_over_obs(obs_counts) |> as_tibble() 
    
  res$n <- cumsum(obs_counts) + 1
  res <- res |> pivot_longer(-n, names_to = "Model", values_to = "Weight")
  p <- res |> 
    mutate(Model = fct_recode(Model, "Intercept only" = "14", "Main effects" = "V1", "XGBoost" = "15")) |>
    ggplot(aes(x = n, y = Weight, fill = Model)) +
    geom_bar(position="stack", stat="identity") +
    theme_bw() +
    labs(x = "n", y = "Weight", fill = "") +
    scale_x_continuous(breaks = c(100, seq(500, 4000, by = 500))) +
    theme(
      legend.title = element_blank(),
      legend.position = 'bottom',
      legend.text = element_text(size = 8), # Change this for smaller text
      legend.key.size = unit(1, "lines"),  # Change this for smaller keys
      legend.background = element_rect(color = "black", linewidth = 0.15)  # Add a box with black border around the legend
    )
  ggsave("figures/esl_weights.pdf", plot = p, width = 6, height = 4, units = "in")
}

# Plot predicted stratified according to k-means
plot_predicted_values_kmeans_stratified <- function(N = 3000) {
  fit_eSL_kmeans <- fit_eSL_with_candidates(candidates, c(kmeans_meta_fit, kmeans_meta_predict_weights))

  fit_esl_and_plot <- function(simDat) {

    # create a grid of values for Age and Parasites
    grid <- expand.grid(Age = seq(0.5, 15, length.out = 50),
                        Parasites = seq(0, 7, length.out = 50))

    eSL <- fit_eSL_kmeans(simDat)

    candidates_fitted_predict <- eSL$candidates_fitted_predict
    candidate_plots <- NULL
    for (name in names(candidates_fitted_predict)) {
      if (name == 'logRegIntercept') next
      learner <- candidates_fitted_predict[[name]]
      learner_predictions <- learner$predict_fun(learner$fitted_model, grid)
      grid[name] <- learner_predictions

      if (is.null(candidate_plots)) 
        candidate_plots <- plot_grid_predictions(grid, name, name)
      else 
        candidate_plots <- candidate_plots + theme(legend.position = "none") | plot_grid_predictions(grid, name, name)
    }

    res <- predict_eSL(eSL, grid)
    grid$predicted <- res$predicted
    grid$most_weighted <- factor(res$most_weighted, labels = c("Main effects", "Intercept only", "XGBoost")[unique(res$most_weighted)])

    eslplot_stratified <- ggplot(grid, aes(x = Age, y = Parasites, color = predicted, shape = most_weighted)) + 
      theme_bw() +
      geom_point(size = 2) + 
      scale_color_gradientn(limits = c(0, 1), colors = c("blue", "green", "red")) + 
      scale_shape_manual(values = c("Main effects" = 16, "XGBoost" = 3, "Intercept only" = 17)) +
      xlab("X1") + 
      ylab("X2") + 
      ggtitle("Locally weighted eSL predictions grouped (n = 3,000)") +
      labs(color = "Predictions", shape = "Most weighted") +
      guides(color = guide_colorbar(order = 0), shape = guide_legend(order = 1))

    eslplot_tiled <- ggplot(grid, aes(x = Age, y = Parasites, fill = predicted)) + 
      theme_bw() +
      geom_tile() + 
      scale_fill_gradientn(limits = c(0, 1), colors = c("blue", "green", "red")) + 
      xlab("X1") + 
      ylab("X2") + 
      ggtitle("Locally weighted eSL predictions (n = 3,000)") +
      labs(fill = "Predictions")
    list(eslplot_stratified, eslplot_tiled, candidate_plots)
  }

  simDat <- simulateMalariaData(N)
  p <- fit_esl_and_plot(simDat)
  p[[2]]
  ggsave('figures/esl_preds_lw_stratified.pdf', plot = p[[1]]+ggtitle("Locally weighted eSL predictions grouped (n = 1,000)") , width = 6, height = 5, units = 'in')
  ggsave('figures/esl_preds_lw_tiled.png', plot = p[[2]], width = 6, height = 5, units = 'in')
  tiled <- p[[2]] + theme(legend.position = "none") | p[[1]] 
  ggsave('figures/esl_preds_lw_stratified_tiled.pdf', plot = tiled, width = 10, height = 5, units = 'in')
}

plot_predicted_values_kmeans_location <- function() {
  fit_esl_and_plot <- function(simDat) {

    # create a grid of values for Age and Parasites
    grid <- expand.grid(Age = seq(0.5, 15, length.out = 50),
                        Parasites = seq(0, 7, length.out = 50))

    eSL <- fit_eSL_kmeans_location(simDat)
    res <- predict_eSL(eSL, grid)                
    grid$predicted <- res$predicted
    grid$most_weighted <- factor(res$most_weighted, labels = c("Main effects", "Intercept only", "XGBoost")[unique(res$most_weighted)])

    eslplot_stratified <- ggplot(grid, aes(x = Age, y = Parasites, color = predicted, shape = most_weighted)) + 
      theme_bw() +
      geom_point(size = 2) + 
      scale_color_gradientn(limits = c(0, 1), colors = c("blue", "green", "red")) + 
      scale_shape_manual(values = c(16, 3, 17)) +
      xlab("X1") + 
      ylab("X2") + 
      ggtitle("Locally weighted eSL predictions grouped (n = 1000)") +
      labs(color = "Predictions", shape = "Most weighted") +
      guides(color = guide_colorbar(order = 0), shape = guide_legend(order = 1))

    eslplot_tiled <- ggplot(grid, aes(x = Age, y = Parasites, fill = predicted)) + 
      theme_bw() +
      geom_tile() + 
      scale_fill_gradientn(limits = c(0, 1), colors = c("blue", "green", "red")) + 
      xlab("X1") + 
      ylab("X2") + 
      ggtitle("Locally weighted eSL predictions (n = 1000)") +
      labs(fill = "Predictions")
    list(eslplot_stratified, eslplot_tiled)
  }
  N <- 1000
  simDat <- simulateMalariaData(N)
  p <- fit_esl_and_plot(simDat)
  ggsave('figures/esl_preds_lw_stratified_location.png', plot = p[[1]], width = 6, height = 5, units = 'in')
  ggsave('figures/esl_preds_lw_stratified_location_tiled.png', plot = p[[2]], width = 6, height = 5, units = 'in')
}

plot_predicted_values_kmeans_stratified()
