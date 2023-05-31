library(tidyverse)
library(patchwork)
source('esl.r')
source('model.r')
source('learners.r')


## Test with truth
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

predictions <- ggplot(aes(x = logReg, y = xgboost, color = truth, shape=as.factor(clusterRes$cluster)), data = lvl1) +
  geom_point(size = 2.5) + 
  theme_bw() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Main effects", y = "XGBoost", color = "Probabilities", shape = "K-means cluster") +
  scale_color_gradientn(limits = c(0, 1), colors = c("blue", "green", "red"))
predictions



# Plot predicted values

fit_esl_and_plot <- function(simDat, title, esl_fitter) {

  # create a grid of values for Age and Parasites
  grid <- expand.grid(Age = seq(0.5, 15, length.out = 50),
                      Parasites = seq(0, 7, length.out = 50))

  eSL <- esl_fitter(simDat)
  print(eSL$fitted_meta)
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

N <- 1000
simDat <- simulateMalariaData(N)
eslplotQuadProg <- fit_esl_and_plot(simDat, "eSL predictions n = 10000", fit_eSL_quad_prog)
eslplotKmeans <- fit_esl_and_plot(simDat, "eSL predictions n = 10000", fit_eSL_kmeans)
eslplotQuadProg
eslplotKmeans
parplot <- eslplotQuadProg + theme(legend.position = "none") | eslplotKmeans
parplot
ggsave('figures/esl_preds_par.png', plot = parplot, width = 10, height = 5, dpi = 360, units = 'in')

# Plot weights against n 

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

obs_counts <- c(99, rep(100, 39))
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
    legend.text = element_text(size = 12), # Change this for smaller text
    legend.key.size = unit(1.5, "lines"),  # Change this for smaller keys
    legend.background = element_rect(color = "black", linewidth = 0.15)  # Add a box with black border around the legend
  )
ggsave("figures/esl_weights.png", plot = p, width = 10, height = 6, dpi = 360, units = "in")
