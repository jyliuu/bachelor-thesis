library(tidyverse)

source('esl.r')
source('model.r')
source('learners.r')


## Test with truth
simDat <- simulateMalariaData(3000)
candidatesWithTrue <- c(candidatesLogReg, candidatesTree, 
list(logRegTrue = c(fit_logreg_true, function(mod, obs) trueModel(obs$Age, obs$Parasites))))

fit_eSL_quad_prog <- fit_eSL_with_candidates(candidatesWithTrue, meta_learning_algorithm_quad_prog)
eSL <- fit_eSL_quad_prog(simDat)
print(eSL$fitted_meta)


fit_esl_and_plot <- function(simDat, title) {

  # create a grid of values for Age and Parasites
  grid <- expand.grid(Age = seq(0.5, 15, length.out = 50),
                      Parasites = seq(0, 7, length.out = 50))

  eSL <- fit_eSl_quad_prog(simDat)
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
eslplot1k <- fit_esl_and_plot(simDat, "eSL predictions n = 1000")

N <- 9000
simDat9k <- simulateMalariaData(N)
eslplot10k <- fit_esl_and_plot(rbind(simDat, simDat9k), "eSL predictions (n = 10,000)")

parplot <- eslplot1k + theme(legend.position = "none") | eslplot10k
parplot
ggsave('figures/esl_preds_par.png', plot = parplot, width = 10, height = 5, dpi = 360, units = 'in')

