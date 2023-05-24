library(tidyverse)

source('esl.r')
source('model.r')

# obs <- data.frame(
#   Age = c(15),
#   Parasites = c(7)
# )

# eSL <- fit_eSl_quad_prog(simulateMalariaData(10^4))
# predict_eSL(eSL, obs)

N <- 10^4
simDat <- simulateMalariaData(N)

# create a grid of values for Age and Parasites
grid <- expand.grid(Age = seq(0.5, 15, length.out = 50),
                     Parasites = seq(0, 7, length.out = 50))

eSL <- fit_eSl_quad_prog(simDat)
grid$eSL <- predict_eSL(eSL, grid)                

eslplot <-  ggplot(grid, aes(x = Age, y = Parasites, fill = eSL)) + 
  theme_bw() +
  geom_tile() + 
  scale_fill_gradientn(limits = c(0, 1), colors = c("blue", "green", "red")) + 
  xlab("X1") + 
  ylab("X2") + 
  ggtitle("Ensemble super learner") +
  labs(fill = "Predictions")

ggsave('figures/esl_preds_quad_prog.png', plot = eslplot, width = 6, height = 5, dpi = 360, units = 'in')
