library(tidyverse)
library(patchwork)

source('model.r')

N <- 10^4
simDat <- simulateMalariaData(N)


# create a grid of values for Age and Parasites
grid <- expand.grid(Age = seq(0.5, 15, length.out = 50),
                     Parasites = seq(0, 7, length.out = 50))

# evaluate trueModel at each grid point
grid$ps <- trueModel(grid$Age, grid$Parasites)

# plot the grid
trueplot <- ggplot(grid, aes(x = Age, y = Parasites, fill = ps)) + 
  theme_bw() +
  geom_tile() + 
  scale_fill_gradientn(limits = c(0, 1), colors = c("blue", "green", "red")) + 
  xlab("X1") + 
  ylab("X2") + 
  ggtitle("The data-generating regression function") +
  labs(fill = "Probabilities")

trueplot
ggsave("trueplot.png",
    plot = trueplot,
    width = 6,
    height = 5,
    dpi = 360,
    units = "in")
# Fitting logistic regression without interaction term
modLogReg0 <- glm(Fever ~ Age + Parasites, data = simDat, family = binomial()) 
modLogRegIntercept <- glm(Fever ~ 1, data = simDat, family = binomial()) 


# Estimates the true parameters correctly
grid$psLogReg0 <- predict(modLogReg0, newdata=grid, type='response')
grid$psLogRegIntercept <- predict(modLogRegIntercept, newdata=grid, type='response')

# modLogReg0 predictions 
misspecifiedplot <- ggplot(grid, aes(x = Age, y = Parasites, fill = psLogReg0)) + 
  theme_bw() +
  geom_tile() + 
  scale_fill_gradientn(limits = c(0, 1), colors = c("blue", "green", "red")) + 
  xlab("X1") + 
  ylab("X2") + 
  ggtitle("Main effects logistic regression") +
  labs(fill = "Probabilities")

misspecifiedplotintercept <-  ggplot(grid, aes(x = Age, y = Parasites, fill = psLogRegIntercept)) + 
  theme_bw() +
  geom_tile() + 
  scale_fill_gradientn(limits = c(0, 1), colors = c("blue", "green", "red")) + 
  xlab("X1") + 
  ylab("X2") + 
  ggtitle("Intercept logistic regression") +
  labs(fill = "Probabilities")

## Fitting xgboost model
library(xgboost)
bstSparse <- xgboost(data = as.matrix(simDat[c('Age', 'Parasites')]), 
                     label = simDat$y,
                     max.depth = 3,
                     eta = 0.2,
                     nthread = 5,
                     nrounds = 100, 
                     objective = 'binary:logistic',
                     booster = 'dart'
) 

grid$xgboostps <- predict(bstSparse, as.matrix(grid[c('Age', 'Parasites')]))


xgboostmodplot <-  ggplot(grid, aes(x = Age, y = Parasites, fill = xgboostps)) + 
  theme_bw() +
  geom_tile() + 
  scale_fill_gradientn(limits = c(0, 1), colors = c("blue", "green", "red")) + 
  xlab("X1") + 
  ylab("X2") + 
  ggtitle("XGBoost fitted on 10,000 samples") +
  labs(fill = "Predictions")

xgboostmodplot
p <- misspecifiedplot + theme(legend.position = "none") |xgboostmodplot 
ggsave('predictpar.png', plot = p, width = 10, height = 5, dpi = 360, units = 'in')
ggsave('xgboost10k.png', plot = xgboostmodplot, width = 6, height = 5, dpi = 360, units = 'in')


# For the ensemble learner
source('esl.r')
eSL <- fit_eSl(simDat)
grid$eSL <- predict_eSL(eSL, grid)

eslplot <-  ggplot(grid, aes(x = Age, y = Parasites, fill = eSL)) + 
  theme_bw() +
  geom_tile() + 
  scale_fill_gradientn(limits = c(0, 1), colors = c("blue", "green", "red")) + 
  xlab("X1") + 
  ylab("X2") + 
  ggtitle("Ensemble super learner") +
  labs(fill = "Predictions")

ggsave('esl.png', plot = eslplot, width = 6, height = 5, dpi = 360, units = 'in')