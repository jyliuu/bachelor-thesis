library(tidyverse)
library(patchwork)
library(ranger)
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
ggsave("figures/trueplot.pdf",
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
ggsave('figures/predictpar.pdf', plot = p, width = 10, height = 5, units = 'in')
ggsave('figures/xgboost_preds.pdf', plot = xgboostmodplot, width = 6, height = 5, units = 'in')

# Fit random forests using ranger
forest <- ranger(y ~ Age + Parasites, data = simDat, probability = TRUE)
preds <- predict(forest, data = grid, type = "response")
grid$rangerps <- preds$predictions[,1]

rangerplot <- ggplot(grid, aes(x = Age, y = Parasites, fill = rangerps)) + 
  theme_bw() +
  geom_tile() + 
  scale_fill_gradientn(limits = c(0, 1), colors = c("blue", "green", "red")) + 
  xlab("X1") + 
  ylab("X2") + 
  ggtitle("Random forest fitted on 10,000 samples") +
  labs(fill = "Predictions")
rangerplot
