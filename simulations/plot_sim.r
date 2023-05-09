library(tidyverse)
library(patchwork)

source('model.r')

N <- 10^5
simDat <- simulateMalariaData(N)

### 2d plot of the true model

ggplot(simDat, aes(x = Age, y = Parasites, color = ps)) + 
  geom_point() + 
  scale_color_gradient(limits = c(0, 1), low = "blue", high = "red") + 
  xlab("Age") + 
  ylab("Parasites") + 
  ggtitle("2D Plot of Age vs Parasites")
# Not quite what I was looking for, we can make a heatmap plot



# create a grid of values for Age and Parasites
grid <- expand.grid(Age = seq(0.5, 15, length.out = 50),
                     Parasites = seq(0, 7, length.out = 50))

# evaluate trueModel at each grid point
grid$ps <- trueModel(grid$Age, grid$Parasites)

# plot the grid
trueplot <- ggplot(grid, aes(x = Age, y = Parasites, fill = ps)) + 
  geom_tile() + 
  scale_fill_gradient(limits = c(0, 1), low = "green", high = "red") + 
  xlab("Age") + 
  ylab("Parasites") + 
  ggtitle("Heatmap of Age vs Parasites")

trueplot

# Fitting logistic regression without interaction term
modLogReg0 <- glm(Fever ~ Age + Parasites, data = simDat, family = binomial()) 
summary(modLogReg0)

# And with interactions
modLogRegTrue <- glm(Fever ~ Age + Parasites + Age*Parasites, data = simDat, family = binomial()) 
summary(modLogRegTrue)
# Estimates the true parameters correctly

grid$psLogReg0 <- predict(modLogReg0, newdata=grid, type='response')
grid$psLogRegTrue <- predict(modLogRegTrue, newdata=grid, type='response')

# modLogReg0 predictions 
misspecifiedplot <- ggplot(grid, aes(x = Age, y = Parasites, fill = psLogReg0)) + 
  geom_tile() + 
  scale_fill_gradient(limits = c(0, 1), low = "blue", high = "red") + 
  xlab("Age") + 
  ylab("Parasites") + 
  ggtitle("Predicted probabilities using misspecified logReg0")
  
truemodplot <- ggplot(grid, aes(x = Age, y = Parasites, fill = psLogRegTrue)) + 
  geom_tile() + 
  scale_fill_gradient(limits = c(0, 1), low = "blue", high = "red") + 
  xlab("Age") + 
  ylab("Parasites") + 
  ggtitle("Predicted probabilities using true identification logRegTrue")

trueplot | misspecifiedplot / truemodplot

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


xgboostmodplot <- ggplot(grid, aes(x = Age, y = Parasites, fill = xgboostps)) + 
  geom_tile() + 
  scale_fill_gradient(limits = c(0, 1), low = "blue", high = "red") + 
  xlab("Age") + 
  ylab("Parasites") + 
  ggtitle("Predicted probabilities using XGBoost")

xgboostmodplot 

