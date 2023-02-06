library(tidyverse)
library(patchwork)

expit <- function(x){exp(x)/(1+exp(x))}
trueModel <- function(Age, Parasites) expit(-3.5-.3*Age+.85*Parasites+0.35*Age*Parasites)
simulateMalariaData <- function(N){
    Age <- runif(N,.5,15)
    Parasites <- rnorm(N,mean=3.5-0.03*Age)
    ps <- trueModel(Age, Parasites)
    Fever <- rbinom(N,1,ps)
    data.frame(Fever,Age,Parasites,ps)
}

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







