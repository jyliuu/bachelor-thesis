

expit <- function(x){exp(x)/(1+exp(x))}
set.seed(10)
trueModel <- function(Age, Parasites) expit(-3.5-.3*Age+.85*Parasites+0.35*Age*Parasites)
simulateMalariaData <- function(N){
    Age <- runif(N,.5,15)
    Parasites <- rnorm(N,mean=3.5-0.03*Age)
    ps <- trueModel(Age, Parasites)
    Fever <- rbinom(N,1,ps)
    data.frame(Fever, y=Fever,Age,Parasites,ps)
}

