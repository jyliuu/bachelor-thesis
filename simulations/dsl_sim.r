source('dsl.r')
source('model.r')

res10 <- dSL(candidates, simulateMalariaData(10), k=10)
res100 <- dSL(candidates, simulateMalariaData(100), k=100)
res1k <- dSL(candidates, simulateMalariaData(1000), k=25)
res10k <- dSL(candidates, simulateMalariaData(10000), k=10)

res10
res100
res1k
res10k

# Test this way of fitting works
dSL_mod <- fit_dSL(simDat)
dSL_mod
predict_dSL(dSL_mod, simDat)

# Cross validation of discrete super learner
colMeans(cross_validate_multiple(candidates_with_dSL, simulateMalariaData(100), k=100))
colMeans(cross_validate_multiple(candidates_with_dSL, simulateMalariaData(2000), k=30))
colMeans(cross_validate_multiple(candidates_with_dSL, simulateMalariaData(10000), k=10))

# Seems like the discrete super learner is at least as good as the best learner for larger and larger n's

