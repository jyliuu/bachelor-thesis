# Simulations with discrete super learner

## File structure

The library files:
- `model.r`: The data-generating process 
- `learners.r`: Library of learners
- `loss.r`: Loss functions
- `crossvalidation.r`: Cross-validation functions 
- `dsl.r` (sources from `crossvalidation.r, learners.r, loss.r`): Discrete super learner library functions

The simulation files:
- `plot_sim.r` (sources from `model.r`): Plots the data-generating process and model fits 
- `cross_validation_test.r` (sources from `model.r, crossvalidation.r, loss.r, learners.r`): Cross validates learners and return results 
- `dsl_sim.r` (sources from `dsl.r, model.r`): Fits dSL and returns results 
- `dsl_loss_plot.r` (sources from `dsl.r, model.r`): Fits dSL and plots dSL loss in comparison with other learners 
