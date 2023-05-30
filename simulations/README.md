# Simulations with discrete super learner

## File structure

The library files:

- `model.r`: The data-generating process
- `learners.r`: Library of learners
- `loss.r`: Loss functions
- `crossvalidation.r`: Cross-validation functions
- `dsl.r` (sources from `crossvalidation.r, learners.r, loss.r`): Discrete super learner library functions
- `esl.r` (sources from `crossvalidation.r, learners.r, loss.r`): Ensemble super learner library functions
- `simulation_functions.r`: Functions used for simulations for eSL and dSL simulations

The simulation files:

- `plot_sim.r` (sources from `model.r`): Plots the data-generating process and model fits
- `cross_validation_test.r` (sources from `model.r, crossvalidation.r, loss.r, learners.r`): Cross validates learners and return results
- `dsl_sim.r` (sources from `dsl.r, model.r`): Fits dSL and returns results
- `esl_sim.r` (sources from `esl.r, model.r`): Fits eSL and plot predictions
- `dsl_loss_plot.r` (sources from `dsl.r, model.r`): Fits dSL and plots dSL loss in comparison with other learners
- `all_loss_plot.r` (sources from `dsl.r, esl.r, simulation_functions.r, model.r`): Fits dSL and eSL and plots their loss in comparison with other learners
- `dsl_variance_new_obs.r` (sources from `dsl.r, simulation_functions.r, model.r`): Fits dSL and plots variance of predictions for single new observation
- `dsl_vs_esl.r` (sources from `dsl.r, esl.r, simulation_functions.r, model.r`): Fits dSL and eSL and compares their loss along with their prediction variance
- `quad_prog_test.r`: Tests the quadratic programming solver `lsei`
