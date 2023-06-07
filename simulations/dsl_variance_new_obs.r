source('dsl.r')
source('model.r')
source('simulation_functions.r')
library(reshape2)

# Test
obs <- data.frame(
    Age = 4.5,
    Parasites = 2
)
training_counts <- c(150, 500, 1000, 1500, 3000)
variable_names <- c("Main effects", "Intercept only", "XGBoost", "Discrete super learner")
candidates_with_dSL <- c(candidates, list(dSL = c(fit_dSL, predict_dSL)))


fit_and_predict_1000_times <- function() {
    xgboost_fit_predict <- c(fit_xgboost, predict_xgboost)
    logistic_fit_predict <- c(fit_logreg, predict_logreg)

    true_prob <- trueModel(obs$Age, obs$Parasites)
    res_xgboost <- fit_and_predict_on_new_obs(xgboost_fit_predict, obs, 1000)
    res_logistic <- fit_and_predict_on_new_obs(logistic_fit_predict, obs, 1000)
    res_lib <- fit_and_predict_library_on_new_obs(candidatesLogReg, obs, 1000)

    c(true_prob, res_xgboost, res_logistic)
}


fit_and_predict_1000_times_training_150_up_to_3000 <- function() {
    res_lib_diff_ns <- foreach(n = training_counts) %do% {
        print(n)
        df <- as.data.frame(fit_and_predict_library_on_new_obs(candidates_with_dSL, obs, n, 1000))
        rownames(df) <- NULL
        df
    }


    # Bind all dataframes into one and add an id column
    df_combined <- bind_rows(res_lib_diff_ns, .id = "id")
    df_melted <- melt(df_combined, id.vars = "id")
    df_melted
}

plot_from_saved_data <- function(df_melted) {
    # Read old data
    load('data/learner_vars_1k_upto_3k.RData')
    boxplots <- ggplot(learner_vars_1k_upto_3k, aes(x = id, y = value, fill = variable)) +
        theme_bw() +
        theme(
            legend.title = element_blank(),
            legend.position = 'bottom',
            legend.text = element_text(size = 8), # Change this for smaller text
            legend.key.size = unit(1, "lines"),  # Change this for smaller keys
            legend.background = element_rect(color = "black", linewidth = 0.15)  # Add a box with black border around the legend
        )+
        geom_hline(aes(yintercept = true_prob), color = "red", linetype = "dashed") +
        scale_x_discrete(labels = training_counts) +
        scale_fill_discrete(labels = variable_names) +
        scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
        geom_boxplot(outlier.size = 0.1, linewidth = 0.2) +
        labs(x = "n", y = "Predicted probability", fill = "Model")

    ggsave("figures/learner_vars_1000.pdf",
        plot = boxplots,
        width = 6,
        height = 4,
        units = "in")
}

plot_histograms_overlayed <- function(df_melted) {
    all_except_baseline <- df_melted |> filter(variable != 'result.2', id == 4)
    mean_and_sds <- all_except_baseline |> group_by(variable) |> summarize(mean = mean(value), sd = sd(value))

    hist_dsl <- all_except_baseline |>
        ggplot(aes(x = value, fill = variable, alpha = variable)) + 
        geom_histogram(color = "black", bins = 40, position = "identity") +
        geom_vline(aes(xintercept = mean, color = variable), linetype = "dashed", size = 1, data = mean_and_sds, show.legend = FALSE) +  # Grouped vertical lines
        theme_bw() +
        theme(
            legend.title = element_blank(),
            legend.position = 'bottom',
            legend.text = element_text(size = 10), # Change this for smaller text
            legend.key.size = unit(1.5, "lines"),  # Change this for smaller keys
            legend.background = element_rect(color = "black", linewidth = 0.15)  # Add a box with black border around the legend
        ) +    
        labs(x = "Predicted probability", y = "Count") +
        scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
        scale_fill_discrete(labels = c("Main effects", "XGBoost", "Discrete super learner")) +
        scale_alpha_manual(values = c(0.6, 0.4, 0.7)) +
        guides(alpha = "none")

    ggsave("figures/preds_n1k_dsl.png",
        plot = hist_dsl,
        width = 10,
        height = 6,
        dpi = 360,
        units = "in")
    hist_dsl
}


plot_shift_in_distribution <- function(df_melted) {
    dSL_shift_dist <- df_melted |> filter(variable == 'result.4')


    hist_dsl_shift <- dSL_shift_dist |>
        ggplot(aes(x = value, fill = id, alpha = 0.5)) + 
        geom_histogram(color = "black", bins = 25, position = "identity") +
        theme_bw() +
        theme(
            legend.title = element_blank(),
            legend.position = 'bottom',
            legend.text = element_text(size = 10), # Change this for smaller text
            legend.key.size = unit(1.5, "lines"),  # Change this for smaller keys
            legend.background = element_rect(color = "black", linewidth = 0.15),  # Add a box with black border around the legend
            strip.background = element_blank(),  # Remove background from facet labels
            strip.text = element_blank()  # Remove facet labels
        ) +    
        labs(x = "Predicted probability", y = "Count") +
        scale_x_continuous(breaks = seq(0, 1, by = 0.5)) +
        scale_fill_discrete(labels = c("n = 150", "n = 500", "n = 1000", "n = 1500", "n = 3000")) +
        guides(alpha = "none") +
        facet_wrap(~ id, nrow = 1)

    ggsave("figures/preds_dsl_shift.pdf",
        plot = hist_dsl_shift,
        width = 10,
        height = 4,
        units = "in")
    hist_dsl_shift
}


fit_and_predict_dsl_1000_times_training_150_up_to_3000 <- function(K = 1000) {
    res_lib_diff_ns <- foreach(n = training_counts) %do% {
        print(n)
        preds <- foreach (k = 1:K, .combine = 'rbind') %do% {
            print(paste('Predicting for k =', k))
            train_set <- simulateMalariaData(n)
            fit <- fit_dSL(train_set)
            pred <- predict_dSL_names(fit, obs)

            as.data.frame(pred)
        }
        preds        
    }


    # Bind all dataframes into one and add an id column
    df_combined <- bind_rows(res_lib_diff_ns, .id = "id")
    
    hist_dsl_shift <- df_combined |> ggplot(aes(x = result, fill = selected_learner, alpha=0.5)) + 
        geom_histogram(color = "black", bins = 20, position = "stack") +
        theme_bw() +
        theme(
            legend.title = element_blank(),
            legend.position = 'bottom',
            legend.text = element_text(size = 10), # Change this for smaller text
            legend.key.size = unit(1.3, "lines"),  # Change this for smaller keys
            legend.background = element_rect(color = "black", linewidth = 0.15),  # Add a box with black border around the legend
            strip.background = element_blank()  # Remove background from facet labels
        ) +
        scale_x_continuous(breaks = seq(0, 1, by = 0.5)) +
        scale_fill_discrete(labels = c("Main effects", "XGBoost")) +
        guides(alpha = "none") +
        facet_wrap(~ id, nrow = 1, labeller = labeller(id = function(x) paste0("n = ", training_counts[as.numeric(x)]))) +
        labs(x = "Predicted probability", y = "Count") 

    ggsave("figures/preds_dsl_shift.pdf",
        plot = hist_dsl_shift,
        width = 10,
        height = 4,
        units = "in")
}

fit_and_predict_dsl_1000_times_training_150_up_to_3000()
