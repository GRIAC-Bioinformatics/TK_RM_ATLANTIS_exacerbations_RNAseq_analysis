# =============================================================================
# cv_functions.R
# Cross-validation and metric summarisation functions for exacerbation models
#
# Usage: source("cv_functions.R")
# =============================================================================

library(caret)
library(pROC)
library(dplyr)
library(tibble)
library(doParallel)



# 1. Run one CV model 
#' Fit a logistic regression model with repeated CV via caret
#'
#' @param formula      Model formula (outcome must be `exacerbation_f`)
#' @param data         Prepared data frame (output of prepare_cv_data)
#' @param train_ctrl   trainControl object (output of make_train_control)
#' @param seed         Random seed for reproducibility (default 42)
#' @return A trained caret object
run_cv_model <- function(formula, data, train_ctrl, seed = 42) {
  set.seed(seed)
  train(
    formula,
    data      = data,
    method    = "glm",
    family    = binomial,
    trControl = train_ctrl,
    metric    = "ROC"
  )
}


# 2. Run all models in parallel 
#' Wrapper that registers a parallel cluster, trains all models, then cleans up
#'
#' @param model_list   Named list of formulas, e.g.
#'                     list(Clinical = formula1, "Clinical + GSVA" = formula2)
#' @param data         Prepared data frame
#' @param train_ctrl   trainControl object
#' @param seed         Random seed (default 42)
#' @param n_cores      Number of cores (default: all - 1)
#' @return Named list of trained caret objects, one per model
run_all_cv_models <- function(model_list,
                              data,
                              train_ctrl,
                              seed     = 42,
                              n_cores  = parallel::detectCores() - 1) {
  cl <- makePSOCKcluster(n_cores)
  registerDoParallel(cl)
  on.exit(stopCluster(cl), add = TRUE)   # always clean up, even on error
  
  lapply(model_list, function(frm) {
    run_cv_model(frm, data, train_ctrl, seed)
  })
}


# 3a. Build all incremental CV models 
#' Run CV for all predictor sets in one parallel batch
#'
#' Keeps the parallel cluster alive for all models instead of
#' restarting it once per predictor set.
#'
#' @param predictor_sets  Named list of character vectors
#' @param data            model_table (logical `exacerbation_f` column)
#' @param train_ctrl      trainControl from make_train_control()
#' @param seed            Random seed (default 42)
#' @return Named list of trained caret objects, one per predictor set
run_incremental_cv_models <- function(predictor_sets,
                                      data_cv,
                                      train_ctrl,
                                      seed = 42) {
  # build named formula list — same pattern as run_all_cv_models()
  formula_list <- lapply(predictor_sets, function(preds) {
    as.formula(paste("exacerbation_f ~", paste(preds, collapse = " + ")))
  })
  
  # one parallel batch for all incremental models
  run_all_cv_models(formula_list, data_cv, train_ctrl, seed)
}

# 3b. Summarise incremental CV results into a comparison table 
#' Summarises mean and 95% empirical CI for AUC, Sensitivity, Specificity, and
#' Balanced Accuracy across all CV resamples for each incremental predictor set.
#'
#' @param predictor_sets  Named list of character vectors (one per model step);
#'                        names must match those of cv_models
#' @param cv_models       Named list of trained caret objects returned by
#'                        run_incremental_cv_models()
#' @return A tibble with one row per predictor set and columns for each metric
#'         with its mean and 95% CI
build_incremental_cv_table <- function(predictor_sets,
                                       cv_models) {
  bind_rows(lapply(names(predictor_sets), function(model_id) {
    
    resamp <- cv_models[[model_id]]$resample %>%
      mutate(BalancedAcc = (Sens + Spec) / 2)
    
    tibble(
      Model      = model_id,
      Predictors = paste(predictor_sets[[model_id]], collapse = ", "),
      
      AUC                    = round(mean(resamp$ROC), 2),
      AUC_CI                 = ci_fmt(list(
        lo = round(quantile(resamp$ROC, 0.025), 2),
        hi = round(quantile(resamp$ROC, 0.975), 2))),
      Sensitivity            = round(mean(resamp$Sens), 2),
      Sensitivity_CI         = ci_fmt(list(
        lo = round(quantile(resamp$Sens, 0.025), 2),
        hi = round(quantile(resamp$Sens, 0.975), 2))),
      Sensitivity_CI_low     = round(quantile(resamp$Sens, 0.025), 2),
      Sensitivity_CI_high    = round(quantile(resamp$Sens, 0.975), 2),
      Specificity            = round(mean(resamp$Spec), 2),
      Specificity_CI         = ci_fmt(list(
        lo = round(quantile(resamp$Spec, 0.025), 2),
        hi = round(quantile(resamp$Spec, 0.975), 2))),
      Balanced_Accuracy      = round(mean(resamp$BalancedAcc), 2),
      Balanced_Accuracy_CI   = ci_fmt(list(
        lo = round(quantile(resamp$BalancedAcc, 0.025), 2),
        hi = round(quantile(resamp$BalancedAcc, 0.975), 2)))
    )
  }))
}

# Formatting helper
ci_fmt <- function(ci) paste0("[", ci$lo, "–", ci$hi, "]")
