#' Change Pattern function
#'
#' @description
#' Changes the pattern to be used when fitting the IRT mode.
#' Allows for prior distributions to be set to be set with specific hyper-parameters
#'
#' @param pattern The original pattern to be modified
#' @param disc_mean hyper-parameter for the mean of the discrimination parameter's prior
#' @param disc_var hyper-parameter for the variance of the discrimination parameter's prior
#' @param diff1_mean hyper-parameter for the mean of the first difficulty parameter's parameter's prior
#' @param diff1_var hyper-parameter for the variance of the first difficulty parameter's parameter's prior
#' @param diff2_mean hyper-parameter for the mean of the second difficulty parameter's parameter's prior
#' @param diff2_var hyper-parameter for the variance of the second difficulty parameter's parameter's prior
#' @return The modified response pattern with prior distributions specified from inputted hyper-parameters
change_pattern <- function(pattern, disc_mean,disc_var, diff1_mean,diff1_var,diff2_mean,diff2_var){
  for(i in 1:nrow(pattern)){
    if(pattern$name[i] == "a1"){
      pattern$prior.type[i] = "lnorm"
      pattern$prior_1[i] = disc_mean
      pattern$prior_2[i] = disc_var
    }
    if(pattern$name[i] == "d1"){
      pattern$prior.type[i] = "norm"
      pattern$prior_1[i] = diff1_mean
      pattern$prior_2[i] = diff1_var
    }
    if(pattern$name[i] == "d2"){
      pattern$prior.type[i] = "norm"
      pattern$prior_1[i] = diff2_mean
      pattern$prior_2[i] = diff2_var
    }
    if(pattern$est[i] == TRUE){ #change the starting value for parameters, if discrimination, start at 1, for difficulties start at 0 (after running this probably not a good idea to start at 0 for difficulties, as in reality they end at values like 3 or 6)
      if(pattern$name[i] == "a1"){
        pattern$value[i] = 1
      }
      else{
        pattern$value[i] = 0
      }
    }
  }
  return(pattern)
}


#' Calculate losses on the tuning grid
#'
#'
#' @description For a given tuning grid, and a validation split of the data
#' calculates the loss on each combination of hyper-parameters in the tuning grid
#'
#' @param validate_split the validation split of the data
#' @param param_grid set of tuning parameters in grid
#' @param bound the bound on the probabilities when using the logarithmic scoring rule
#' default set at 0
#'
#'
#' @return the parameter grid with the associated loss for each combination of hyper-parameters
#' when fitted to the validation data
#'
#' @export
loss_tune_irt <- function(validate_split,param_grid,bound = 0){
  for (i in 1:nrow(param_grid)){
    tune_uncollapsed <- validate_split |>
      dplyr::filter(is_validate == FALSE)
    tune_collapsed <- collapse_train(tune_uncollapsed)
    tune_wide <- train_wider(tune_collapsed)
    tune_matrix <- train_wide_matrix(tune_wide)
    temp_model <- suppressMessages(suppressWarnings(mirt::mirt(tune_matrix,1, itemtype = "gpcm", technical = list(NCYCLES=1))))
    pattern <- mirt::mod2values(temp_model)
    pattern <- change_pattern(pattern, param_grid$disc_mean[i], param_grid$disc_var[i], param_grid$diff1_mean[i],param_grid$diff1_var[i],param_grid$diff2_mean[i],param_grid$diff2_var[i])
    fitted_tune <- suppressMessages(suppressWarnings(fit_irt_pattern_matrix(tune_matrix, pattern)))
    if(ncol(mirt::coef(fitted_tune,simplify=TRUE)$items) < 7){
      param_grid$loss[i] <- NA
    }
    else{
      tune_matrix <- append_abilities(as.data.frame(tune_matrix),fitted_tune)
      tuned_params <- extract_item_info(fitted_tune)
      validate_set <- validate_split |>
        dplyr::filter(is_validate == TRUE)
      ability_lookup <-tune_matrix |>
        dplyr::select(user_id, ability)
      validate_set <- validate_set |>
        dplyr::left_join(ability_lookup, by = "user_id")
      validate_set <- append_to_test(validate_set, tuned_params)
      validate_set <- append_repeats(validate_set, tune_uncollapsed)
      validate_set$predicted <- rep(0, nrow(validate_set))
      validate_set <- make_irt_predictions(validate_set)
      param_grid$loss[i] <- loss_calc(validate_set$predicted, validate_set$correct, bound = bound)
    }
  }
  return(param_grid)
}
