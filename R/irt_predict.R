#' Appends the estimated abilities of each user in the training data to the training data
#'
#'
#' @description given a fitted IRT model and the wide training data the model was fitted to
#' append the ability estimate for the user to the row
#'
#' @param train_wide the wide training data (collapsed)
#' @param mod the fitted IRT model (to the training data)
#'
#'
#' @return the wide training data with ability appended
#'
append_abilities <- function(train_wide, mod){
  train_wide <- as.data.frame(train_wide)
  user_abilities <- mirt::fscores(mod, simplify=TRUE)
  train_wide$ability <- user_abilities[,"F1"]
  return(train_wide)
}

#' Return all item parameter estimates for items in the training data
#'
#'
#' @description given a fitted IRT model, extract all item parameters
#'
#' @param mod the fitted IRT model (to the training data)
#'
#'
#' @return the fitted item parameters
#'
extract_item_info <- function(mod){
  item_params <- mirt::coef(mod, simplify=TRUE)
  item_params <- item_params$items
  return(item_params)
}

#' Append fitted item parameters to the test data #
#'Currently only supports 2PL GPCM - extend for other model types later
#'
#'
#' @description given a set of test data, and a list of item parameters
#' append the item parameters for the relevant item in each row to that row
#'
#' @param test_data the test data
#' @param item_params the fitted item parameters
#'
#'
#' @return the test data with appended item parameters
#'
append_to_test <- function(test_data,item_params){
  test_data$a1 <- rep(NA_real_, nrow(test_data))
  test_data$ak0 <- rep(NA_real_, nrow(test_data))
  test_data$ak1 <- rep(NA_real_, nrow(test_data))
  test_data$d0 <- rep(NA_real_,nrow(test_data))
  test_data$d1 <- rep(NA_real_, nrow(test_data))
  test_data$ak2 <- rep(NA_real_, nrow(test_data))
  test_data$d2 <- rep(NA_real_, nrow(test_data))
  for (i in 1:nrow(test_data)){
    item <- as.character(test_data$problem_id[i])
    test_data$a1[i] <- item_params[item,1]
    test_data$ak0[i] <- item_params[item,2]
    test_data$ak1[i] <- item_params[item,3]
    test_data$d0[i] <- item_params[item,4]
    test_data$d1[i] <- item_params[item,5]
    test_data$ak2[i] <- item_params[item,6]
    test_data$d2[i] <- item_params[item,7]
  }
  return(test_data)
}

#' Appends a repeat column to tell us whether repeats for an item have occurred
#'
#'
#' @description given test and training data, looks to see if the observation in test data is a repeat
#'
#' @param test_data the test data
#' @param train_long the UNCOLLAPSED training data
#'
#'
#' @return the test data with a new column indicating if it's a repeat
#'
append_repeats <- function(test_data,train_long){
  prev_attempt_summary <- train_long |>
    dplyr::group_by(user_id, problem_id) |>
    dplyr::summarise(
      any_wrong = any(correct == 0),
      .groups = "drop"
    )
  test_data <- test_data |>
    dplyr::left_join(
      prev_attempt_summary,
      by = c("user_id", "problem_id")
    )
  return(test_data)
}

#' Makes IRT predictions on the test data
#'
#'
#' @description Makes IRT predictions on the test data given we have the information about item parameters and ability
#' in this test data
#' @param test_long the test data in long format
#'
#'
#' @return the test data with an extra predicted column
#'
make_irt_predictions <- function(test_long){
  for (i in 1:nrow(test_long)){
    is_gpcm <- !is.na(test_long$d2[i])
    if (!is_gpcm) {
      test_long$predicted[i] <-  1 / (1 + exp(-((test_long$a1[i] * test_long$ability[i]) + test_long$d1[i])))
    }
    else{
      if (is.na(test_long$any_wrong[i]) || !test_long$any_wrong[i]) {
        #category 2
        test_long$predicted[i] <-
          exp((2 * test_long$a1[i] * test_long$ability[i]) + test_long$d2[i]) /
          (exp(0) +
             exp((1 * test_long$a1[i] * test_long$ability[i]) + test_long$d1[i]) +
             exp((2 * test_long$a1[i] * test_long$ability[i]) + test_long$d2[i]))
      } else {
        #category 1
        test_long$predicted[i] <-
          exp((1 * test_long$a1[i] * test_long$ability[i]) + test_long$d1[i]) /
          (exp(0) +
             exp((1 * test_long$a1[i] * test_long$ability[i]) + test_long$d1[i]) +
             exp((2 * test_long$a1[i] * test_long$ability[i]) + test_long$d2[i]))
      }
    }
  }
  return(test_long)
}


#' Prediction function for making predictions on the test data given an IRT model
#'
#'
#' @description given a fitted IRT model and the data it was fitted to, makes predictions on the test data
#'
#' @param data all data with a specified split
#' @param mod the fitted IRT model (to the training data)
#'
#'
#' @return the test data with an appended prediction
#'
#' @export
irt_predictions <- function(data,mod){
  train_data <- data[data$is_test == FALSE, ]
  test_data <- data[data$is_test == TRUE, ]
  irt_collapse <- collapse_train(train_data)
  irt_wide <- train_wider(irt_collapse)
  irt_wide <- append_abilities(irt_wide,mod)
  ability_lookup <- irt_wide |>
    dplyr::select(user_id, ability)
  test_data <- test_data |>
    dplyr::left_join(ability_lookup, by = "user_id")
  item_params <- extract_item_info(mod)
  test_data <- append_to_test(test_data,item_params)
  test_data <- append_repeats(test_data, train_data)
  test_data$predicted <- rep(0, nrow(test_data))
  test_data <- make_irt_predictions(test_data)
  return(test_data)
}




