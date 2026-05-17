#' Check all test problems appear in training data
#'
#' @description
#' For IRT models, item parameters can only be estimated for problems seen
#' during training. This function checks all problems in the test set also
#' appear in the training set.
#'
#' @param split_data A data frame output from \code{clean_single_response()}
#'
#' @return Logical, TRUE if all test problems appear in training
#' @export
check_all_probs <- function(split_data) {
  check_result <- length(unique(split_data[split_data$is_test == FALSE, ]$problem_id)) ==
    length(unique(split_data$problem_id))
  return(check_result)
}

#' Check all training problems have response variation
#'
#' @description
#' For IRT models, each problem must have at least one correct and one
#' incorrect response in the training set for item parameters to be estimated.
#'
#' @param split_data A data frame output from \code{clean_single_response()}
#'
#' @return Logical, TRUE if all problems have variation in responses
#' @export
has_problem_variation <- function(split_data) {
  train_data <- split_data |>
    dplyr::filter(is_test == FALSE)
  train_data |>
    dplyr::group_by(problem_id) |>
    dplyr::summarise(
      has_variation = dplyr::n_distinct(correct) > 1,
      .groups = "drop") |>
    dplyr::pull(has_variation) |>
    all()
}

#' Check minimum number of responses for a skill
#'
#' @description
#' Skills with very few responses may result in empty training sets.
#' This function checks the total number of responses exceeds a minimum.
#'
#' @param split_data A data frame output from \code{clean_single_response()}
#' @param minimum An integer giving the minimum number of responses required
#'
#' @return Logical, TRUE if the number of responses meets the minimum
#' @export
count_responses <- function(split_data, minimum) {
  if (nrow(split_data) < minimum) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Check all validation problems appear in the training data
#'
#' @description
#' For IRT models, item parameters can only be estimated for problems seen
#' during training. This function checks all problems in the validation set also
#' appear in the training set.
#'
#' @param split_data A data frame output from \code{clean_single_response()}
#'
#' @return Logical, TRUE if all validation problems appear in training
#' @export
check_all_probs_val <- function(split_data){
  check_result <- length(unique(split_data[split_data$is_validate == FALSE,]$problem_id)) == length(unique(split_data$problem_id))
  return(check_result)
}

#' Check all validation problems have variation in response
#'
#' @description
#' For IRT models, each problem must have at least one correct and one
#' incorrect response in the training set for item parameters to be estimated.
#'
#' @param split_data A data frame output from \code{clean_single_response()}
#'
#' @return Logical, TRUE if all validation problems have variation in responses
#' @export
has_problem_variation_val <- function(split_data) {
  train_data <- split_data |>
    dplyr::filter(is_validate == FALSE)
  train_data |>
    dplyr::group_by(problem_id) |>
    dplyr::summarise(
      has_variation = dplyr::n_distinct(correct) > 1,
      .groups = "drop") |>
    dplyr::pull(has_variation) |>
    all()
}
