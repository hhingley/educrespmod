#' Addresses repeated interactions between students and items
#'
#'
#' @description The training data will contain repeated interactions between the same student and same item.
#' The collapse_train function will address these observations, combining all interactions between the same items and students
#' When a student always answers correctly they receive entry 2. When they got it correct at least once, they receive entry 1.
#' If never correct entry is 0,
#'
#' @param train_long The training data in long format
#'
#'
#' @return The training data collapsed, so repeat attempts are not included
#'
collapse_train <- function(train_long){
  collapsed_data <- train_long |>
    dplyr::group_by(user_id, problem_id) |>
    dplyr::summarise(n_attempts = dplyr::n(),
              n_correct = sum(correct),
              .groups = "drop") |>
    dplyr::mutate(
      score = as.integer(factor(
        dplyr::case_when(
          n_correct == 0 ~ 0,
          n_correct == n_attempts ~ 2,
          TRUE ~ 1
        ),
        levels = 0:2,
        ordered = TRUE
      )
      ))
  return(collapsed_data)
}

#' Converts the collapsed training data into wide format
#'
#'
#' @description To fit IRT models in the mirt package, the data we fit our model to must be in wide format.
#' The train_wider function converts the collapsed training data to the wide format
#'
#' @param collapsed_train The collapsed training data
#'
#'
#' @return The collapsed training data in wide format
#'
train_wider <- function(collapsed_train){
  collapsed_train <- collapsed_train |>
    dplyr::select(user_id, problem_id, score) |>
    tidyr::pivot_wider(
      names_from  = problem_id,
      values_from = score,
      values_fill = NA)
  return(collapsed_train)
}

#' Converts the collapsed training data into wide format
#'
#'
#' @description To fit IRT models in the mirt package, the data we fit our model to must be in wide, matrix format.
#' The train_wide_matrix function converts the collapsed wider training data to a matrix format
#'
#' @param train_wide The collapsed training data in wide format
#'
#'
#' @return The training data in matrix form
#'
train_wide_matrix <- function(train_wide){
  irt_matrix <- train_wide |>
    tibble::column_to_rownames("user_id") |>
    as.matrix()
  return(irt_matrix)
}

#' Fits an IRT model where no pattern is specified, but type of IRT model is
#'
#'
#' @description Fits an IRT model using mirt package given an input matrix and specified model type
#'
#' @param data A data frame output from \code{clean_single_response()},
#'   containing an \code{is_test} column
#' @param itemtype The specified type of IRT model, default GPCM
#'
#' @return The fitted IRT model
#'
#' @export
fit_irt <- function(data, itemtype = "gpcm"){
  train_data <- data[data$is_test == FALSE, ]
  irt_collapse <- collapse_train(train_data)
  irt_wide <- train_wider(irt_collapse)
  irt_matrix <- train_wide_matrix(irt_wide)
  mod <- mirt::mirt(irt_matrix,1,itemtype = itemtype)
  return(mod)
}

#' Fits an IRT model with a specified pattern
#'
#'
#' @description Fits an IRT model using mirt package given an input matrix and specified pattern
#'
#' @param data A data frame output from \code{clean_single_response()},
#'   containing an \code{is_test} column
#' @param pattern A specified pattern for an IRT model
#' @param itemtype The specified type of IRT model, default GPCM
#'
#' @return The fitted IRT model
#'
#' @export
fit_irt_pattern <- function(data, pattern, itemtype = "gpcm"){
  train_data <- data[data$is_test == FALSE, ]
  irt_collapse <- collapse_train(train_data)
  irt_wide <- train_wider(irt_collapse)
  irt_matrix <- train_wide_matrix(irt_wide)
  mod <- mirt::mirt(irt_matrix,1,itemtype=itemtype,pars=pattern)
  return(mod)
}

#' Fits an IRT model with a specified pattern in matrix form
#'
#'
#' @description Fits an IRT model using mirt package given an input matrix (in matrix form) and specified pattern
#'
#' @param irt_matrix A matrix output to fit IRT model to
#' @param pattern A specified pattern for an IRT model
#' @param itemtype The specified type of IRT model, default GPCM
#'
#' @return The fitted IRT model
#'
fit_irt_pattern_matrix <- function(irt_matrix, pattern, itemtype = "gpcm"){
  mod <- mirt::mirt(irt_matrix,1,itemtype=itemtype,pars=pattern)
  return(mod)
}

