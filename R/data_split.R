#' Extract data for a single skill
#'
#' @param data A data frame of the full ASSISTments dataset
#' @param skill A string giving the skill name to extract
#'
#' @return A data frame filtered to the specified skill
#' @export
extract_skill <- function(data, skill) {
  extracted_skill <- data[data$skill_name == skill, ]
  return(extracted_skill)
}

#' Split data into training and test sets using leave-last-out
#'
#' @description
#' For each student, their final response (based on maximum order_id) is
#' placed in the test set. All other responses form the training set.
#'
#' @param data A data frame for a single skill
#'
#' @return A data frame with an additional logical column \code{is_test}
#' @export
test_split <- function(data) {
  split_data <- data |>
    dplyr::group_by(user_id) |>
    dplyr::mutate(is_test = order_id == max(order_id)) |>
    dplyr::ungroup()
  return(split_data)
}

#' Remove students with only one response for a skill
#'
#' @description
#' Students with only one response will have no training data after the
#' leave-last-out split. This function removes those students entirely.
#'
#' @param split_data A data frame output from \code{test_split()}
#'
#' @return A cleaned data frame
#' @export
clean_single_response <- function(split_data) {
  train_users <- split_data |>
    dplyr::filter(is_test == FALSE) |>
    dplyr::pull(user_id) |>
    unique()
  split_data |>
    dplyr::filter(is_test == FALSE | user_id %in% train_users)
}
#' Split training data into tuning and validation sets
#'
#' @description
#' For tuning IRT hyperparameters, further splits the training data into a
#' tuning set and validation set. Each student's last response goes into
#' the validation set. Students with only one response are kept in the
#' tuning set only and contribute no validation data.
#'
#' @param split_data A data frame output from \code{clean_single_response()}
#'
#' @return A data frame of training data with an additional logical column
#'   \code{is_validate}
#' @export
validate_split <- function(split_data) {
  split_data |>
    dplyr::filter(is_test == FALSE) |>
    dplyr::group_by(user_id) |>
    dplyr::mutate(
      n_resp = dplyr::n(),
      is_validate = n_resp > 1 & order_id == max(order_id, na.rm = TRUE)
    ) |>
    dplyr::ungroup()
}
