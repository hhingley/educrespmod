#' Fit a BKT model and return predicts of the test set
#'
#' @description
#' Fits a Bayesian Knowledge Tracing (BKT) model to the training data,
#' returning predictions on the test set.
#'
#' @param data A data frame output from \code{clean_single_response()},
#'   containing an \code{is_test} column
#' @param model A BKT model object created using \code{BKT::bkt()}
#'
#' @return A data frame of test set rows with predicted probabilities
#' @export
bkt_predictions <- function(data, model) {
  train_data <- data[data$is_test == FALSE, ]
  bkt_model <- BKT::fit(model, data = train_data)
  all_preds <- BKT::predict_bkt(bkt_model, data = data)
  names(all_preds)[names(all_preds) == "correct_predictions"] <- "predicted"
  test_preds <- all_preds[all_preds$is_test == TRUE, ]
  return(test_preds)
}
