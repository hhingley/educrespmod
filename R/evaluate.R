#' Calculate cross entropy loss/loss using the logarithmic score rule
#'
#' @description
#' Calculates the mean cross entropy loss (log loss) for a set of predicted
#' probabilities against true binary outcomes. Predictions are bounded away
#' from 0 and 1 to avoid extreme loss values.
#'
#' @param test_preds A numeric vector of predicted probabilities
#' @param test_outcomes A numeric vector of true binary outcomes (0 or 1)
#' @param bound A numeric value specifying the bounding threshold. Predictions
#'   below \code{bound} are set to \code{bound} and above \code{1-bound} are
#'   set to \code{1-bound}. Defaults to 0.
#'
#' @return A single numeric value giving the mean cross entropy loss for all predictions on the test
#' @export
loss_calc <- function(test_preds, test_outcomes, bound = 0) {
  test_preds <- pmax(bound, pmin(1 - bound, test_preds))
  losses <- -((test_outcomes * log(test_preds)) + ((1 - test_outcomes) * log(1 - test_preds)))
  return(mean(losses))
}
