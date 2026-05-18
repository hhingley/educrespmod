#' Load and clean the (corrected and collapsed) ASSISTments 2009-2010 Skill Builder dataset
#'
#' @description
#' Loads the ASSISTments 2009-2010 Skill Builder dataset from a CSV file
#' and applies dataset-specific cleaning. This function is specific to
#' this dataset only and should not be used for other datasets.
#'
#' @param path A string giving the file path to the CSV file
#'
#' @return A cleaned data frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- load_assistments_skillbuilder_2009("path/to/skill_builder_data_corrected_collapsed.csv")
#' }
load_assistments_skillbuilder_2009 <- function(path) {
  df <- utils::read.csv(path)

  # Drop row index column
  df <- df[, names(df) != "X"]

  # Rename order_id to order_id_original
  names(df)[names(df) == "order_id"] <- "order_id_original"

  # Rename opportunity to order_id
  names(df)[names(df) == "opportunity"] <- "order_id"

  # Drop opportunity_original
  df <- df[, names(df) != "opportunity_original"]

  # Clean user_id to character
  df$user_id <- as.character(df$user_id)

  # Fix specific skill names
  df$skill_name[df$skill_name ==
                  "Order of Operations +,-,/,* () positive reals"] <-
    "Order_of_Operations_Positive_Reals"

  # Replace blank skill names
  df$skill_name[df$skill_name == ""] <- "blank"

  return(df)
}
