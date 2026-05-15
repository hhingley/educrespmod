# Suppress R CMD check notes for dplyr column name references
utils::globalVariables(c(
  "user_id",
  "order_id",
  "is_test",
  "problem_id",
  "correct",
  "has_variation"
))
