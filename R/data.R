#' Simulated continuous data
#'
#' A dataset containing artificial continuous data of 500 samples and 12
#' variables. The variables are either independent (A, B), duplicated (C, D) or
#' tripled (E, F), each with additional noise so simulate copies of the same
#' variable
#'
#' The group structure looks as follows:
#' Group1 - A
#' Group2 - B
#' Group3 - C1, C2
#' Group4 - D1, D2
#' Group5 - E1, E2, E3
#' Group6 - F1, F2, F3
#' @format A data frame with 2000 rows and 12 variables:
#'
"X_n500"

#' Simulated discrete data
#'
#' A dataset containing artificial discrete data of 500 samples and 3
#' variables.
#' @format A data frame with 2000 rows and 3 variables:
"Y_n500"

#' Baseline dataset (precision matrix) used for simulating X_n500.rds and
#' Y_n500.rda
#'
"adjmat_truth"
