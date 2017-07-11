#' @title unique_fraction
#' @aliases fraction_unique
#' @description fraction of unique values in a vector
#' @param vec A vector
#' @param na.rm Whether to consider NAs as a distinct element
#' @return A numeric between 0 and 1
#' @examples
#' unique_fraction(c(1, 1, 2, 2, 2))
#' unique_fraction(c(1, 1, 2, 2, NA, 2, NA))
#' unique_fraction(c(1, 1, 2, 2, NA, 2, NA), na.rm = FALSE)
#' @export

unique_fraction = function(vec, na.rm = TRUE){

  stopifnot(is_vector(vec))
  stopifnot(length(na.rm) == 1 && is.logical(na.rm))

  dplyr::n_distinct(vec, na.rm = na.rm)/length(vec)
}

fraction_unique = unique_fraction
