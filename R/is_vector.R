#' @title is_vector
#' @description Checks for a vector of one of these types: c("integer",
#'   "numeric", "character", "logical", "raw", "complex")
#' @param vec A vector
#' @examples
#' is_vector(1:5)
#' is_vector(list(1:5))
#' is_vector(matrix(1:4, 2))
#' @export

is_vector = function(vec){
  types = c("integer", "numeric", "character", "logical", "raw", "complex")
  any(sapply(types, function(x) inherits(vec, x)))
}