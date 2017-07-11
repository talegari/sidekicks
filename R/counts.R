#' @title counts
#' @description Obtain counts of an atomic vector as a named integer vector
#' @param vec An atomic vector
#' @return A named integer vector
#' @examples
#' counts(c(1,2,2,2,1,NA))
#' @export
counts = function(vec){

  stopifnot(is_vector(vec))

  vec_tabulated    = table(vec)
  vec_count        = as.vector(vec_tabulated)
  names(vec_count) = names(vec_tabulated)

  vec_count
}