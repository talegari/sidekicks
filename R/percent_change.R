#' @title percent_change
#' @description percentage change of a vector from something, typically the
#'   first element
#' @param vec An integer or numeric vector
#' @param from An integer or a number
#' @return A integer or numeric vector indicating percentage change
#' @examples
#' percent_change(c(2,1,3), from = 1)
#' @export
percent_change = function(vec, from = vec[1]){

  stopifnot((is.numeric(vec) || is.integer(vec)) && length(vec) > 0)
  stopifnot((is.numeric(from) || is.integer(from)) && length(from) == 1L)

  ((vec - from)/from) * 100L
}
