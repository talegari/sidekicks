#' @title sum_to
#' @description Scale a vector so that it sums to some positive number
#' @param vec A non-negative integer or numeric vector without NA
#' @param to A positive number
#' @return A vector that sums to 'to'
#' @examples
#' sum_to(c(1,2,3), 100)
#' @export

sum_to = function(vec, to = 100L){

  stopifnot(!anyNA(vec))
  stopifnot(inherits(vec, "integer") || inherits(vec, "numeric"))
  stopifnot(length(vec) > 0L)
  stopifnot(all(vec >= 0L))
  stopifnot(!all(vec == 0))
  stopifnot(length(to) == 1L)
  stopifnot(to > 0)

  (vec/sum(vec)) * to
}