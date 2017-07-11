#' @title rotate
#' @description rotate a vector by given number of positions forward or backwards
#' @param vec A vector
#' @param by An integer indicating number of positions. Default is 1L
#' @return A vector after rotating it
#' @examples
#' rotate(1:10, 2)
#' rotate(letters, -5)
#' @export

rotate = function(vec, by = 1L){

  stopifnot(length(vec) > 1)
  stopifnot(is_vector(vec))
  stopifnot(length(by) == 1)
  stopifnot(is.integer(by) || (is.numeric(by) && round(by) == by))

  ell = length(vec)
  if(by < 0)
    by = ell - (abs(by) %% ell)

  vec[(((0:(ell - 1)) + by) %% ell) + 1]
}