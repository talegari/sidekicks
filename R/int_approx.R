#' @title int_approx
#' @description Produce an integer vector approximation of a non-increasing
#'   non-negative numeric vector such that it sums to an positive integer
#' @param vec non-decreasing non-negative numeric vector
#' @param n A positive integer to which vector should sum to. Default value is
#'   the rounded sum of the vector
#' @return An integer vector
#' @details The numeric vector is scaled to sum to n. Then, an integer vector is
#'   assigned the integer parts of the scaled vector. The leftout part of the
#'   sum is distributed based on the magnitude of the fractional part. This kind
#'   of approximation is useful when an algorithm produces ranking scores which
#'   have to allocated with positive integer constraints.
#' @examples
#' int_approx(c(3.7, 3.2, 2.4, 1.5))
#' @export

int_approx = function(vec, n){

  stopifnot(is.numeric(vec) || is.integer(vec))
  stopifnot(all(vec >= 0))
  stopifnot(identical(unname(rank(vec, ties.method = "last")), length(vec):1))
  if(missing(n)){
    n = round(sum(vec))
  } else {
    stopifnot(is.numeric(n) && n > 0 && round(n) == n && length(n) == 1)
  }

  ratios   = (vec/sum(vec)) * n
  intpart  = floor(ratios)
  fracpart = ratios - intpart
  leftout  = n - sum(intpart)
  if(leftout != 0){
    addat = order(fracpart, length(vec):1, decreasing = TRUE)[1:leftout]
    intpart[addat] = intpart[addat] + 1
  }
  names(intpart) = names(vec)
  return(intpart)
}
