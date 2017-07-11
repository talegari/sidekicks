#' @title cutq
#' @aliases  quantilecut
#' @description Discretize a numeric vector along quantiles
#' @param vec numeric/integer vector
#' @param n number of buckets (atleast two)
#' @param ... extra named arguments passed to `cut`
#' @return A factor
#' @details By passing extra arguments to `cut`, output can be styled

cutq = function(vec, n = 10, ...){
  stopifnot(inherits(vec, "numeric") || inherits(vec, "integer"))
  stopifnot(length(n) == 1 && round(n) == n && n > 1 && n <= length(vec))

  cut(vec, quantile(vec, seq(0, 1, 1/n), na.rm = TRUE), ...)
}

quantilecut = cutq