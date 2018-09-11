#' @name na_approx
#' @title Linear interpolation of a vector
#' @description Linear interpolation of NAs in a vector using `zoo::na.approx`.
#'   The NAs at the beginning or at the end are replaced by their first non NA
#'   neighbour
#' @param vec A numeric vector with NAs
#' @return A numeric vector without NA
#' @examples
#' na_approx(c(NA, 2, 3, NA, 5, 6, NA))
#' @export
na_approx <- function(vec){

  replaced <- zoo::na.approx(vec)

  if(length(replaced) < length(vec)){
    filled <- which(!is.na(vec))
    minf   <- min(filled)
    maxf   <- max(filled)

    replaced <- c(rep(vec[minf], minf - 1), replaced)
    replaced <- c(replaced, rep(vec[maxf], length(vec) - length(replaced)))
  }

  return(replaced)

}