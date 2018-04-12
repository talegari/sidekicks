#' @name counts
#' @title Obtain counts of an atomic vector as a named integer vector.
#' @description NA as considered valid and their counts are calculated.
#' @param vec An atomic vector
#' @return A named integer vector
#' @examples
#' counts(c(1,2,2,2,1,NA))
#' @export

counts = function(vec){

  assertthat::assert_that(is_vector(vec))

  x                <- NULL
  xdf              <- data.frame(x = vec)
  countdf          <- dplyr::count(xdf, x)
  vec_count        <- countdf[["n"]]
  names(vec_count) <- countdf[["x"]]

  return( vec_count )
}

#' @name freq
#' @title Obtain frequencies of an atomic vector as proportions.
#' @description NA as considered valid and their proportions are calculated.
#' @param vec An atomic vector
#' @return A vector of frequencies as proportions with same length as the
#'   vector.
#' @examples
#' freq(c(1,2,2,2,1,NA))
#' @export

freq <- function(vec){

  n     <- NULL
  x     <- NULL
  xdf   <- data.frame(x = vec)
  props <- xdf %>%
    dplyr::count(x) %>%
    dplyr::mutate(n = n/nrow(xdf))

  return( dplyr::full_join(xdf, props, by = "x")[["n"]] )
}