#' @title count_na
#' @description Count the number of NA
#' @param vec A vector
#' @return An integer
#' @examples
#' count_na(c(1,NA, 2, NA, 3))
#' @export

count_na = function(vec){

  stopifnot(is_vector(vec))

  sum(is.na(vec))

}
