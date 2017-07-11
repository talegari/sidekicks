#' @title rm_na
#' @aliases remove_na
#' @description remove NA from a vector and return it
#' @param vec A vector
#' @return A vector without NA
#' @examples
#' rm_na(c(1,NA, 2, NA))
#' @export

rm_na = function(vec){

  stopifnot(is_vector(vec))

  index = !is.na(vec)
  if(sum(index) == 0)
    return(vector(mode = class(vec), length = 0L))
  else
    return( vec[index] )
}

remove_na = rm_na
