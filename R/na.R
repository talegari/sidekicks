#' @title count_na
#' @description Count the number of NA
#' @param x A vector or matrix or array
#' @return An integer
#' @examples
#' count_na( c(1,NA, 2, NA, 3) )
#' count_na( array(1:64, dim = c(4,4,4)) )
#' @export

count_na = function(x){
  sum(is.na(x))
}

#' @title prop_na
#' @description Ratio of NA to non NA
#' @param x A vector or matrix or array
#' @return numeric
#' @examples
#' prop_na(c(1,NA, 2, NA, 3))
#' @export

prop_na = function(x){
  mean(is.na(x))
}

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

#' @title replace_na
#' @description Replace NA by something appropriate
#' @param object A vector or matrix or list or dataframe or array
#' @param by Something appropriate to replace by
#' @return Object after NA are replaced
#' @examples
#' replace_na(c(1, 2, NA, 4), 3)
#' @export

replace_na = function(object, by){

  stopifnot(any(class(object) %in% c("integer"
                                     , "numeric"
                                     , "character"
                                     , "logical"
                                     , "raw"
                                     , "matrix"
                                     , "list"
                                     , "data.frame")))

  object[which(is.na(object))] = by
  object
}
