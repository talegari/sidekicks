#' @title replace_na
#' @description Replace NA by something appropriate
#' @param object A vector /matrix / list / dataframe
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