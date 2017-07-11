#' @title count_pattern
#' @description Count the number of elements of a vector that match the pattern
#' @param vec A character vector
#' @param pattern character string containing a regular expression.
#' @param perl TRUE or FALSE
#' @return An integer
#' @examples
#' count_pattern(c("aa", "ab", "ac"), "b")
#' @export

count_pattern = function(vec, pattern, perl = TRUE){

  stopifnot(class(vec) %in% c("character"))
  stopifnot(is.character(pattern) && length(pattern) == 1L)
  stopifnot(is.logical(perl) && length(pattern) == 1L)

  sum(grepl(pattern, vec, perl = perl))
}