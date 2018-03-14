#' @name toString_to_characterString
#' @title Split a long delimted string into individual strings and use then to construct a character vector.
#' @description Usually helpful in setting extracting levels elsewhere in the code.
#' @return A string ready to constructed.
#' @param string Long string with delimters, may be output of 'toString'.
#' @param sep delimiter. Default is ",".
#' @param stripWhitespace Whether to trim whitespace on both sides of resulting strings.
#' @examples
#' concat <- toString(colnames(iris))
#' print(concat)
#' toString_to_characterString(concat)
#' eval(parse(text = toString_to_characterString(concat)))
#' @export
toString_to_characterString <- function(string, sep = ",", stripWhitespace = TRUE){

  string_split <- unlist(strsplit(string, split = sep))
  if(stripWhitespace){
    string_split <- vapply(string_split, trimws, character(1))
  }
  string_split <-
    paste0(
      "c("
      , paste0(paste0("'", string_split, "'"), collapse = ", ")
      , ")"
      , sep = ""
  )

  return(string_split)
}

#' @name toString_to_character
#' @title Split a long delimted string into individual strings.
#' @description Usually helpful in setting extracted levels.
#' @return A string.
#' @param string Long string with delimters, may be output of 'toString'.
#' @param sep delimiter. Default is ",".
#' @param stripWhitespace Whether to trim whitespace on both sides of resulting strings.
#' @examples
#' concat <- toString(colnames(iris))
#' print(concat)
#' toString_to_character(concat)
#' @export
toString_to_character <- function(string, sep = ",", stripWhitespace = TRUE){

  string_split <- unlist(strsplit(string, split = sep))
  if(stripWhitespace){
    string_split <- vapply(string_split, trimws, character(1))
  }

  return(string_split)
}
