#' @name encode
#' @title encode a list with base64 encoding at first depth
#' @description encode a list with base64 encoding
#' @param x A listor a vector(essentially an iterable)
#' @return A character vector
encode <- Vectorize(function(x){
  serialize(x, connection = NULL) %>% base64enc::base64encode()
})

#' @name decode
#' @title decode base64 encoded object as a list
#' @description encode a list with base64 encoding
#' @param x A encoded character vector
#' @return A list or vector depending on the encoded object
decode <- Vectorize(function(x){
  base64enc::base64decode(x) %>%
    unserialize() %>%
    unname()
})

#' @name encodeListColumns
#' @title Encode list-columns in a dataframe as character using base64 encoding
#' @description Encoded columns are detected, encoded with base64 and column
#'   names are appended with "__encoded" so that the decoder knows it. Other
#'   columns are not transformed
#' @details This is helpful in writing dataframes with list-columns to disk as
#'   delimited formats and read them back
#' @param df dataframe
#' @return A dataframe where list-columns are encoded using base64 and those
#'   column names are appended by "__encoded"
#' @examples
#' library("magrittr")
#' trialDF <- tibble::tibble(x = 1:10
#'                           , y = c(list(list(iris)), as.list(2:10))
#'                           )
#' trialDF
#' trialDF %>% encodeListColumns()
#' trialDF %>% encodeListColumns() %>% decodeListColumns()
#' @export
encodeListColumns <- function(df){
  df %>%
    dplyr::rename_if(function(col) inherits(col, "list")
                     , function(name) paste0(name, "__encoded")
                     ) %>%
    dplyr::mutate_if(function(col) inherits(col, "list"), encode)
}

#' @name decodeListColumns
#' @title Decode the encoded list-columns in a dataframe using 'encodeListColumns' function
#' @description Encoded columns are detected, encoded with base64 and column names are appended with "__encoded" so that the decoder knows it. Other columns are not transformed. The 'decodeListColumns' function decodes only the columns whose names end with "__encoded"
#' @param df dataframe
#' @return A dataframe where list-columns are encoded using base64 and those column names are appended by "__encoded". Those columns are decoded and "__encoded" is removed.
#' @export
decodeListColumns <- function(df){
  df %>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("__encoded")), decode) %>%
    dplyr::rename_at(dplyr::vars(dplyr::ends_with("__encoded"))
              , function(name) stringr::str_split(name, pattern = "__")[[1]][1]
              )
}
