#' @name encodeCharColumns
#' @title Encode character columns.
#' @description Encode a subset of character columns using base64 encoding.
#' @details Texual data in dataframes with comman seperators like ',' create problems writing to disk and reading back. This function encodes the a subset of character columns using base64 encoding where the resulting dataframe might be written to disk with ',' separator. The function 'decodeCharCols' decodes the encoded character columns are restores back the actual character columns.
#' @param df A dataframe
#' @param columnNames The columns which are to be encoded. If NULL, all character columns are detected and encoded.
#' @return A dataframe
#' @examples
#' iris2 <- iris
#' iris2$Species <- as.character(iris2$Species)
#' iris_enc <- encodeCharColumns(iris2)
#' iris_enc
#' unique(iris_enc$Species)
#' iris_dec <- decodeCharColumns(iris_enc, "Species")
#' iris_dec
#' all.equal(iris_dec, iris2)
#' @export

encodeCharColumns <- function(df, columnNames = NULL){

  assertthat::assert_that(inherits(df, "data.frame"))
  if(!is.null(columnNames)){
    assertthat::assert_that(is.character(columnNames))
    assertthat::assert_that(all(columnNames %in% colnames(df)))
  }

  encode64 <- function(string){
    string %>%
      charToRaw() %>%
      base64enc::base64encode()
  }
  encode64 <- Vectorize(encode64)

  if(is.null(columnNames)){
    cc          <- vapply(df, class, character(1)) == "character"
    if(sum(cc) == 0){
      stop("No character columns were found.")
    }
    columnNames <- colnames(df)[cc]
  }

  if(data.table::is.data.table(df)){
    DF <- data.table::copy(df)
  } else {
    DF <- df
    data.table::setDT(DF)
  }
  .SD <- NULL
  DF[, (columnNames) := lapply(.SD, encode64), .SDcols = columnNames]
  data.table::setDF(DF)

  message("Encoded these columns: ", columnNames)
  attr(DF, "encodedColumnNames") <- columnNames

  return(DF)
}

#' @name decodeCharColumns
#' @title Decode character columns.
#' @description Decode a subset of character columns which were encoded using
#'   base64 encoding.
#' @details Texual data in dataframes with comman seperators like ',' create
#'   problems writing to disk and reading back. The function 'encodeCharCols'
#'   encodes the a subset of character columns using base64 encoding where the
#'   resulting dataframe might be written to disk with ',' separator. The
#'   function 'decodeCharCols' decodes the encoded character columns are
#'   restores back the actual character columns.
#' @param df A dataframe
#' @param columnNames The columns which are to be decoded. If NULL,
#'   'encodedColumnNames' attribute is checked. It is advised not to leave it
#'   NULL.
#' @return A dataframe
#' @examples
#' iris2 <- iris
#' iris2$Species <- as.character(iris2$Species)
#' iris_enc <- encodeCharColumns(iris2)
#' iris_enc
#' unique(iris_enc$Species)
#' iris_dec <- decodeCharColumns(iris_enc, "Species")
#' iris_dec
#' all.equal(iris_dec, iris2)
#' @export
decodeCharColumns <- function(df, columnNames){

  assertthat::assert_that(inherits(df, "data.frame"))
  if(is.null(columnNames)){
    columnNames <- attr(df, "encodedColumnNames", exact = TRUE)
    if(is.null(columnNames)){
      stop("Missing The function 'decodeCharCols' decodes the encoded character columns are restores back the actual character columns. attribute. Please provide the 'columnNames' of columns which were encoded using base64.")
    } else {
      message("Decoding these columns: ", columnNames)
    }
  } else {
    assertthat::assert_that(all(columnNames %in% colnames(df)))
  }

  decode64 <- function(string){
    string %>%
      base64enc::base64decode() %>%
      rawToChar()
  }
  decode64 <- Vectorize(decode64)

  if(data.table::is.data.table(df)){
    DF <- data.table::copy(df)
  } else {
    DF <- df
    data.table::setDT(DF)
  }

  .SD <- NULL
  DF[, (columnNames) := lapply(.SD, decode64), .SDcols = columnNames]
  data.table::setDF(DF)

  return(DF)
}