#' @name remove_duplicate_rows
#' @title Remove duplicate rows considering all or a few columns
#' @description Remove duplicate rows considering all or a few columns
#' @param x A dataframe
#' @param columnNames Names of the columns to use for computing duplicates. If not specified, all columns are used.
#' @param ... Arguments to be passed to `duplicated` function
#' @examples
#' temp <- data.frame(x = c(1, 2, 3, 1), y = c(1, 3, 3, 1), z = c(1, 2, 3, 2))
#' remove_duplicate_rows(temp)
#' remove_duplicate_rows(temp, c("x", "y"))
#' @export

remove_duplicate_rows <- function(x, columnNames, ...){

  assert_that(inherits(x, "data.frame"))
  if(!missing(columnNames)){
    assert_that(is.character(columnNames))
  } else {
    columnNames <- NULL
  }
  assert_that(all(columnNames %in% colnames(x)))

  if(!is.null(columnNames)){
    duplicates <- duplicated(x[, columnNames], ...)
  } else {
    duplicates <- duplicated(x, ...)
  }

  return( x[!duplicates, ] )
}