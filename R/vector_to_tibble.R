#' @title vector_to_tibble
#' @description Utility to convert named vector to two column tibble with first column being names of the vector and second being the vector
#' @param vec A vector
#' @param colname Name of the column to be formed
#' @return A tibble
#' @details The column containing names will be named 'names_colname'
#' @examples
#' vector_to_tibble(c(a = 1, b = 2), colname = "val")
#' @export

vector_to_tibble = function(vec, colname = "col"){

  stopifnot(is_vector(vec))
  stopifnot(length(colname) == 1 && is.character(colname))

  if(is.null(names(vec))){
    tbl           = tibble::tibble(col = vec)
    colnames(tbl) = colname
  } else {
    tbl           = tibble::tibble(col_names = names(vec), col = vec)
    colnames(tbl) = c(paste0(colname, "_names"), colname)
  }

  tbl

}