#' @title dir_apply: lapply kind of behaviour on files of a directory
#' @description lapply kind of behaviour on files of a directory
#' @param directory (string) directory containing input files
#' @param fun (function object or string) function whose input is the filename
#'   as a string and additional arguments if any. Defaults to "read.csv"
#'   Function name or the function name as the string may be given
#' @param extensions (character vector) extension names. Defaults to "csv"
#' @param ... additional arguments to fun, passed asis and unevaluated
#' @return A list with names same as the filenames in the directory

dir_apply = function(directory
                      , fun         = "read.csv"
                      , extensions  = c("csv")
                      , ...){

  # assertions
  stopifnot(file_test("-d", directory))
  directory = normalizePath(directory)
  stopifnot(class(extensions) == "character")
  stopifnot(is.function(match.fun(fun)))
  filenames = tools::list_files_with_exts(directory, extensions)
  if(length(filenames) == 0){
    message("dir_apply: No files with given extension was found")
    return(list())
  }

  # lapply call
  oList = lapply(filenames
                  , function(x){try(match.fun(fun)(x,...), silent = TRUE)})
  names(oList) = tools::file_path_sans_ext(basename(filenames))
  nTryErrors   = sum(vapply(oList
                            , function(x) class(x) == "try-error"
                            , logical(1))
                     )
  if(nTryErrors != 0){
    message("dir_apply: number of try errors is ", nTryErrors)
  }
  return(oList)
}
