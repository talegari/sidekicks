#' @title Append timestamp to a filepath
#' @description format:
#'   filename__year-month-day_hour-minutes-seconds_timezone.extension
#' @param path (string) file path (existence is not checked)
#' @param ext (string) Optional extension for the file
#' @return (string) A file path with time stamp appended in the format described
#'   in the description
#' @examples
#' append_time("hello/a.b")
#' append_time("hello/a.b", "R")
#' @export

append_time <- function(path, ext){

  assert_that(is.string(path))
  if(!missing(ext)){
    assert_that(is.string(ext))
  } else {
    ext = tools::file_ext(path)
  }
  withoutExt = tools::file_path_sans_ext(path)
  withTime   = paste(withoutExt
                     , gsub(":", "-", gsub(" ", "_", Sys.time())), sep = "__"
                     )
  withTime   = paste(withTime, gsub("/", "-", Sys.timezone()), sep = "_")
  if (nchar(ext) > 0) {
    withTime = paste(withTime, ext, sep = ".")
  }
  return(withTime)
}