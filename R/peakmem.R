#' @name peakmem
#' @title Compute peak memory used by a R script
#' @description The function runs the script using Rscript program in a
#'   different process and uses a python utility 'memusg'
#'   (https://github.com/jhclark/memusg/) to measure the peak memory usage by
#'   the prcocess and its child processes. This will be helpful in measuring the
#'   memory usage of code involving parallel processing. For example, code
#'   involving \code{\link{[parallel](mclapply)}} call. Place the 'memusg.py'
#'   file in '/usr/local/bin' directory and make it executable. The function is
#'   meant to measure peak memory utility in the order of GBs with long running
#'   parallel processes. For smaller processes, the overhead itself will distort
#'   the real usage. For smaller benchmarks, use \code{\link{[bench]{mark}}} or
#'   \code{\link{[peakRAM]{peakRAM}}}. For more details, its better to modify
#'   the python program.
#' @param file Location of script
#' @param timeout Timeout for the process, in seconds, or as a difftime object.
#'   If it is not finished before this, it will be killed.
#' @return Maximum memory used in GB
#' @export

peakmem <- function(file, timeout = Inf){

  file <- normalizePath(file)
  start_time <- Sys.time()
  message("start_time: ", start_time)
  res <- try(processx::run("memusg.py", args = c("Rscript", file), timeout = timeout)
             , silent = TRUE
             )
  end_time <- Sys.time()
  message("end_time: ", end_time)
  message("elapsed: "
          , round(end_time - start_time)
          , " "
          , attr(end_time - start_time, "units")
          )

  if(assertthat::is.error(res)){
    stop("R code execution resulted in an error")
  } else {
    res <- stringr::str_split(res[["stderr"]], "\\n") %>% unlist()
    pos <- which(stringr::str_detect(res, "memusg: vmpeak: "))
    if(length(pos) == 1){
      val <- res[pos] %>%
        stringr::str_split("\\s") %>%
        unlist() %>%
        `[`(3) %>%
        as.numeric() %>%
        magrittr::divide_by(2^20)
    } else {
      stop("Something went wrong with memusg. Try debug mode.")
    }
  }

  message("peakmem: ", round(val, 3), " GB")
  return(val)
}
