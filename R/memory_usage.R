#' @title current memory usage
#' @description measure R session's current memory usage after garbage
#'   collection
#' @details Do garbage collection and check current memory usage
#' @param unit A string, either "MB"(Default) or "GB"
#' @return A number indicating memory usage in MB (or GB)
#' @export

mem_cur = function(unit = "MB"){

  stopifnot(unit %in% c("MB", "GB"))

  gcO <- gc(verbose = FALSE)
  gcinfo(verbose = FALSE)
  ifelse(unit == "MB"
         , sum(gcO[,2])
         , sum(gcO[,2])/2^10
         )
}

#' @title maximum memory usage
#' @description measure R session's maximum memory usage after the last gc(reset
#'   = TRUE) call
#' @details Do garbage collection and check maximum memory usage
#' @param unit A string, either "MB"(Default) or "GB"
#' @return A number indicating memory usage in MB (or GB)
#' @export

mem_max = function(unit = "MB"){

  stopifnot(unit %in% c("MB", "GB"))

  gcO <- gc(verbose = FALSE)
  gcinfo(verbose = FALSE)
  ifelse(unit == "MB"
         , sum(gcO[,6])
         , sum(gcO[,6])/2^10
  )
}
