#' @title mem_current
#' @description measure R session's current memory usage
#' @details Do garbage collection and check current memory usage
#' @param unit A string, either "MB"(Default) or "GB"
#' @return A number indicating memory usage in MB (or GB)
#' @export

mem_current = function(unit = "MB"){

  stopifnot(unit %in% c("MB", "GB"))

  gcO <- gc(verbose = FALSE)
  gcinfo(verbose = FALSE)
  ifelse(unit == "MB"
         , sum(gcO[,2])
         , sum(gcO[,2])/2^10
         )
}

#' @title memory_max
#' @description measure R session's maximum memory usage
#' @details Do garbage collection and check maximum memory usage
#' @param unit A string, either "MB"(Default) or "GB"
#' @return A number indicating memory usage in MB (or GB)
#' @export

memory_max = function(unit = "MB"){

  stopifnot(unit %in% c("MB", "GB"))

  gcO <- gc(verbose = FALSE)
  gcinfo(verbose = FALSE)
  ifelse(unit == "MB"
         , sum(gcO[,6])
         , sum(gcO[,6])/2^10
  )
}
