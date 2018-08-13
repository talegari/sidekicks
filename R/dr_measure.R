#' @name dr_measure
#' @title A measure of kNN of original and dimension reduced data
#' @description The idea is to measure the effectiveness of dimension reduction
#'   methods by computing a measure using the nearest neighbors of a point in
#'   the original space and the reduced space. Currently, "jaccard" is
#'   implemented.
#' @details The metric used to compute distances on dimension reduced data is
#'   always euclidean.
#' @param ldData (numeric matrix) Dimension reduced data
#' @param d ('dist' object) Distances between points in the original space
#' @param measure Currently, "jaccard" is implemented
#' @param ... Additional arguments to be passed to \code{\link[dbscan]{kNN}}
#' @examples
#' d_full  <- stats::dist(iris[,1:4])
#' newData <- stats::cmdscale(d_full)
#' newData_tsne <- Rtsne::Rtsne(d_full, is_distance = TRUE)[["Y"]]
#'
#' vec <- dr_measure(newData, d_full, k = 10)
#' summary(vec)
#' plot(stats::density(vec))
#'
#' vec_tsne <- dr_measure(newData_tsne, d_full, k = 10)
#' summary(vec_tsne)
#' plot(stats::density(vec_tsne))
#' @export
dr_measure <- function(ldData, d, measure = "jaccard", ...){

  assertthat::assert_that(is.matrix(ldData) && is.numeric(ldData))
  assertthat::assert_that(inherits(d, "dist"))
  assertthat::assert_that(assertthat::is.string(measure) && measure %in% c("jaccard"))
  arguments <- list(...)

  nd        <- parallelDist::parallelDist(ldData)
  knnOut_nd <- do.call(dbscan::kNN, c(list(x = nd), arguments))[["id"]]
  knnOut_d  <- do.call(dbscan::kNN, c(list(x = d), arguments))[["id"]]

  similObject <-
    proxy::dist(
      x          = knnOut_d
      , y        = knnOut_nd
      , method   = function(x, y) length(base::intersect(x, y))/length(base::union(x, y))
      , pairwise = TRUE
      )

  return( similObject )
}
