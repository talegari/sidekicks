#' @name condense
#' @title Sample points based on distance matrix
#' @description Sample points based on distance matrix so that distance between any pair is not less than a fixed radius. This will not preserve density, but does preserve local outliers.
#' @param d object of class 'dist'
#' @param radius (numeric) radius around a point
#' @param seed (positive integer) A seed. Default is 1.
#' @param ... Arguments for \code{\link[dbscan]{frNN}}
#' @return Index of condensed points
#' @examples
#' library("magrittr")
#' dataset <- rbind(
#'   MASS::mvrnorm(1000, mu = c(0, 0), Sigma = matrix(c(1, 0.3, 0.3, 1), 2))
#'   , MASS::mvrnorm(100, mu = c(10, 10), Sigma = matrix(c(1, 0.3, 0.3, 1), 2))
#'   , MASS::mvrnorm(10, mu = c(5, 5), Sigma = matrix(c(1, 0.3, 0.3, 1), 2))
#'   , MASS::mvrnorm(10, mu = c(0, 8), Sigma = matrix(c(1, 0.3, 0.3, 1), 2))
#'   , MASS::mvrnorm(10, mu = c(7, 0), Sigma = matrix(c(1, 0.3, 0.3, 1), 2))
#'   )
#' plot(dataset)
#' do <- dist(dataset)
#' dbscan::kNNdist(do, 4) %>% apply(1, median) %>% summary()
#'
#' ci <- condense(do, 0.103)
#' dataset[ci, ] %>% plot()
#' dataset[condense(do, 0.68), ] %>% plot()
#' @export

condense <- function(d, radius, seed = 1, ...){

  arguments <- list(...)
  size      <- attr(d, "Size")

  neighbors <- do.call(dbscan::frNN, c(list(x = d, eps = radius), arguments))[["id"]]

  condensed <- vector("integer", length = size)
  currIndex <- 1:size

  iter <- 0L

  while(length(currIndex) > 1){
    iter <- iter + 1L

    set.seed(seed)
    obs <- sample(currIndex, 1L)

    currIndex       <- setdiff(currIndex, c(obs, neighbors[[obs]]))
    condensed[iter] <- obs
  }

  return(c(condensed[1:iter], currIndex))
}
