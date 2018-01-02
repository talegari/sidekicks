#' @name dist_pca
#' @title Distance matrix with inter feature(variable) distances based on PCA
#' @description Computes distance between features(variables) weighing the
#'   loadings proportional to corresponding eigenvalues
#' @details See the book: 'An introduction to applied multivariate analysis with
#'   R' by Everitt (et al) section 3.6 on 'Rescaling the principal components'
#' @param model Object of class 'prcomp'
#' @param ... Arguments to be passed to 'proxy::dist'
#' @examples
#' pca_iris          <- stats::prcomp(scale(iris[,1:4]))
#' feature_dist_iris <- dist_pca(pca_iris)
#' feature_dist_iris
#' plot(stats::hclust(feature_dist_iris))
#' @export

dist_pca <- function(model, ...){

  stopifnot(inherits(model, "prcomp"))

  # eigen_i * (1 - cov(pca_j, feature_i))
  dissim <- t(model$sdev^2 * (1 - t(model$rotation)))

  feature_dist <- proxy::dist(dissim, ...)

  return(feature_dist)
}
