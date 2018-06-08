#' @name dist_pca
#' @title Distance matrix with inter feature(variable) distances based on PCA
#' @description Computes distance between features(variables) weighing the
#'   loadings proportional to corresponding eigenvalues
#' @details See the book: 'An introduction to applied multivariate analysis with
#'   R' by Everitt (et al) section 3.6 on 'Rescaling the principal components'
#' @param model Object of class 'prcomp' or 'princomp'.
#' @param ... Arguments to be passed to 'proxy::dist'
#' @examples
#' pca_iris          <- stats::prcomp(scale(iris[,1:4]))
#' feature_dist_iris <- dist_pca(pca_iris)
#' feature_dist_iris
#' plot(stats::hclust(feature_dist_iris))
#'
#' pca_iris2         <- stats::princomp(covmat = cor(iris[,1:4]), cor = TRUE)
#' feature_dist_iris2 <- dist_pca(pca_iris2)
#' feature_dist_iris2
#' all.equal(feature_dist_iris, feature_dist_iris2)
#' plot(stats::hclust(feature_dist_iris))
#' @export

dist_pca <- function(model, ...){

  stopifnot(inherits(model, c("prcomp", "princomp")))

  # dissim(feature_i, pca_j) = eigen_i * (1 - cov(pca_j, feature_i))
  if(inherits(model, c("prcomp"))){
    dissim <- t(model$sdev^2 * (1 - t(model$rotation)))
  } else {
    dissim <- t(model$sdev^2 * (1 - t(model$loadings)))
  }

  feature_dist <- proxy::dist(dissim, ...)

  return(feature_dist)
}
