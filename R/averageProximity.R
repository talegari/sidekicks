#' @title Compute average proximity matrix on a dataset using randomForest
#' @description Each predictor(column) is estimated using other predictors. Proximity matrices of these models are averaged
#' @param dataset A \eqn{n \times m} dataframe with valid column names
#' @return A matrix with dimension \eqn{n \times n}
#' @examples
#' ap = averageProximity(iris)
#' dim(ap)
#' plot(hclust(ap))
#' @export

averageProximity = function(dataset){

  assert_that(inherits(dataset, "data.frame"))
  assert_that(!is.null(colnames(dataset)))

  pMatAvg = matrix(0, nrow = nrow(dataset), ncol = nrow(dataset))

  for(aColname in colnames(dataset)){
    X       = dataset[ , setdiff(colnames(dataset), aColname)]
    Y       = dataset[[aColname]]
    model   = randomForest::randomForest(x = X, y = Y, proximity = TRUE)
    pMat    = model[["proximity"]]
    pMatAvg = pMatAvg + pMat
  }

  return(as.dist(pMatAvg/ncol(dataset)))
}
