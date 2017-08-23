#' @title Plot a few principal components
#' @description Plot a few principal components against each other, their densities colored by some factor
#' @param model A 'prcomp' model
#' @param rank Number of first principal components to select
#' @param color A factor to color by
#' @examples
#' pcaPlot(prcomp(iris[,1:4]), color = iris$Species)
#' @return Plots as a side effect
#' @export

pcaPlot = function(model, rank = 3, color){

  assert_that(inherits(model, "prcomp"))
  assert_that(is.count(rank) && rank > 1)
  assert_that(rank <= ncol(model[["x"]]))
  if(!missing(color)){
    assert_that(is.factor(color) && length(color) == nrow(model[["x"]]))
  }

  if(missing(color)){
    df = as.data.frame(model$x[, 1:rank])
    GGally::scatmat(df)
  } else {
    df = cbind(as.data.frame(model$x[, 1:rank]), data.frame(colorVar = color))
    GGally::scatmat(df, color = "colorVar")
  }
}
