#' @title Find the outlier rows in numeric matrix
#' @description and visualize them with TSNE
#' @param dataMatrix A numeric matrix
#' @param doScale Whether to scale the data
#' @param method Only 'knn' is supported currently
#' @param doPlot Whether to compute TSNE and visualiza outliers
#' @param coef For 'boxplot.stats'
#' @param perplexity For 'Rtsne::Rtsne'
#' @param ... specific inputs based on method (TODO)
#' @return A list with: oulierIndex and tsne plot(ggplot2) object
#' @details TODO
#' @examples
#' result <- outlierIndex(as.matrix(iris[-143,1:4]), k = 3, representative = median)
#' result[[1]]
#' result[[2]]
#' @export

outlierIndex <- function(dataMatrix
                         , doScale    = TRUE
                         , method     = "knn"
                         , doPlot     = TRUE
                         , coef       = 1.5
                         , perplexity = 30
                         , ...){

  assert_that(is.matrix(dataMatrix))
  assert_that(!anyDuplicated(dataMatrix))
  assert_that(typeof(dataMatrix) %in% c("integer", "double"))
  assert_that(is.logical(doScale))
  assert_that(method %in% c("knn"))
  assert_that(is.number(coef) && coef > 0)
  assert_that(is.count(perplexity))

  dataMatrix <- if(doScale) scale(dataMatrix)
  options    <- list(...)

  # start: knn
  if(method == "knn"){
    if(is.null(options[["k"]])){
      stop("Provide a value for k: numbers of neighbours in KNN")
    }
    assert_that(is.count(options[["k"]]))

    if(is.null(options[["representative"]])){
      stop("Provide a representative function")
    }
    assert_that(is.function(options[["representative"]]))

    set.seed(1)
    distances <- dbscan::kNNdist(x = dataMatrix, k = options[["k"]])
    repValues <- apply(distances, 1, options[["representative"]])
  }
  # end: knn

  # handle repValues
  outs     <- grDevices::boxplot.stats(repValues, coef = coef)[["out"]]
  outBool  <- repValues %in% outs
  outIndex <- which(outBool)
  if(length(outIndex) == 0){
    message("No ouliers were detected")
  }

  if(doPlot && length(outIndex) > 0){

    set.seed(1)
    tsneOut <- Rtsne::Rtsne(dataMatrix, perplexity = perplexity)$Y %>%
      tibble::as_tibble() %>%
      dplyr::bind_cols(outlier = outBool, lab = as.character(1:nrow(dataMatrix)))

    tsnePlot <-
      ggplot(tsneOut, aes_string("V1", "V2", color = "outlier")) +
      geom_point() +
      geom_text(data = dplyr::filter_(tsneOut, "outlier")
                , aes_string(label = "lab", hjust = 1, vjust = 1)
      )

    return(list(oulierIndex = outIndex, tsnePlot = tsnePlot))

  }

  return(list(oulierIndex = outIndex, tsnePlot = NULL))
}
