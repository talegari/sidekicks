#' @name outlier_hdbscan
#' @title Detect outliers from hdbscan for large data
#' @description Obtain aggreagted GLOSH outlier scores based on hdbscan
#' @param mat (numeric matrix) data matrix
#' @param k (pos int) Minimum size of clusters for hdbscan
#' @param sampleSize (pos int) Size of the sample
#' @param nEpochs (pos int) Number of samples
#' @param distMethod (string) Method of compute distance matrix. Default is
#'   'euclidean'
#' @param seed (pos int) seed
#' @param nproc (pos int) Number of parallel processses to use via forking
#' @param distFunc 'fun' argument for 'parallelDist::parDist' when distMethod is
#'   "custom"
#' @return A vector of outlier scores
#' @examples
#' set.seed(1)
#' mix3Gaus <- rbind(
#'   mvtnorm::rmvnorm(1e3, mean = c(10, 20))
#'   , mvtnorm::rmvnorm(
#'     2e3
#'     , mean = c(20, 30)
#'     , sigma = matrix(c(1, 0.2, 0.2, 1), ncol = 2))
#'   , mvtnorm::rmvnorm(100, mean = c(15, 25), sigma = diag(6, 2))
#'  )
#' mix3Gaus <- mix3Gaus[sample(nrow(mix3Gaus)), ]
#'
#' outScore <- outlier_hdbscan(mat = mix3Gaus
#'                             , k = 100
#'                             , sampleSize = 1e3
#'                             , nEpochs    = 1e2
#'                             )
#'
#' plot(density(outScore))
#' plot(mix3Gaus)
#' plot(mix3Gaus, col = ifelse(outScore > 0.8, 1, 2))
#' @export
outlier_hdbscan <- function(mat
                             , k
                             , sampleSize
                             , nEpochs
                             , distMethod  = "euclidean"
                             , seed        = 1
                             , nproc       = 1
                             , distFunc
                            ){

  # Idea: Obtaining a GLOSH outlier score based on a hierarchical structure was
  # established by Campello et al. This might not be feasible for large datasets
  # as working with large distances matrices is prohibitive. We aggregate
  # outlier glosh scores over multiple samples of the dataset and provide an
  # outlier score for each observation.

  # Reference: Campello, Ricardo JGB, Davoud Moulavi, Arthur Zimek, and Joerg
  # Sander. "Hierarchical density estimates for data clustering, visualization,
  # and outlier detection." ACM Transactions on Knowledge Discovery from Data
  # (TKDD) 10, no. 1 (2015): 5.

  # assertions
  assertthat::assert_that(is.matrix(mat))
  assertthat::assert_that(typeof(mat) %in% c("integer", "double"))
  assertthat::assert_that(assertthat::is.count(k))
  assertthat::assert_that(assertthat::is.count(sampleSize))
  assertthat::assert_that(assertthat::is.count(nEpochs))
  assertthat::assert_that(assertthat::is.string(distMethod))
  assertthat::assert_that(assertthat::is.count(seed))
  assertthat::assert_that(assertthat::is.count(nproc))
  if(!missing(distFunc)){
    assertthat::assert_that(distMethod == "custom")
    assertthat::assert_that(is.function(distFunc))
  }

  nr <- nrow(mat)

  # Obtain glosh score for a sample of data
  get_glosh_score <- function(seed){

    set.seed(seed)
    sampleIndex <- sample(nr, sampleSize)

    # get distances for the sample
    if(distMethod != "custom"){

      dO <- parallelDist::parDist(mat[sampleIndex, ], method = distMethod)

    } else {

      dO <- parallelDist::parDist(x        = mat[sampleIndex, ]
                                  , method = distMethod
                                  , func   = distFunc
                                  )
    }

    # get outlier scores for the sample
    clustOut <- dbscan::hdbscan(x = dO, minPts = k)
      res      <- data.table::data.table(
        point   = sampleIndex
        , score = clustOut[["outlier_scores"]]
        )

    return(res)
  }

  # generate seeds
  set.seed(seed)
  seeds <- sample(10 * nEpochs, nEpochs)

  # loop over multiple samples to obtain outlier scores and aggregate them
  res <- pbmcapply::pbmclapply(seeds, get_glosh_score, mc.cores = nproc)
  res <- data.table::rbindlist(res)
  point <- NULL
  score <- NULL
  res <- res[, list(score = mean(score)), point][order(point)][["score"]]

  return(res)

}
