#' @title Sample n things out if 1:maxValue with replacement
#' @description Sample from 1:maxValue vector without creating it, using runif
#' @param n Number of samples expected
#' @param maxValue Length of the vector to sample from
#' @param seed Value of seed
#' @return An numeric vector, with replacement sample of 1:maxValue
#' @examples
#' as.character(try(sample(1:1e20, 100), silent = TRUE))
#' sample_large(100, 1e20)
#' @export

sample_large = function(n, maxValue, seed){

  assert_that(is.number(maxValue) && maxValue > 0)
  assert_that(is.number(n) && n > 0)

  if(missing(seed)){
    seed = sample(1:1000, 1)
    message("Set seed: ", seed)
  } else {
    assert_that(is.number(seed))
  }

  set.seed(seed)
  round( stats::runif(n, min = 1L, max = maxValue + 1) )
}
