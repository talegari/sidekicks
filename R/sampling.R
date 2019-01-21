# dplyr like sample methods for matrix

#' @name sample_frac_matrix
#' @title dplyr like sample_frac for a matrix
#' @description  dplyr like sample_frac for a matrix
#' @param mat A matrix
#' @param frac fraction of rows to sample
#' @param replace See arguments for 'base::sample'
#' @param weight See arguments for 'base::sample'
#' @return A matrix
#' @examples
#' library("magrittr")
#' mat <- matrix(1:1e4, ncol = 10)
#' mat %>% sample_frac_matrix(0.1) %>% dim()
#' @export
#'
sample_frac_matrix <- function(mat
                               , frac
                               , replace = FALSE
                               , weight  = NULL
                               ){
  nr <- nrow(mat)
  mat[sample(nr, frac * nr, replace = replace, prob = weight), ]
}

#' @name sample_n_matrix
#' @title dplyr like sample_n for a matrix
#' @description  dplyr like sample_n for a matrix
#' @param mat A matrix
#' @param n Number of rows to sample
#' @param replace See arguments for 'base::sample'
#' @param weight See arguments for 'base::sample'
#' @return A matrix
#' library("magrittr")
#' mat <- matrix(1:1e4, ncol = 10)
#' mat %>% sample_n_matrix(100) %>% dim()
#' @export
sample_n_matrix <- function(mat
                            , n
                            , replace = FALSE
                            , weight  = NULL
                            ){
  nr <- nrow(mat)
  mat[sample(nr, n, replace = replace, prob = weight), ]
}