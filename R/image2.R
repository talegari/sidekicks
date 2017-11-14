#' @title ggplot2 based alternative for R's 'image'
#' @description Visualize a matrix
#' @param x A numeric or integer matrix
#' @param ... parameters to be passed to ggplot2::geom_raster
#' @examples
#' image2( matrix(c(rnorm(1e5), runif(1e5)), 1e2) )
#' @export

image2 <- function(x, ...){

  assertthat::assert_that(inherits(x, "matrix"))
  assertthat::assert_that(typeof(x) %in% c("double", "integer"))

  plotObject <-
    ggplot2::ggplot(data = reshape2::melt(x)
                    , aes_string("Var2", "Var1", fill = "value")
                    ) +
    ggplot2::geom_raster(...) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL)

  return(plotObject)
}
