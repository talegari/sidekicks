#' @name kde2d_tidy
#' @title Convert the output of MASS::kde2d output into long format.
#' @description Long format is helpful in ggplot2 plots.
#' @param kde2d_output Ouput of MASS:kde2d output.
#' @examples
#' library("MASS")
#' temp <- MASS::kde2d(x = runif(100, 0, 1)
#'                     , y = runif(100, 0, 5)
#'                     , n = 500
#'                     )
#' str(temp)
#' kde2d_tidy(temp)
#' @export

kde2d_tidy <- function(kde2d_output){

  temp_reshaped <- reshape2::melt(kde2d_output[["z"]], value.name = "z")
  data.table::setDT(temp_reshaped)

  x <- y <- Var1 <- Var2 <- NULL # avoid NOTEs


  temp_reshaped[ , x := kde2d_output[["x"]][Var1] ]
  temp_reshaped[ , y := kde2d_output[["y"]][Var2] ]

  temp_reshaped <- temp_reshaped[ , c("x", "y", "z"), with = FALSE]
  return(temp_reshaped)
}
