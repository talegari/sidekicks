#' @title cor2: Compute correlations of columns of a dataframe of mixed types
#' @description Compute correlations of columns of a dataframe of mixed types.
#'   The dataframe is allowed to have columns of these four classes: integer,
#'   numeric, factor and character. The character column is considered as
#'   categorical variable.
#' @details The correlation is computed as follows: \itemize{
#'   \item integer/numeric pair: pearson correlation using `cor` function. The
#'   valuelies between -1 and 1.
#'
#'   \item integer/numeric - factor/categorical pair: correlation coefficient or
#'   squared root of R^2 coefficient of linear regression of integer/numeric
#'   variable over factor/categorical variable using `lm` function. The value
#'   lies between 0 and 1. \item factor/categorical pair: cramersV value is
#'   computed based on chisq test using `lsr::cramersV` function. The value lies
#'   between 0 and 1.
#'   }
#'   For a comprehensive implementation, use `polycor::hetcor`
#' @param df input data frame
#' @examples
#' iris_cor <- cor2(iris)
#' @export

cor2 = function(df){

  stopifnot(inherits(df, "data.frame"))
  stopifnot(sapply(df, class) %in% c("integer"
                                     , "numeric"
                                     , "factor"
                                     , "character"))

  cor_fun <- function(pos_1, pos_2){

    # both are numeric
    if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
       class(df[[pos_2]]) %in% c("integer", "numeric")){
      r <- stats::cor(df[[pos_1]]
                      , df[[pos_2]]
                      , use = "pairwise.complete.obs"
      )
    }

    # one is numeric and other is a factor/character
    if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
       class(df[[pos_2]]) %in% c("factor", "character")){
      r <- sqrt(
        summary(
          stats::lm(df[[pos_1]] ~ as.factor(df[[pos_2]])))[["r.squared"]])
    }

    if(class(df[[pos_2]]) %in% c("integer", "numeric") &&
       class(df[[pos_1]]) %in% c("factor", "character")){
      r <- sqrt(
        summary(
          stats::lm(df[[pos_2]] ~ as.factor(df[[pos_1]])))[["r.squared"]])
    }

    # both are factor/character
    if(class(df[[pos_1]]) %in% c("factor", "character") &&
       class(df[[pos_2]]) %in% c("factor", "character")){
      r <- lsr::cramersV(df[[pos_1]], df[[pos_2]], simulate.p.value = TRUE)
    }

    return(r)
  }

  cor_fun <- Vectorize(cor_fun)

  # now compute corr matrix
  corrmat <- outer(1:ncol(df)
                   , 1:ncol(df)
                   , function(x, y) cor_fun(x, y)
  )

  rownames(corrmat) <- colnames(df)
  colnames(corrmat) <- colnames(df)

  return(corrmat)
}
