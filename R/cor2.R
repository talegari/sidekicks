#' @title cor2: Compute correlations of columns of a dataframe of mixed types
#' @description Compute correlations of columns of a dataframe of mixed types.
#'   The dataframe is allowed to have columns of these four classes: integer,
#'   numeric, factor and character. The character column is considered as
#'   categorical variable.
#' @details The correlation is computed as follows:
#'
#'   \itemize{
#'
#'   \item integer/numeric pair: pearson correlation using `cor` function. The
#'   valuelies between -1 and 1.
#'
#'   \item integer/numeric - factor/categorical pair: Anova is performed  and
#'   effect size is computed . The value lies between 0 and 1. \item
#'   factor/categorical pair: cramersV value is computed based on chisq test
#'   using `lsr::cramersV` function. The value lies between 0 and 1.
#'
#'   }
#' Pairwise complete observations are used to compute correlation.
#' For a comprehensive implementation, use `polycor::hetcor`
#' @param df input data frame
#' @param nproc Number of parallel processes to use
#' @return  A simil/dist object.
#' @examples
#' iris_cor <- cor2(iris, nproc = 1)
#' iris_cor <- cor2(iris)
#' @export

cor2 = function(df
                , nproc = parallel::detectCores()
                ){

  stopifnot(inherits(df, "data.frame"))
  stopifnot(sapply(df, class) %in% c("integer"
                                     , "numeric"
                                     , "factor"
                                     , "character")
            )
  assertthat::assert_that(assertthat::is.count(nproc))
  nproc <- min(nproc, parallel::detectCores())

  cor_fun <- function(pos_1, pos_2){

    van <- df[[pos_1]]
    tu  <- df[[pos_2]]

    van_cc <- complete.cases(van)
    tu_cc  <- complete.cases(tu)

    van <- van[van_cc & tu_cc]
    tu  <- tu[van_cc & tu_cc]

    # both are numeric
    if(class(van) %in% c("integer", "numeric") &&
       class(tu) %in% c("integer", "numeric")){
      r <- stats::cor(van
                      , tu
                      , use = "pairwise.complete.obs"
                      )
    }

    # one is numeric and other is a factor/character
    if(class(van) %in% c("integer", "numeric") &&
       class(tu) %in% c("factor", "character")){
      r <- stats::aov( van ~ as.factor(tu) ) %>%
        lsr::etaSquared() %>%
        `[`(1) %>%
        sqrt()
    }

    if(class(tu) %in% c("integer", "numeric") &&
       class(van) %in% c("factor", "character")){
      r <- stats::aov( tu ~ as.factor(van) ) %>%
        lsr::etaSquared() %>%
        `[`(1) %>%
        sqrt()
    }

    # both are factor/character
    if(class(van) %in% c("factor", "character") &&
       class(tu) %in% c("factor", "character")){
      r <- lsr::cramersV(van, tu, simulate.p.value = TRUE)
    }

    return(r)
  }

  # now compute corr matrix
  Var1 <- NULL
  Var2 <- NULL

  grid <- expand.grid(1:ncol(df), 1:ncol(df)) %>%
    dplyr::filter(Var1 > Var2) %>%
    as.matrix()

  # parallel process using futures
  vec <- pbapply::pbsapply(1:nrow(grid)
                           , function(x) cor_fun(grid[x, 1], grid[x,2])
                           , cl = nproc
                           )
  class(vec)         <- c("dist", "simil")
  attr(vec, "Size")  <- ncol(df)
  attr(vec, "diag")  <- FALSE
  attr(vec, "upper") <- FALSE

  return(vec)
}
