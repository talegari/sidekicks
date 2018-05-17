#' @name regression_corrector
#' @title Adds a quick layer of flexible univariate regression to fit any
#'   regression model to reduce bias.
#' @description Regress actual 'y' versus predicted 'y' in a regression fit
#'   using a flexible univariate regressor to reduce bias and possibly reduce
#'   other patterns in the residues.
#' @param fit A regression fit
#' @param data Data to be used to predict the 'fit'
#' @param actual Actual 'y'
#' @param predictFun A predict function to be used with 'fit' and 'data' as first two unnamed arguments
#' @param method Method for the univariate fit. These are implemented: smoothing spline using `stats::smooth.spline`, linear regression using `lm` and localized regression using `loess`.
#' @param ... Arguments to the function fitting the univariate fit
#' @details The returned object
#' @return A object of class 'regressionCorrector' with these components:
#' \itemize{
#' \item method: method argument
#' \item reg: univariate regression fit
#' \item fit: fit argument
#' \item predictFun: predictFun argument
#' \item actual: actual argument
#' \item predicted: Predicted output of predicting fit on data
#' }
#' @examples
#' dplyr::glimpse(MASS::Boston)
#'
#' set.seed(2)
#' train_sample <- sample.int(nrow(MASS::Boston), 400)
#' boston_train <- MASS::Boston[train_sample, ]
#' boston_test  <- MASS::Boston[-train_sample, ]
#'
#' set.seed(500)
#' fit_gbm  <- gbm::gbm(medv ~., data = boston_train, n.trees = 500)
#' pred_gbm <- predict(fit_gbm, boston_train, n.trees = 500)
#' plot(pred_gbm, MASS::Boston$medv[train_sample] - pred_gbm)
#'
#' rcf <- regression_corrector(
#'   fit_gbm
#'   , boston_train
#'   , boston_train[["medv"]]
#'   , function(model, data) stats::predict(model, data, n.trees = 500)
#'   )
#' rcf
#'
#' MLmetrics::RMSE(stats::predict(fit_gbm, boston_test, n.trees = 500)
#'                 , boston_test[["medv"]]
#'                 )
#' MLmetrics::RMSE(predict(rcf, boston_test, n.trees = 500)
#'                 , boston_test[["medv"]]
#'                 )
#'
#' old <- ggplot2::qplot(boston_test[["medv"]]
#'      , boston_test[["medv"]] - predict(fit_gbm, boston_test, n.trees = 500)
#'      ) +
#'   ggplot2::geom_hline(yintercept = 0, color = "green") +
#'   ggplot2::ggtitle("before correction")
#' new <- ggplot2::qplot(boston_test[["medv"]], boston_test[["medv"]] - predict(rcf, boston_test)) +
#'   ggplot2::geom_hline(yintercept = 0, color = "green") +
#'   ggplot2::ggtitle("after correction")
#' cowplot::plot_grid(old, new, align = "h")
#'
#' rcf
#' rcf2 <- regression_corrector(rcf, MASS::Boston, MASS::Boston[["medv"]])
#' rcf2
#' rcf3 <- regression_corrector(rcf2, MASS::Boston, MASS::Boston[["medv"]])
#' rcf3
#'
#' MLmetrics::RMSE(predict(rcf3, boston_test)
#'                 , boston_test[["medv"]]
#'                 )
#' @export
regression_corrector <- function(fit
                                 , data
                                 , actual
                                 , predictFun = stats::predict
                                 , method     = "smooth.spline"
                                 , ...
                                 ){

  assertthat::assert_that(is.numeric(actual) && !anyNA(actual))

  arguments <- list(...)

  predicted <- predictFun(fit, data)
  assertthat::assert_that(is.numeric(predicted))
  assertthat::assert_that(length(actual) == length(predicted))
  assertthat::assert_that(assertthat::is.string(method))
  assertthat::assert_that(method %in% c("smooth.spline", "lm", "loess"))


  if(method == "smooth.spline"){
    regCorrector       <- list(reg = do.call(stats::smooth.spline
                                             , c(list(x = predicted, y = actual)
                                                 , arguments
                                                 )
                                             )
                               )
  }

  if(method == "lm"){
    df            <- data.frame(actual = actual
                                , predicted = predicted
                                )
    regCorrector  <- list(reg = do.call(stats::lm
                                       , c(list(formula = actual ~ predicted
                                                , data = df
                                                )
                                           , arguments
                                           )
                                       )
                          )
  }

  if(method == "loess"){
    df            <- data.frame(actual = actual
                                , predicted = predicted
                                )
    regCorrector  <- list(reg =
                            do.call(stats::loess
                                   , c(list(formula = actual ~ predicted
                                            , data = df
                                            )
                                       , arguments
                                       )
                                   )
                          )
  }

  regCorrector[["method"]]     <- method
  regCorrector[["fit"]]        <- fit
  regCorrector[["predictFun"]] <- predictFun
  regCorrector[["actual"]]     <- actual
  regCorrector[["predicted"]]  <- predicted
  class(regCorrector)          <- "regressionCorrector"

  return(regCorrector)
}

#' @name predict.regressionCorrector
#' @title Predict method for regressionCorrector class
#' @description Predict method for regressionCorrector class
#' @seealso regression_corrector
#' @param object Object of class 'regressionCorrector'
#' @param newdata New data to predict on
#' @param ... Optional arguments
#' @return A numeric vector
#' @export
predict.regressionCorrector <- function(object, newdata, ...){

  arguments <- list(...)
  method    <- object[["method"]]

  x <- object[["predictFun"]](object[["fit"]], newdata)
  assertthat::assert_that(is.numeric(x) && !anyNA(x))

  result <- switch(
    method
    , smooth.spline = do.call(stats::predict
                              , c(list(object = object[["reg"]], x = x)
                                  , arguments
                                  )
                              )[["y"]]
    , lm            = do.call(stats::predict
                              , c(list(object = object[["reg"]], newdata = data.frame(predicted = x))
                                  , arguments
                                  )
                              )
    , loess         = do.call(stats::predict
                              , c(list(object = object[["reg"]], newdata = data.frame(predicted = x))
                                  , arguments
                                  )
                              )
  )

  return(result)
}

#' @name print.regressionCorrector
#' @title Print method for regressionCorrector class
#' @description Print method for regressionCorrector class
#' @seealso regression_corrector
#' @param x Object of class 'regressionCorrector'
#' @param ... Currently, unused
#' @return invisible NULL
#' @details Prints the method of univariate fit, RMSE and PRMSE between actual
#'   and predicted output of fit on data (not the predict of
#'   regressionCorrector).
#' @export
print.regressionCorrector   <- function(x, ...){
  message("Regressor Corrector model with method: ", x[["method"]])
  message("Fitted RMSE: "
          , round(MLmetrics::RMSE(x[["actual"]]
                             , x[["predicted"]]
                             ), 2)
          )
  message("Fitted RMSPE: "
          , round(MLmetrics::RMSPE(x[["actual"]]
                             , x[["predicted"]]
                             ), 2)
          )
  return(invisible(NULL))
}
