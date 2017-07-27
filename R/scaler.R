#' @title Create scaler model
#' @description Creates a scaler object containing column means and standard
#'   deviations so that it can be used to predict on a similar dataset
#' @param data (numeric matrix or numeric dataframe) The dataset
#' @param center (flag) whether to center the columns or not
#' @param scale (flag) whether to scale the columns or not
#' @details This computes means and standard deviations of each columns and
#'   stores it for a prediction on a dataset using predict method. If scale is
#'   TRUE, the columns are automatically centered even if center is set to
#'   FALSE.
#'
#'   The scaler class provides a model-predict interface to scale  and unscale
#'   matrices and dataframes. This predict method supports type argument - scale
#'   or unscale. The scaler_ function is used to construct scaler object by
#'   providing centering vector(alias for means of columns, ex: columnwise
#'   medians) and scaling vector (alias for column standard deviations, ex:
#'   columnwise mean absolute deviations). scaler class is meant to aid
#'   analysis, for performance critical work use Rfast::standardize()
#' @examples
#' set.seed(1)
#' n_70     = round(nrow(mtcars) * 0.7)
#' index    = sample(1:nrow(mtcars), n_70)
#' mtcars_A = mtcars[index, ]
#' mtcars_B = mtcars[index, ]
#' model    = scaler(mtcars_A)         # creates model based on mtcars_A
#' mtcars_1 = predict(model, newdata = mtcars_A) # scale mtcars_A
#' mtcars_2 = predict(model, newdata = mtcars_B) # scale mtcars_B using model
#' class(mtcars_2)                     # does not convert to matrix
#' mtcars_2_B = predict(model, newdata = mtcars_2, type = "unscale")
#' all.equal(mtcars_2_B, mtcars_B)
#' @export
scaler            = function(data, center = TRUE, scale = TRUE){
  UseMethod("scaler")
}

#' @export
scaler.matrix     = function(data, center = TRUE, scale = TRUE){

  stopifnot(inherits(data, "matrix") &&
              (typeof(data) %in% c("integer", "double"))
  )
  assertthat::assert_that(assertthat::is.flag(center))
  assertthat::assert_that(assertthat::is.flag(scale))
  if(scale == FALSE && center == FALSE){
    stop("One among 'center' and 'scale' should be TRUE")
  }
  if(scale == TRUE && center == FALSE){ center = TRUE }

  ncols = ncol(data)
  if(ncols == 0) stop("matrix should have atleast one column")

  col_means = NULL
  if(center){
    col_means = colMeans(data, na.rm = TRUE)
  }

  col_sds = NULL
  if(scale){
    col_sds   = apply(data, 2, function(x) sd(x, na.rm = TRUE))
  }
  if(any(col_sds == 0)){
    warning("Some standard deviations are zero")
  }

  scaler_list = list(ncols       = ncols
                     , col_means = col_means
                     , col_sds   = col_sds
  )

  class(scaler_list) = "scaler"

  return(scaler_list)
}

#' @export
scaler.data.frame = function(data, center = TRUE, scale = TRUE){

  stopifnot(all(sapply(data, class) %in% c("numeric", "integer")))
  assertthat::assert_that(assertthat::is.flag(center))
  assertthat::assert_that(assertthat::is.flag(scale))
  if(scale == FALSE && center == FALSE){
    stop("One among 'center' and 'scale' should be TRUE")
  }
  if(scale == TRUE && center == FALSE){ center = TRUE }

  ncols = ncol(data)
  if(ncols == 0) stop("dataframe should have atleast one column")

  col_means = NULL
  if(center){
    col_means = sapply(data, function(x) mean(x, na.rm = TRUE))
  }

  col_sds = NULL
  if(scale){
    col_sds   = sapply(data, function(x) sd(x, na.rm = TRUE))
  }
  if(any(col_sds == 0)){
    warning("Some standard deviations are zero")
  }

  scaler_list = list(ncols       = ncols
                     , col_means = col_means
                     , col_sds   = col_sds
  )

  class(scaler_list) = "scaler"

  return(scaler_list)

}

# predict = function(object, newdata, type = "scale"){
#   UseMethod("predict")
# }


#' @title Scale a dataset using scaler model
#' @description See \code{\link{scaler}}
#' @param object A scaler object
#' @param ... See details
#' @details \itemize{
#' \item newdata A numeric matrix or a dataframe to be scaled
#' \item type Either 'scale' or 'unscale'
#' }
#' @export

predict.scaler = function(object, ...){

  # assertions ----
  args = list(...)

  if(!("newdata" %in% names(args))){
    stop("newdata is missing")
  }

  if(!inherits(args$newdata, "matrix") && !inherits(args$newdata, "data.frame")){
    stop("newdata should be integer/numeric matrix or a dataframe")
  }

  if(!("type" %in% names(args))){
    type = "scale"
  } else {
    type = args$type
  }
  stopifnot(type %in% c("scale", "unscale"))


  # work ----
  if(inherits(args$newdata, "matrix")){

    stopifnot(typeof(args$newdata) %in% c("integer", "double"))
    if(ncol(args$newdata) != object$ncols){
      stop("Number of columns mismatch")
    }

    if(type == "scale"){

      if(!is.null(object$col_means)){
        scaled = sweep(args$newdata, 2 , object$col_means, `-`)
      }

      if(any(object$col_sds == 0)){
        warning(
          paste0("Some column standard deviations are zero. "
                 , "This might produce a few NAN in the resulting matrix")
        )
      }
      if(!is.null(object$col_sds)){
        scaled = sweep(scaled, 2 , object$col_sds, `/`)
      }

      return(scaled)

    } else {

      if(any(object$col_sds == 0)){
        warning(
          paste0("Some column standard deviations are zero. "
                 , "This might produce a few NAN in the resulting matrix")
        )
      }
      if(!is.null(object$col_sds)){
        unscaled = sweep(args$newdata, 2 , object$col_sds, `*`)
      } else {
        unscaled = args$newdata
      }

      if(!is.null(object$col_means)){
        unscaled = sweep(unscaled, 2 , object$col_means, `+`)
      }

      return(unscaled)
    }
  }

  if(inherits(args$newdata, "data.frame")){

    stopifnot(sapply(args$newdata, class) %in% c("integer", "numeric"))

    if(ncol(args$newdata) != object$ncols){
      stop("Number of columns mismatch")
    }

    if(type == "scale"){

      if(!is.null(object$col_means)){
        scaled = mapply(`-`, args$newdata, object$col_means, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        data.table::setDF(scaled)
      }

      if(any(object$col_sds == 0)){
        warning(paste0("Some column standard deviations are zero. "
                       , "This might produce a few NAN in the resulting matrix"))
      }
      if(!is.null(object$col_sds)){
        scaled = mapply(`/`, scaled, object$col_sds, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        data.table::setDF(scaled)
      }

      return(scaled)

    } else {

      if(any(object$col_sds == 0)){
        warning(paste0("Some column standard deviations are zero. "
                       , "This might produce a few NAN in the resulting matrix"))
      }
      if(!is.null(object$col_sds)){
        unscaled = mapply(`*`, args$newdata, object$col_sds, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        data.table::setDF(unscaled)
      } else {
        unscaled = args$newdata
      }

      if(!is.null(object$col_means)){
        unscaled = mapply(`+`, unscaled, object$col_means, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        data.table::setDF(unscaled)
      }

      return(unscaled)
    }
  }
}

#' @title Constructor for scaler class
#' @description See \code{\link{scaler}}
#' @param ncols (positive integer) Number of columns
#' @param center_vector Vector to be used for centering. Length should be equal
#'   to ncols
#' @param scale_vector Vector to be used for scaling. Length should be equal to
#'   ncols
#' @export
scaler_           = function(ncols, center_vector, scale_vector){

  assertthat::assert_that(assertthat::is.count(ncols))
  assertthat::assert_that(is.numeric(center_vector) && length(center_vector) == ncols)
  assertthat::assert_that(!anyNA(center_vector))
  assertthat::assert_that(is.numeric(scale_vector) && length(scale_vector) == ncols)
  assertthat::assert_that(!anyNA(scale_vector))

  scaler_object = list(ncols       = ncols
                       , col_means = center_vector
                       , col_sds   = scale_vector
  )
  class(scaler_object) = "scaler"

  return(scaler_object)
}

