#' @name upsample_minority
#' @title Upsample minority class to the size of majority class
#' @description Upsamples the minority class in binary classification setting to
#'   the size of majority class by randomly selecting minority class with
#'   repition to match the number of majority class
#' @param x A dataframe
#' @param classVarName Name of the binary class column
#' @param arrangeVarName Name of the variable to arrange by
#' @param seed Seed chosen for sampling, defaults to 100
#' @param shuffle Whether to shuffle the dataframe at the end. This cannot be
#'   TRUE when arrangeVarName is specified. Shuffle used the seed value seed +
#'   1.
#' @examples
#' iris2         <- iris[c(1:25, 51:100), ]
#' iris2$Species <- factor(iris2$Species)
#' sampled <- upsample_minority(iris2, "Species")
#' table(sampled$Species)
#' @export
upsample_minority <- function(x
                              , classVarName
                              , arrangeVarName
                              , seed = 100
                              , shuffle = FALSE
                              ){

  # assertions ----
  assert_that(inherits(x, "data.frame"))

  colnames_x <- colnames(x)

  assert_that(is.string(classVarName))
  assert_that(classVarName %in% colnames_x)
  if(class(x[[classVarName]]) != "factor"){
    stop("Class variable has to be a factor with two classes")
  }
  if(levels(x[[classVarName]]) %>% length != 2){
    stop("Class variable has to be a factor with two classes")
  }
  assert_that(is.count(seed))
  if(!missing(arrangeVarName)){
    assert_that(is.string(arrangeVarName))
    assert_that(arrangeVarName %in% colnames_x)
  } else {
    arrangeVarName <- NULL
  }
  assert_that(is.flag(shuffle))
  if(!is.null(arrangeVarName) && shuffle){
    stop("'shuffle' argument cannot be TRUE when 'arrangeVarName' is specified")
  }

  # upsample minority class ----
  tabulated           <- table(x[[classVarName]])
  minorityClassName   <- names(which.min(tabulated))
  minorityIndex       <- which(x[[classVarName]] == minorityClassName)
  set.seed(seed)
  minoritySampleIndex <- sample(minorityIndex
                                , max(tabulated)
                                , replace = TRUE
                                )
  binded <- dplyr::bind_rows(x[-minorityIndex, ], x[minoritySampleIndex, ])

  if(!is.null(arrangeVarName)){
    binded <- dplyr::arrange_(binded, arrangeVarName)
  }

  if(shuffle){
    set.seed(seed + 1)
    binded <- binded[sample.int(nrow(binded)), ]
  }

  return( binded )

}

#' @name downsample_majority
#' @title Downsample majority class to the size of minority class
#' @description Downsamples the majority class in binary classification setting
#'   to the size of minority class by randomly selecting majority class to match
#'   the number of minority class
#' @param x A dataframe
#' @param classVarName Name of the binary class column
#' @param arrangeVarName Name of the variable to arrange by
#' @param seed Seed chosen for sampling, defaults to 100
#' @param shuffle Whether to shuffle the dataframe at the end. This cannot be
#'   TRUE when arrangeVarName is specified. Shuffle used the seed value seed + 1.
#' @examples
#' iris2         <- iris[c(1:25, 51:100), ]
#' iris2$Species <- factor(iris2$Species)
#' sampled <- downsample_majority(iris2, "Species")
#' table(sampled$Species)
#' @export
downsample_majority <- function(x
                                , classVarName
                                , arrangeVarName
                                , seed = 100
                                , shuffle = FALSE
                                ){

  # assertions ----
  assert_that(inherits(x, "data.frame"))

  colnames_x <- colnames(x)

  assert_that(is.string(classVarName))
  assert_that(classVarName %in% colnames_x)
  if(class(x[[classVarName]]) != "factor"){
    stop("Class variable has to be a factor with two classes")
  }
  if(levels(x[[classVarName]]) %>% length != 2){
    stop("Class variable has to be a factor with two classes")
  }
  assert_that(is.count(seed))
  if(!missing(arrangeVarName)){
    assert_that(is.string(arrangeVarName))
    assert_that(arrangeVarName %in% colnames_x)
  } else {
    arrangeVarName <- NULL
  }
  assert_that(is.flag(shuffle))
  if(!is.null(arrangeVarName) && shuffle){
    stop("'shuffle' argument cannot be TRUE when 'arrangeVarName' is specified")
  }

  tabulated           <- table(x[[classVarName]])
  majorityClassName   <- names(which.max(tabulated))
  majorityIndex       <- which(x[[classVarName]] == majorityClassName)
  set.seed(seed)
  majoritySampleIndex <- sample(majorityIndex
                                , min(tabulated)
                                )
  binded <- dplyr::bind_rows(x[-majorityIndex, ], x[majoritySampleIndex, ])

  if(!is.null(arrangeVarName)){
    binded <- dplyr::arrange_(binded, arrangeVarName)
  }

  if(shuffle){
    set.seed(seed + 1)
    binded <- binded[sample.int(nrow(binded)), ]
  }

  return( binded )
}
