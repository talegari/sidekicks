#' @name loc
#' @title classwise local outliers
#' @description Detect local outliers when an observation of class A occurs in
#'   the neighborhood of class B using fixed radius neighborhood search. When
#'   the fraction of neighbors of the same class as the observation is less than
#'   the threshold, it is considered as an outlier.
#' @details Ideal conditions for using this is when there is no data imbalance
#'   and there are observations similar to eachother but from different classes.
#'   When minnStrategy is 'include', the global outliers (ones with less than
#'   minn neighbors) are included in the output.
#' @return Vector of indexes of outliers
#' @param d object of class 'dist'
#' @param classes (factor) Vector of classes
#' @param r Value of the fixed radius
#' @param thres Threshold for the fraction of observations of same class
#' @param minn Minimum number of neighbors in the neighborhood
#' @param minnStrategy When minnStrategy is 'include', the points with less than
#'   minn neighbors are considered outliers.
#' @export

loc <- function(d, classes, r, thres = 0.4, minn = 3, minnStrategy = "include"){
  
  assertthat::assert_that(inherits(d, "dist"))
  assertthat::assert_that(is.factor(classes) && nlevels(classes) > 1)
  assertthat::assert_that(assertthat::is.number(r) && r > 0)
  assertthat::assert_that(assertthat::is.count(minn))
  assertthat::assert_that(assertthat::is.string(minnStrategy) && 
                            minnStrategy %in% c("exclude", "include")
                          )
  
  lvs  <- levels(classes)
  ell  <- length(classes)
  
  props <- dbscan::frNN(d, eps = r, sort = FALSE, approx = TRUE)[["id"]] %>% 
    lapply(function(x){
            if(length(x) >= minn){
              factor(classes[x], levels = lvs)
            } else {
              factor(vector("character", 0L), levels = lvs)
            }
      }
    ) %>%
    lapply(table) %>%
    lapply(prop.table) %>%
    lapply(unclass)
  
  if(minnStrategy == "include"){
    
    res <- sapply(1:ell, function(x) props[[x]][[classes[x]]]) %>% 
      `<`(thres) %>% 
      tidyr::replace_na(TRUE) %>% 
      which()
  
  } else {
    
    res <- sapply(1:ell, function(x) props[[x]][[classes[x]]]) %>% 
      `<`(thres) %>% 
      tidyr::replace_na(FALSE) %>% 
      which()
  
  }
  
  return(res)
  
}
