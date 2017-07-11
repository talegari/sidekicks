#' @title jaccard_m
#' @description multiset jaccard for two sets A and B
#' @details Let n(a,A) be the number of occurences of element a in multiset A.
#'   definition: numerator/denominator where numerator   = sum( min( n(a,A) ,
#'   n(a,B) ) for every a in union(A,B)) and denominator = sum( max( n(a,A) ,
#'   n(a,B) ) for every a in union(A,B))
#' @param vec1 A vector
#' @param vec2 A vector
#' @return A number
#' @examples
#' set.seed(1); A = sample(letters, 100, TRUE)
#' set.seed(2); B = sample(letters, 100, TRUE)
#' jaccard_m(A, B)
#' @export

jaccard_m = function(vec1, vec2){

  a_counts   = counts(vec1)
  b_counts   = counts(vec2)

  num = 0
  den = 0
  for(ele in intersect(names(a_counts), names(b_counts))){
    num = num + min(a_counts[ele], b_counts[ele])
    den = den + max(a_counts[ele], b_counts[ele])
  }

  return( sum(num)/sum(den) )
}
