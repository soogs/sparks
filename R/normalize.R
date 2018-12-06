#' normalize
#'
#' normalize each column of a matrix into unit vector. if the column is filled with zeros, it is left unchanged, since calculating the l2 norm of that column and dividing the column by that norm would lead to NA (diviing by zero)
#'
#' @param hi ya
#'
#' @return yo
#'
#' @examples
#' hi
#'
#' @export

normalize <- function(MATRIX){
  norm_inluding_zeros <- function(x){
    if(sum(x==0) == length(x)){
      result <- 1
    } else {
      result <- norm(as.matrix(x),"F")
    }
    return(result)
  }
  apply(as.matrix(MATRIX),2,function(x){x/norm_inluding_zeros(x)})
}
