#' scaleData
#'
#' idea/code 100 percent from Niek. Thanks sir. Standardizes the columns using the "biased sd" ####
#'
#' @param hi
#'
#' @return hi
#'
#' @examples
#' hi
#'
#' @export

scaleData <- function(X, value = 0){

  X <- scale(X, scale = FALSE)
  attr(X, "scaled:center") <- NULL
  sdX <-  apply(X, 2, function(x) sqrt( sum( x^2 ) / (length(x) - value )   ))  #compute the sd for each column

  sdX[sdX == 0] <- 1
  # to account for a column that is completely 0,
  # i make the sd into 1.

  sdX <- matrix(sdX, nrow(X), ncol(X), byrow = T)                     #put all the sd's in a matrix

  sdX

  X <- X * (1 / sdX)      #divide each entry in X by its sd
  return(X)
}
