#' VAFcontrol
#'
#' enter amount of noise in a dataset
#'
#' @param hi ya
#'
#' @return yo
#'
#' @examples
#' hi
#'
#' @export

VAFcontrol <- function(data, VAFx, seed){

  n <- nrow(data)
  p <- ncol(data)

  # sum of squares of the X2 dataset
  ssqXtrue <- sum(data^2)

  set.seed(seed)
  # sample from normal distribution (Ex = Error of X)
  Ex <- matrix(MASS::mvrnorm(n = n, mu = rep(1,p), Sigma = diag(p)),nrow = n, ncol = p)

  # centering the Ex matrix
  Ex <- t(t(Ex) - colMeans(Ex))
  Ex <- scaleData(Ex)

  # sum of squares of EX
  ssqEx <- sum(Ex^2)

  # Rescale noise to desired level
  fx <- sqrt(ssqXtrue*(1-VAFx)/(VAFx * ssqEx))
  # fx=sqrt(ssqXtrue*(1-VAFx(vx))/(VAFx(vx)*ssqEx));
  # (matlab code - here vx is an index because VAFx in the matlab code is a vector)

  # 1. the VAFx and SSQ Ex are multiplied
  # 2. (1-VAFx) is multiplied with SSQ Xtrue
  # 3. ratio of these two are calculated
  # 4. square-rooted

  data_noise <- data + fx*Ex

  return(data_noise)
}


