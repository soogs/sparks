#' Wsparsedata
#'
#' generates data from a sparse weights structure
#'
#' @param hi
#'
#' @return hi
#'
#' @examples
#' hi
#'
#' @export

Wsparsedata <- function(n, p, k, n_zeros = NULL, Winit = NULL, empirical = FALSE, seed, VAFx){
  mu <- rep(0,p)
  Sigma <- diag(1,p)

  if (n > p){
    set.seed(seed)
    dat <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma, empirical = empirical)
    dat <- scaleData(dat)
  } else if (p > n){
    set.seed(seed)
    dat <- mvtnorm::rmvnorm(n = n, mean = mu, sigma = Sigma)
    dat <- scaleData(dat)
  }
  svd1 <- svd(dat)

  v1 <- svd1$v[,1:k]

  makesparse <- function(eachrow) {
    eachrow[sort(abs(eachrow), index.return= T)$ix[1:n_zeros]] <- 0
    return(eachrow)
  }

  if (is.null(Winit)){
    v1 <- apply(v1, 2, makesparse)
  } else {
    v1[Winit == 0] <- 0
  }

  # normalize the columns of W
  v1  <- v1 %*% diag(1/sqrt(diag(t(v1) %*% v1)))

  svdp1 <- svd(t(dat %*% v1) %*% dat)
  pnew1 <- t(svdp1$u %*% t(svdp1$v))

  newdat1 <- dat %*% v1 %*% t(pnew1)

  # VAFx to control the amount of noise #
  # sum of squares of the X2 dataset
  ssqXtrue <- sum(newdat1^2)

  set.seed(seed)
  # sample from normal distribution (Ex = Error of X)
  Ex <- matrix(MASS::mvrnorm(n = n, mu = rep(1,p), Sigma = diag(p)), nrow = n, ncol = p)

  # centering the Ex matrix
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

  newdat2 <- newdat1 + fx*Ex

  returnobj <- list(X = newdat2, Xtrue  = newdat1, P = pnew1, W = v1)

  return(returnobj)
}

# tests ####
# inits <- matrix(0, nrow = 10, ncol = 2)
# inits[1:6,1] <- 1
# inits[5:10,2] <- 1
#
# hi <- Wsparsedata(n = 100, p = 10, k = 2, n_zeros = 4, empirical = FALSE, seed = 20, VAFx = 1, Winit = inits)
#
# hi$W
#
# spca1 <- spca.defiled(x = hi$X, K = 2, para = rep(6,2), type = "predictor", sparse = "varnum")
#
# spca2 <- spca.defiled(x = hi$X, K = 2, para = rep(6,2), type = "predictor", sparse = "varnum", Winit = hi$W)
#
# spca2$loadings
# spca1$loadings
#
# TuckerCoef(spca1$loadings, hi$W)
# TuckerCoef(spca2$loadings, hi$W)

#
# pca <- prcomp(hi$X)
# TuckerCoef(pca$rotation[,1:2], hi$W)
#
# spca1 <- spca.defiled(x = hi$X, K = 2, para = rep(6,2), type = "predictor", sparse = "varnum")
# TuckerCoef(spca1$loadings, hi$W)
