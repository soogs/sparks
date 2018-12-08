#' findLasso
#'
#' major idea/code originally by Niek. Thanks sir. Provides the lasso penalty values, given the number of zeros in the PCA coefficients matrix
#'
#' @param dat dataset
#' @param zeros vector: number of zeros wanted per component
#' @param R number of components
#' @param whichfunction choice between "spca_adj" or "rsvd_spca": which function do you want?
#' @param init initial values for the lasso penalties
#' @param ridge ridge penalty. default 1e-6
#' @param maxiterOut maximum iteration for all
#' @param maxiterIn maximum iteration within one component
#'
#' @return lasso, converged
#'
#' @examples
#' hi
#'
#' @export


findLasso <- function(dat, zeros, R, whichfunction = c("spca_adj", "rsvd_spca"), init, ridge = 1e-6, maxiterOut, maxiterIn){
  # zeros should be a vector

  estimatedzeros <- rep(0,R)
  lasso <- rep(init,R)

  converged <- FALSE

  iterOut <- 0

  while(abs(zeros - estimatedzeros) > 0 && iterOut <= maxiterOut ){
    iterOut <- iterOut + 1

    mixedorder <- sample(R)

    for (j in mixedorder){
      iterIn <- 0

      up <- init
      down <- 0

      estimatedzero <- 0
      while(abs(zeros[j] - estimatedzero) > 0 && iterIn <= maxiterIn){
        iterIn <- iterIn + 1
        lasso[j] <- (down + up)/2

        # lasso[-j] <- 0

        if(whichfunction == "spca_adj"){
          fit <- spca_adj(x = dat, K = R, para = lasso, type = "predictor", sparse = "penalty", inits = "SVD", lambda = ridge)
          estimatedzero <- sum(abs(fit$Wraw[,j]) < 1e-06)
        } else {
          fit <- rsvd_spca(dat = dat, R = R, lambda = lasso, penalty = "soft", ridge = ridge, maxiter = 1000, inits = "SVD")
          estimatedzero <- sum(abs(fit$V[,j]) < 1e-06)
        }

        if(zeros[j] > estimatedzero){
          down  <- lasso[j]
          # if the estimated zeros are not enough,
          # pull up the 'down'
        } else if (zeros[j] < estimatedzero){
          up  <- lasso[j]
          # if the estimated zeros are more than enough,
          # pull down the 'up'
        }
        # else (don't do anything)

        print(round(lasso,10))
      }

      if(j == R){
        if(whichfunction == "spca_adj"){
          estimatedzeros <- apply((abs(fit$Wraw) < 1e-06),2,sum)
        } else {
          estimatedzeros <- apply((abs(fit$V) < 1e-06),2,sum)
        }
      }
    }

  }

  if( iterOut < maxiterOut && iterIn < maxiterIn ){
    converged <- TRUE
  }
  return(list(lasso = lasso, converged = converged))
}
