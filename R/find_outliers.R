#' find_outliers
#'
#' given a vector, find outliers
#'
#' @param hi
#'
#' @return hi
#'
#' @examples
#' hi
#'
#' @export

find_outliers <- function(y, coef = 1.5) {
  qs <- c(0, 0.25, 0.5, 0.75, 1)
  stats <- as.numeric(quantile(y, qs))
  iqr <- diff(stats[c(2, 4)])

  outliers <- y < (stats[2] - coef * iqr) | y > (stats[4] + coef * iqr)

  return(outliers)
}
