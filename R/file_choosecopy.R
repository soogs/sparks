#' file_choosecopy
#'
#' addition to file_choose: it copies the path into clipboard. only works on linux.
#'
#' @param hi
#'
#' @return hi
#'
#' @examples
#' hi
#'
#' @export

file_choosecopy <- function(x){
  location <- file.choose()
  con <- pipe("xclip -selection clipboard -i", open="w")
  write(location, con)
  close(con)
}
