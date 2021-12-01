#' Yes No Flags
#'
#' Turns 1/0 into "Yes"/"No"
#'
#' @param x an integer vector
#' @return a character vector
#'
#' @examples
#'
#' flag <- c(0, 1, 1, 0, 0, 0)
#' yesno(flag)
#'
#' @export
yesno <- function(x) {
  UseMethod("yesno")
}

#' @export
yesno.numeric <- function(x) {
  if (!all(x %in% c(1, 0))) {
    warning(sprintf("non 1/0 values in %s", deparse(substitute(x))))
  }
  rtn <- rep(NA_character_, length = length(x))
  rtn[x == 1] <- "Yes"
  rtn[x == 0] <- "No"
  rtn
}

#' 1/0 Flags
#'
#' Turns Yes/No variables into 1/0 integers
#'
#' The input is forced to lowercase and only the first character, the "y" or
#' "n", is used to map to the 1/0 integer values.  The function allows for "1"
#' and "0" to be in the character vector as well.
#'
#' @param x a character vector
#' @return an integer vector
#'
#' @examples
#'
#' flag <- c("Y", "No", "NO", "no", "n", "YES", "Yes", "yEs", "1", "0")
#' onezero(flag)
#'
#' @export
onezero <- function(x) {
  UseMethod("onezero")
}

#' @export
onezero.character <- function(x) {
  x <- tolower(x)
  rtn <- rep(NA_integer_, length = length(x))
  rtn[grepl("^(y|1)", x)] <- 1L
  rtn[grepl("^(n|0)", x)] <- 0L
  rtn
}
