#' Split a string on commas
#'
#' @param string A character vector with, at most, one element.
#' @inheritParams stringr::str_split
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' x <- "alfa,bravo,charlie,delta"
#' str_split_comma(x)
str_split_comma <- function(string) {
  stopifnot(is.character(string), length(string) <= 1)
  if (length(string) == 1) {
    stringr::str_split(string = string, pattern = ",")[[1]]
  } else {
    character()
  }
}

#' Split a string and converts it to numeric
#'
#' @param string A character vector with, at most, one element.
#' @param pattern A string denoting the separator by which the string parameter
#'    should be split on
#' @inheritParams stringr::str_split
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' x <- "1,2,3,4"
#' str_split_nums(x, pattern = ",")
str_split_nums <- function(string, pattern) {
  stopifnot(is.character(string), length(string) <= 1)
  if (length(string) == 1) {
    stringr::str_split(string = string, pattern = pattern)[[1]] |>
      as.numeric()
  } else {
    numeric()
  }
}
