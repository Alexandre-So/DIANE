#' Removes the null from a vector
#' 
#' @noRd
drop_nulls <- function(x){
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#' 
#' @param x,y Two elements to test, one potentially `NULL`
#' 
#' @noRd
"%||%" <- function(x, y){
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#' 
#' @param x,y Two elements to test, one potentially `NA`
#' 
#' @noRd
"%|NA|%" <- function(x, y){
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#' 
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#' 
#' @noRd
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList

