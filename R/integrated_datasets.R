#' Integrated datasets
#' 
#' 
#' This dataset contains custom dataset data. It has been made to be easy to edit
#' and allow to add any kind of custom dataset to DIANE as long as you follow the
#' specified nomenclature. This is especially usefull for the server version of
#' DIANE, who will parse the content of this file.
#' 
#'
#' 
#' @format A list. Each item of the list correspond to an organism. for each of
#' them, some fields can be set. 
#' \describe{
#'  \item{count}{Count data}
#'  \item{design}{Experimental design}
#'  \item{informations}{Free fields, with informations about how data have been collected.
#'  Can contain any informations you think are worth of being display to end users.
#'  Can contain either a string, or a list, where each elements of the list 
#'  correspond to a specific topic.}
#'  }
#' @examples
#' {
#'  print(names(integrated_datasets))
#'  print(head(integrated_datasets[["Lupinus albus"]][["Dataset name"]][["counts"]]))
#' }
"integrated_datasets"
