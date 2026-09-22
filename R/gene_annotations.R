#' Gene annotations for Arabidopsis thaliana
#'
#' For each gene identifier, are provided its common name and description.
#' Other organisms carry their own annotations in their file under
#' \code{inst/extdata/organisms}, read with \code{\link{organism}}.
#'
#' @source 
#' \describe{
#'  \item{For Arabidopsis thaliana}{\url{http://bioconductor.org/packages/release/data/annotation/html/org.At.tair.db.html}
#'  {TAIR10 via the bioconductor package org.At.tair.db}}
#'  }
#' @format A named list with the following elements:
#' \describe{
#'  \item{Arabidopsis thaliana}{Dataframe with AGI terms as rownames, and two columns :
#'  description and name.}
#' }
#' @examples
#' {
#'  print(head(gene_annotations[["Arabidopsis thaliana"]]))
#' }
"gene_annotations"