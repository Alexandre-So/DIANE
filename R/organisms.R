#' Custom organisms index
#'
#' Every field of every custom organism except the two heavy ones, \code{annotation}
#' and \code{go}. Listing the organisms or grouping them by genus therefore loads
#' no annotation table. Each organism sits in its own file under
#' \code{inst/extdata/organisms}, read with \code{\link{organism}}.
#'
#' @format A named list, keyed by the name displayed in the interface.
#' \describe{
#'  \item{slug}{Base name of the file holding the full organism}
#'  \item{genus}{Genus, used to group organisms in the interface}
#'  \item{regulators}{Regulators. Can be left empty}
#'  \item{gene_exemple}{A gene exemple that will be displayed inside of the online interface}
#'  \item{informations}{Free fields, with informations about how data have been collected.
#'  Can contain any informations you think are worth of being display to end users.
#'  Can contain either a string, or a list, where each elements of the list
#'  correspond to a specific topic.}
#'  \item{n_genes}{Number of genes in the annotation}
#'  \item{has_go}{Whether a GO mapping is available}
#'  }
#' @seealso \code{\link{organism}}
#' @examples
#' {
#'  print(names(organisms_index))
#'  print(organisms_index[["Lupinus albus Var Amiga"]][["genus"]])
#' }
"organisms_index"


#' Read one custom organism
#'
#' Reads only the organism asked for, from its own file. Nothing is cached: the
#' app is launched on demand behind shinyproxy, so a cold start has to be cheap
#' and an in-process cache would never be warm.
#'
#' @param name Organism name, as found in \code{names(organisms_index)}.
#'
#' @return The organism: the fields of \code{\link{organisms_index}}, plus
#' \code{annotation} and \code{go}. \code{NULL} for an unknown name, so this
#' doubles as a membership test like the former \code{organisms} list.
#'
#' @export
#' @examples
#' {
#'  print(head(organism("Lupinus albus Var Amiga")[["annotation"]]))
#' }
organism <- function(name) {
  if (length(name) != 1 || is.na(name)) return(NULL)

  index <- DIANE::organisms_index
  if (!name %in% names(index)) return(NULL)

  read_organism_file(index[[name]][["slug"]])
}


#' Read one organism file
#'
#' @param slug Base name of the file, without extension.
#'
#' @return The organism.
#' @noRd
#' @note Under inst/ rather than data/ on purpose: pkgload::load_all() reads
#' every data/*.rda eagerly, which would load all organisms at startup.
read_organism_file <- function(slug) {
  path <- system.file("extdata", "organisms", paste0(slug, ".rds"), package = "DIANE")
  if (!nzchar(path)) {
    stop("Organism file not found: ", slug, call. = FALSE)
  }
  readRDS(path)
}
