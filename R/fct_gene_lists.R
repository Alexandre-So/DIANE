#' Parse a user provided gene list
#'
#' @description Splits free user input into gene names. Handles both ways of
#' providing a list, as they reduce to the same thing : a pasted text area gives
#' one string with line breaks, an uploaded file gives one string per line.
#'
#' Splits on whitespace, comma and semicolon, strips surrounding quotes, drops
#' empty tokens, deduplicates keeping the first occurrence. A multi column file
#' injects its extra columns and its header, which are then reported as not
#' found rather than silently guessed away.
#'
#' @param x character vector, or NULL. NULL, NA and empty strings give an empty
#' result, never an error.
#'
#' @return character vector of unique gene names, possibly empty
#'
#' @export
#' @examples
#' DIANE::parse_gene_list("AT1G01010\nAT1G01020\nAT1G01010")
#' DIANE::parse_gene_list(c("AT1G01010,AT1G01020", "", "  AT1G01030  "))
#' DIANE::parse_gene_list(NULL)
#'
#' data("abiotic_stresses")
#' genes <- DIANE::parse_gene_list(
#'   paste(head(abiotic_stresses$heat_DEGs, 5), collapse = "\n"))
#' length(genes)
parse_gene_list <- function(x) {
  if (is.null(x))
    return(character(0))

  x <- as.character(x)
  x <- x[!is.na(x)]

  if (length(x) == 0)
    return(character(0))

  tokens <- unlist(strsplit(x, "[[:space:],;]+"))
  tokens <- gsub("^['\"]+|['\"]+$", "", tokens)
  tokens <- tokens[nzchar(tokens)]

  unique(tokens)
}


#' Check that a gene list name can be used
#'
#' @description Gene lists share one namespace whatever their origin, and their
#' name is the key indexing clustering and network inference results. Returns
#' TRUE if usable, else the message to display, so callers can do
#' \code{if (!isTRUE(msg)) shinyalert::shinyalert(msg, type = "error")}.
#'
#' @param name candidate name, checked after \code{trimws()}
#' @param existing_names names already taken. Each caller decides what counts as
#' taken : the import module forbids all of them, the DEA only the imported ones
#' so that re running an analysis under the same name stays possible.
#' @param allow_key_separator whether \code{" + "} is allowed. It is the
#' separator of joined keys, so it is ambiguous, but DEA automatic names contain
#' it by construction (\code{(C + M) (H)}). Forbidding it reduces the ambiguity
#' without closing it : \code{A} + \code{B + C} already collides with
#' \code{A + B} + \code{C}.
#' @param max_nchar maximum length allowed
#'
#' @return TRUE, or a character message
#'
#' @export
#' @examples
#' DIANE::check_gene_list_name("Heat responsive family")
#' DIANE::check_gene_list_name("")
#' DIANE::check_gene_list_name("My list", existing_names = c("My list", "(C) (H)"))
#' DIANE::check_gene_list_name("stress A + stress B")
#' DIANE::check_gene_list_name("(C + M) (H)", allow_key_separator = TRUE)
#' DIANE::check_gene_list_name("None")
check_gene_list_name <- function(name,
                                 existing_names = character(0),
                                 allow_key_separator = FALSE,
                                 max_nchar = 50) {
  if (is.null(name) || length(name) != 1 || is.na(name))
    return("Please give a name to this gene list.")

  name <- trimws(as.character(name))

  if (!nzchar(name))
    return("Please give a name to this gene list.")

  if (nchar(name) > max_nchar)
    return(paste0("A gene list name cannot be longer than ",
                  max_nchar, " characters."))

  if (grepl("[[:cntrl:]]", name))
    return("A gene list name cannot contain line breaks or control characters.")

  if (!allow_key_separator && grepl(" + ", name, fixed = TRUE))
    return("A gene list name cannot contain ' + ', used by DIANE to name
           combinations of gene lists.")

  if (name %in% c("None", "FALSE"))
    return("This name is reserved by DIANE, please choose another one.")

  if (name %in% existing_names)
    return(paste0("The gene list '", name, "' already exists."))

  TRUE
}
