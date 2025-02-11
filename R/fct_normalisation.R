#' Normalize raw count data
#' 
#' @description The function corrects for different sequencing depths bewteen samples.
#' It relies on the TCC package, to build a TCC-class object containing the raw counts 
#' and the conditions for each sample. The function calcNormFactors is then applied, 
#' and uses the method chosen by the user. It can, weather or not, proceed to a first step or 
#' removing potentially differentially expressed genes to have less biased normalisation
#' factors in the second normalization step. It returns a TCC object,
#' with an element norm_factors containing the computed normalization factors. 
#'
#' @param data raw counts to be normalized (data frame or matrix), with genes as rownames and
#' conditions as columns.
#' @param conditions condition of each column of the data argument. Default is
#' all the conditions in the experiment. (as defined by the underscore prefixes).
#' @param norm_method method used for normalization, between tmm or deseq2
#' @param deg_method method used for DEGs detection if chosen, between edgeR ou deseq2
#' @param fdr pvalue threshold for adjusted pvalues for DEGs detection if chosen
#' @param iteration weather or not to perform a prior removal of DEGs (TRUE or FALSE)
#' @details 
#' Filtering low counts is highly recommended after normalization, 
#' consider using the \code{DIANE::filter_low_counts}
#' function just after this function.
#' 
#' You can get the normalized expression matrix with \code{TCC::getNormalizedData(tcc)},
#' tcc being the result of \code{DIANE::normalize()} or \code{DIANE::filter_low_counts()}
#' @importFrom TCC calcNormFactors TCC
#' @return a TCC-Class object
#' @export
#' @examples
#' data("abiotic_stresses")
#' tcc_object <- DIANE::normalize(abiotic_stresses$raw_counts, 
#' abiotic_stresses$conditions, iteration = FALSE)
normalize <- function(data, 
                      conditions = stringr::str_split_fixed(colnames(data), '_', 2)[, 1], 
                      norm_method = "tmm", 
                      deg_method = "deseq2", 
                      fdr = 0.01,
                      iteration = TRUE){
  tcc <- TCC::TCC(count =  data, group = conditions)
  tcc <- suppressWarnings(suppressMessages(TCC::calcNormFactors(tcc, norm.method = norm_method, 
                                               test.method = deg_method, 
                              iteration = iteration, FDR = fdr, floorPDEG = 0.05)))
  return(tcc)
}

#' Remove low count genes
#' 
#' @description Removes genes having a sum of counts accross all samples
#' lesser than the specified threshold. It returns un aupdated TCC object,
#' which count element contains the filtered expression matrix.
#'
#' @param tcc data to be filtered to remove low count genes
#' @importFrom TCC filterLowCountGenes
#' @param thr the sum of counts across all samples to be exceeded for a gene
#' @export
#' @return a TCC-Class object
#' @examples
#' data("abiotic_stresses")
#' tcc_object <- DIANE::normalize(abiotic_stresses$raw_counts, 
#' abiotic_stresses$conditions, iteration = FALSE)
#' threshold = 10*length(abiotic_stresses$conditions)
#' tcc_object <- DIANE::filter_low_counts(tcc_object, threshold)
#' normalized_counts <- TCC::getNormalizedData(tcc_object)
filter_low_counts <- function(tcc, thr){
  tcc <- TCC::filterLowCountGenes(tcc, low.count = thr)
  return(tcc)
}

#' Remove low count genes condition wise.
#' 
#' @description Removes genes having a maximum median of counts accross all 
#' conditions lesser than the specified threshold. It returns un aupdated TCC 
#' object, which count element contains the filtered expression matrix. Compare
#' to the filter_low_counts function, it aims to preserve genes having medium
#' counts in a few batch of conditions. Code based on the filterLowCountGenes
#' function from the TCC package.
#'
#' @param tcc data to be filtered to remove low count genes
#' @param thr the sum of counts across all samples to be exceeded for a gene
#' @export
#' @return a TCC-Class object
#' @examples
#' data("abiotic_stresses")
#' tcc_object <- DIANE::normalize(abiotic_stresses$raw_counts, 
#' abiotic_stresses$conditions, iteration = FALSE)
#' threshold = 10*length(abiotic_stresses$conditions)
#' tcc_object <- DIANE::filter_low_count_condition_wise(tcc_object, threshold)
#' normalized_counts <- TCC::getNormalizedData(tcc_object)
filter_low_count_condition_wise <- function(tcc, threshold = 10, conditions = NULL)
{
  obj <- tcc$copy()
  conditions_median <- summarize_per_conditions(count_data = tcc$count, design = conditions)
  gene.keep = (do.call(pmax, conditions_median) > threshold)
  # return(gene.keep)
  obj$count <- obj$count[gene.keep, ]
  if (!is.null(obj$simulation$trueDEG) && length(obj$simulation$trueDEG) !=
      0)
    obj$simulation$trueDEG <- obj$simulation$trueDEG[gene.keep]
  if (!is.null(obj$estimatedDEG) && length(obj$estimatedDEG) !=
      0)
    obj$estimatedDEG <- obj$estimatedDEG[gene.keep]
  if (!is.null(obj$stat) && length(obj$stat) != 0) {
    for (i in 1:length(obj$stat)) {
      if (length(obj$stat[[i]]) == length(gene.keep))
        obj$stat[[i]] <- obj$stat[[i]][gene.keep]
    }
  }
  return(obj)
}


#' Summarize per conditions
#'
#' @param count_data count matrix, with genes as rows and conditions as columns.
#' @param design 
#'
#' @returns new matrix with median of counts per condition.
#' @export
#'
#' @noRd
summarize_per_conditions <- function(count_data, design = NULL) {
  if (is.null(design)) {
    design <- unique(stringr::str_remove(colnames(count_data), "_\\d+$"))
  }
  
  processed_counts <- lapply(design, function(cond) {
    selected_cols <- count_data[, stringr::str_detect(colnames(count_data), cond), drop = FALSE]
    row_medians <- matrixStats::rowMedians(as.matrix(selected_cols))
    setNames(as.data.frame(row_medians), cond)
  })
  
  return(do.call(cbind, processed_counts))
}
