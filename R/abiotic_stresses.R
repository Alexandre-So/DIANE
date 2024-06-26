#' Combined abiotic stresses
#' 
#' 
#' This dataset contians the transcriptome of Arabidopsis thaliana plants exposed to global warming 
#' induced conditions. It was generated in the article "Molecular plant responses to combined abiotic 
#' stresses put a spotlight on unknown and abundant genes", by Sewelam et Al. in Journal of experimental
#' Botany, 2020.
#' The experimental perturbations studied are high tempreature, hight salinity and osmotic changes in 
#' the soil. Each factors has two levels, one of them considered as the reference, and the other one as 
#' the stress level.
#' 
#'
#' @source 
#' 
#' \describe{
#'  \item{Data download : }{\href{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE146206}{GEO accession}}
#'  \item{Publication :  }{\href{https://academic.oup.com/jxb/advance-article/doi/10.1093/jxb/eraa250/5842162#204457392}{Molecular 
#'  plant responses to combined abiotic stresses put a spotlight on unknown and abundant genes}}}
#' @format A named list with the following elements:
#' \describe{
#'  \item{raw_counts}{Dataframe of positive values. Raw transcript aboundances 
#'  as obtained after mapping and quantifying RNASeq reads. Rows are transcripts, and
#'  columns are experimental triplicate conditions}
#'  \item{normalized_counts}{Dataframe of positive values. Normalized transcript aboundances 
#'  Rows are transcripts, and columns are experimental triplicate conditions}
#'  \item{design}{Dataframe. Describes for each condition, the level of each factor.}
#'  \item{conditions}{Character vector. Gives the condition corresponding to each 
#'  column of the raw_counts element}
#'  \item{heat_DEGs}{Character vector. 692 genes detected as differentially expressed between control and heat stress, 
#'  with adjusted pavlue of 0.01 and absolute log fold change of 2.}
#'  \item{heat_DEGs_coseq_membership}{Named character vector. Cluster of the 692 genes detected as 
#'  differentially expressed between control and heat stress, 
#'  This membership was obtained after running coseq expression based clustering on those genes, 
#'  on all the dataset conditions, and resulted in 9 clusters.}
#'  \item{heat_DEGs_regulatory_links}{Matrix of size 25*654 genes. The matrix contains the importances of the 25 regulators
#'  on the 654 genes, and was infered with the \code{network_inference} method. Those gens are the ones detected as 
#'  differentially expressed between control and heat stress, and the regulators among them were grouped when correlated over 0.9.}
#'  \item{heat_edge_tests}{Statistical testing from the \code{edge_testing} function, applied to the heat_DEGs_regulatory_links element.
#'  It contains the pvalue for each edge of a prior network built on a desired density.}
#'  
#' }
#' @examples
#' {
#'  print(head(abiotic_stresses$raw_counts))
#'  print(abiotic_stresses$design)
#'  print(abiotic_stresses$conditions)
#' }
"abiotic_stresses"