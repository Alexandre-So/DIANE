#' Draw expression heatmap
#'
#' @param data expression dataframe, with genes as rownames and samples as columns
#' @param subset subset of genes to be display
#' @param show_rownames show rownames or not
#' @param title plot title
#' @param log Show log(expression+1) in the heatmap if TRUE, expression if FALSE
#' @param profiles Show expression/mean(expression) for each gene if TRUE, expression if FALSE
#' @param conditions if NULL, shows all the conditions, else if character vector, shows only the required ones
#'
#'
#' @importFrom pheatmap pheatmap
#' @importFrom stringr str_split_fixed
#' @export
#' @examples
#' data("abiotic_stresses")
#' DIANE::draw_heatmap(abiotic_stresses$normalized_counts, subset = abiotic_stresses$heat_DEGs,
#' title = "Log expression for DE genes under heat stress")
draw_heatmap <-
  function(data,
           subset = NULL,
           show_rownames = FALSE,
           title = "Expression dataset",
           log = TRUE,
           profiles = FALSE,
           conditions = NULL) {
    if (is.null(subset)) {
      sample_subset <- sample(rownames(data), size = min(length(rownames(data)), 100))
    }
    else
      sample_subset <- subset
    
    if (sum(stringr::str_detect(rownames(data), paste0(sample_subset, collapse = '|'))) == 0) {
      stop("The required subset of genes was not found in expression data rownames")
    }
    
    if (is.null(conditions))
      conds <- colnames(data)
    else
      conds <-
        colnames(data)[stringr::str_split_fixed(colnames(data), '_', 2)[, 1] %in% conditions]
    
    if (log)
      data <- log(data + 1)
    if (profiles)
      data <- data / rowMeans(data)
    
    if (length(conds) == 0) {
      stop("The required conditions were not found in the expression data")
    }
    
    mat <- data[sample_subset, conds]
    
    sample <- stringr::str_split_fixed(colnames(mat), '_', 2) [, 1]
    samples <- data.frame(sample, row.names = colnames(mat))
    
    pheatmap::pheatmap(
      mat,
      color = grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 7, name = "YlGnBu"))(100),
      annotation_col = samples,
      show_rownames = show_rownames,
      main = title,
      fontsize = 17, border_color = NA
    )
  }



#' Draw distributions of expression data
#'
#' @param data expression dataframe, with samples as columns and genes as rows
#' @param type one of the following : boxplot, density, density_ridges
#' @export
#' @examples
#' data("abiotic_stresses")
#' DIANE::draw_distributions(abiotic_stresses$normalized_counts, type = "boxplot")
#' DIANE::draw_distributions(abiotic_stresses$raw_counts)
draw_distributions <- function(data, type = "boxplot") {
  
  # Check input
  if(! type %in% c("boxplot", "density", "density_ridges")){
    stop("type must be one of the following : boxplot, density, density_ridges")
  }
  
  
  d <-
    suppressMessages(reshape2::melt(log(data[sample(rownames(data),
                                                    replace = FALSE,
                                                    size = round(dim(data)[1] / 4, 0)),] + 1)))
  
  colnames(d)[c(length(colnames(d)) - 1, length(colnames(d)))] <-
    c("sample", "logCount")
  
  d$condition <- stringr::str_split_fixed(d$sample, "_", 2)[, 1]
  
  if (type == "boxplot") {
    g <-
      ggplot2::ggplot(data = d, ggplot2::aes_string(x = "sample", y = "logCount"))
    g <- g + ggplot2::geom_boxplot(
      alpha = 0.5,
      lwd = 1,
      ggplot2::aes(fill = condition),
      outlier.color = "black",
      outlier.alpha = 0.1
    )
  } else if(type == "density_ridges"){
    g <-
      ggplot2::ggplot(data = d,
                      ggplot2::aes_string(y = "sample", x = "logCount", color = "condition")) +
      ggridges::geom_density_ridges(fill = "#EAEFF3")
  } else {
    g <-
      ggplot2::ggplot(data = d,
                      ggplot2::aes(x = logCount)) + ggplot2::theme_bw() +
      ggplot2::geom_line(ggplot2::aes(color=sample), stat="density", linewidth=0.5, alpha=0.5)
  } 
  
  g <-
    g + ggplot2::theme_bw() + ggplot2::theme(
      plot.title = ggplot2::element_text(size = 22, face = "bold"),
      strip.text.x = ggplot2::element_text(size = 20),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 20, face = "bold"),
      legend.text = ggplot2::element_text(size = 22, angle = 0),
      axis.text.y = ggplot2::element_text(size = 18, angle = 30),
      axis.text.x = ggplot2::element_text(
        size = 15,
        angle = -50,
        hjust = 0,
        colour = "grey50"
      ),
      legend.text.align = 1,
      axis.title = ggplot2::element_text(size = 24)
    )
  g
}



#' Draw PCA results Legacy
#'
#'
#' @description Draws variables contributions to principal components,
#' as well as the PCA screeplot.
#' First to fourth principal components are shown, except if there are
#' only 4 samples. In that case, 3 principal components are computed.
#' This function is the original DIANE function, which is preserved for 
#' reproductibility purpose.
#'
#' @param data normalized expression data with samples as columns and genes as rows.
#'
#' @export
#' @import ggplot2
#'
#' @examples
#' data("abiotic_stresses")
#' draw_PCA_legacy(abiotic_stresses$normalized_counts)
draw_PCA_legacy <- function(data) {
  # PCA computation
  # data <- log(data + 2)
  
  if (ncol(data) < 4) {
    stop(
      "The input expression file has too few conditions 
      for PCA to be interesting. It should have at least 4 samples."
    )
  }

  
  nf = 4
  
  
  if (ncol(data) == 4) {
    message(
      "The input expression file has few conditions (4), so
            only 3 principal components will be computed instead of the 4 by default."
    )
    nf = 3
    
  }
  data <- data / rowMeans(data)
  acp <-
    ade4::dudi.pca(
      data,
      center = TRUE,
      scale = TRUE,
      scannf = FALSE,
      nf = nf
    )
  
  acp$co$condition = stringr::str_split_fixed(rownames(acp$co), '_', 2)[, 1]
  acp$co$replicate = stringr::str_split_fixed(rownames(acp$co), '_', 2)[, 2]
  
  scree <-
    data.frame(
      component = seq(1:length(acp$eig)),
      eigen.values = acp$eig,
      explained.variance = round(acp$eig / sum(acp$eig) *
                                   100, 2)
    )
  scree <- scree[1:min(nrow(scree), 4), ]
  
  # Plots
  g1_2 <-
    ggplot2::ggplot(
      data = acp$co,
      ggplot2::aes(
        x = Comp1,
        y = Comp2,
        color = condition,
        label = condition,
        shape = replicate
      )
    ) + ggplot2::geom_text(
      color = "black",
      size = 6,
      alpha = 0.5,
      nudge_x = 0.07,
      nudge_y = 0.07
    ) +
    ggplot2::geom_point(size = 6, alpha = 0.7) + ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) + ggplot2::geom_vline(xintercept = 0) + ggplot2::geom_hline(yintercept = 0) +
    ggplot2::theme(
      legend.position = "none",
      title = ggplot2::element_text(size = 18, face = "bold")
    ) +
    ggplot2::ggtitle("Principal components 1 and 2") +
    ggplot2::xlab(paste("x-axis : cor. to Comp1 ", scree[1, "explained.variance"], "%")) +
    ggplot2::ylab(paste("y-axis : cor. to Comp2 ", scree[2, "explained.variance"], "%"))
  
  g2_3 <-
    ggplot2::ggplot(
      data = acp$co,
      ggplot2::aes(
        x = Comp2,
        y = Comp3,
        color = condition,
        label = condition,
        shape = replicate
      )
    ) + ggplot2::geom_text(
      color = "black",
      size = 6,
      alpha = 0.5,
      nudge_x = 0.07,
      nudge_y = 0.07
    ) +
    ggplot2::geom_point(size = 6, alpha = 0.7) + ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) + ggplot2::geom_vline(xintercept = 0) + ggplot2::geom_hline(yintercept = 0) +
    ggplot2::theme(
      legend.position = "none",
      title = ggplot2::element_text(size = 18, face = "bold")
    ) +
    ggplot2::ggtitle("Principal components 2 and 3") +
    ggplot2::xlab(paste("x-axis : cor. to Comp2 ", scree[2, "explained.variance"], "%")) +
    ggplot2::ylab(paste("y-axis : cor. to Comp3 ", scree[3, "explained.variance"], "%"))
  
  
  if (ncol(data) > 4) {
    g3_4 <-
      ggplot2::ggplot(
        data = acp$co,
        ggplot2::aes(
          x = Comp3,
          y = Comp4,
          color = condition,
          label = condition,
          shape = replicate
        )
      ) + ggplot2::geom_text(
        color = "black",
        size = 6,
        alpha = 0.5,
        nudge_x = 0.07,
        nudge_y = 0.07
      ) +
      ggplot2::geom_point(size = 6, alpha = 0.7) + ggplot2::xlim(-1, 1) +
      ggplot2::ylim(-1, 1) + ggplot2::geom_vline(xintercept = 0) + ggplot2::geom_hline(yintercept = 0) +
      ggplot2::theme(
        legend.position = "bottom",
        title = ggplot2::element_text(size = 18, face = "bold"),
        legend.text = ggplot2::element_text(size = 18),
        legend.text.align = 1
      ) +
      ggplot2::ggtitle("Principal components 3 and 4") +
      ggplot2::xlab(paste("x-axis : cor. to Comp3 ", scree[3, "explained.variance"], "%")) +
      ggplot2::ylab(paste("y-axis : cor. to Comp4 ", scree[4, "explained.variance"], "%"))
  }
  
  screeplot <- ggplot2::ggplot(
    scree,
    ggplot2::aes(
      y = explained.variance,
      x = component,
      fill = component,
      label = paste(round(explained.variance, 1), '%')
    )
  ) +
    ggplot2::geom_bar(stat = "identity") + ggplot2::geom_text(size = 6,
                                                              vjust = 1.6,
                                                              color = "white") +
    ggplot2::ggtitle("PCA Screeplot") + ggplot2::theme(
      legend.position = "none",
      title = ggplot2::element_text(size = 18, face = "bold")
    )
  
  if (ncol(data) == 4)
    gridExtra::grid.arrange(g1_2, g2_3, screeplot, ncol = 2)
    
  else
    gridExtra::grid.arrange(g1_2, g2_3, g3_4, screeplot, ncol = 2)
}


#' Draw gene expression levels
#'
#' The normalized counts of the desired genes in the specified conditions are
#' shown. Please limit the number of input genes for readability reasons (up to 10 genes).
#'
#' @param data normalized expression dataframe, with genes as rownames and
#' conditions as colnames.
#' @param genes character vector of genes to be plotted (must be contained in
#' the rownames of data)
#' @param conds conditions to be shown on expression levels (must be contained in
#' the column names of data before the _rep suffix). Default : all conditions.
#' @param gene.name.size size of the facet plot title font for each gene. Default : 12
#' @param log2_count transform count using the log2 function. A pseudocount of 1 is added to
#' avoid negative values.
#' @param start_from_zero set the beginning of the y axis to 0.
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' genes <- sample(abiotic_stresses$heat_DEGs, 4)
#' conditions <- c("C", "M", 'H', 'SH')
#' DIANE::draw_expression_levels(abiotic_stresses$normalized_counts,
#' genes = genes, conds = conditions)
draw_expression_levels <-
  function(data,
           genes,
           conds = unique(stringr::str_split_fixed(colnames(data), '_', 2)[, 1]),
           gene.name.size = 12,
           log2_count = FALSE,
           start_from_zero = FALSE) {
    
    # trimming the gene names to allow more flexible use in the UI
    genes <- stringr::str_trim(genes)
    
    ## If no input gene is found.
    if ( sum(rownames(data) %in% genes) == 0) {
      stop("The required genes were not found in expression data rownames")
    }
    
    # If there is more than 10 genes.
    if ( sum(rownames(data) %in% genes) > 10) {
      stop("Please specify less than 10 genes, for readability reasons.")
    }
    
    conditions <-
      colnames(data)[stringr::str_split_fixed(colnames(data), '_', 2)[, 1] %in% conds]
    if (length(conditions) == 0) {
      stop("The required conditions were not found in the expression data")
    }
    
    data <- as.data.frame(data)
    if(log2_count == TRUE){
      data <- log2(data+1)
    }
    data$gene <- rownames(data)

    d <-
      suppressMessages(reshape2::melt(as.data.frame(data[intersect(rownames(data), genes), c(conditions, 'gene')])))
    d$condition <- stringr::str_split_fixed(d$variable, '_', 2)[, 1]
    d$replicate <- stringr::str_split_fixed(d$variable, '_', 2)[, 2]
    
    ggplot2::ggplot(d,
                    ggplot2::aes(x = condition,
                                 y = value,
                                 color = replicate)) + 
      ggplot2::theme_bw() + 
     {if(start_from_zero) ggplot2::expand_limits(y=0)} + ###Make the y axis start at 0
      ggplot2::geom_point(size = 4, alpha = 0.8) +
      ggplot2::facet_wrap(~ gene, scales = "free") +
      # {if(log2_count){ ggplot2::ggtitle("Log2 Normalized expression levels") } else {ggplot2::ggtitle("Normalized expression levels")}} +
      ggplot2::ggtitle("Normalized expression levels") +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          size = 22,
          hjust = 0.5,
          face = "bold"
        ),
        strip.text.x = ggplot2::element_text(size = gene.name.size),
        legend.title = ggplot2::element_text(size = 20),
        legend.text = ggplot2::element_text(size = 18),
        axis.text.y = ggplot2::element_text(size = 22, angle = 320),
        axis.title.y = ggplot2::element_text(size = 20),
        axis.text.x = ggplot2::element_text(size = 15, angle = 20)
      ) + ggplot2::xlab("") +
      {if(log2_count){ ggplot2::ylab(expression(paste(log[2], " Normalized counts")) )} else {ggplot2::ylab("Normalized counts")}}
  }


##  ............................................................................
##  PCA Related functions                                                   ####

#' compute_pca
#' 
#' 
#' @description compute variables contributions to principal components,
#' as well as the PCA scree.
#' 
#' @param data normalized expression data with samples as columns and genes as rows.
#' @param kept_axes max number of component to keep.
#'
#' @import ggplot2
#' @import ade4
#'
#' @export
#'
#' @examples
#' data("abiotic_stresses")
#' pca <- compute_pca(abiotic_stresses$normalized_counts)
compute_pca <- function(data, kept_axes = 4){
  
  if (ncol(data) < 4) {
    stop(
      "The input expression file has too few conditions 
      for PCA to be interesting. It should have at least 4 samples."
    )
  }
  
  
  nf = kept_axes
  
  ###FIXME : allow to compute ncol(data) - 1 components ?
  if (ncol(data) == 4) {
    message(
      "The input expression file has few conditions (4), so
            only 3 principal components will be computed instead of the 4 by default."
    )
    nf = 3
    
  }
  
  data <- data / rowMeans(data)
  acp <-
    ade4::dudi.pca(
      data,
      center = TRUE,
      scale = TRUE,
      scannf = FALSE,
      nf = nf
    )
  
  acp$co$condition = stringr::str_split_fixed(rownames(acp$co), '_', 2)[, 1]
  acp$co$replicate = stringr::str_split_fixed(rownames(acp$co), '_', 2)[, 2]
  
  ###FIXME : Not an obligation to compute this here 
  acp$scree <-
    head(
      data.frame(
        component = seq(1:length(acp$eig)),
        eigen.values = acp$eig,
        explained.variance = round(acp$eig / sum(acp$eig) *
                                     100, 2)
      ), kept_axes)
  
  return(acp)
}

#' draw_specific_pca
#' 
#' 
#' @description plot PCA results.
#' 
#' @param pca PCA data obtained from compute_pca function.
#' @param component_1 First component to plot
#' @param component_2 Second component to plot
#' @param legend Display legend on the plot.
#'
#' @export
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#'
#' @examples
#' data("abiotic_stresses")
#' pca <- compute_pca(abiotic_stresses$normalized_counts)
#' draw_specific_pca(pca, 1, 2)
draw_specific_pca <- function(pca, component_1, component_2, legend = TRUE){
  
  acp_plot <- ggplot2::ggplot(data = pca$co,
                              ggplot2::aes_string(
                                x = paste0("Comp",component_1),
                                y = paste0("Comp",component_2),
                                color = "condition",
                                label = "condition",
                                shape = "replicate"
                              )) + 
                                ggplot2::theme_bw() +
                                ggrepel::geom_text_repel( ###REQUIRE ggrepel PACKAGE https://github.com/slowkow/ggrepel (geom_label_repel)
                                color = "black",
                                size = 6,
                                alpha = 0.5,
                                max.overlaps = 40
                                # nudge_x = 0.07,
                                # nudge_y = 0.07
                              ) +
    ggplot2::geom_point(size = 6, alpha = 0.7) + ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) + ggplot2::geom_vline(xintercept = 0) + ggplot2::geom_hline(yintercept = 0) +
    # ggplot2::theme_light() +
    ggplot2::theme(legend.position = "none", title = ggplot2::element_text(size = 18, face = "bold"), axis.text = ggplot2::element_text(size=14)) +
    ggplot2::ggtitle(paste0("Principal components ", component_1, " and ", component_2)) +
    ggplot2::xlab(paste("x-axis : cor. to Comp ", component_1, " ", pca$scree[component_1, "explained.variance"], "%")) +
    ggplot2::ylab(paste("y-axis : cor. to Comp ", component_2, " ", pca$scree[component_2, "explained.variance"], "%")) 
  
  if(legend){
    acp_plot <- acp_plot +
      # ggplot2::theme_light() +
      ggplot2::theme(
        legend.position = "bottom", title = ggplot2::element_text(size = 18, face = "bold"),
        legend.text = ggplot2::element_text(size = 14),
        axis.text = ggplot2::element_text(size=14), legend.spacing = ggplot2::unit(0.1, "cm"), legend.key.size = ggplot2::unit(0.1, "cm"), legend.spacing.x = ggplot2::unit(0.2, "cm"),
        legend.text.align = 1
      ) +
      ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 4))) +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 4)))
      
  }
  
  acp_plot
  
}

#' draw_pca_scree
#' 
#' 
#' @description Display PCA scree plot, which shows contribution of all the
#' computed components.
#' 
#' @param pca PCA data obtained from the compute_pca function.
#'
#' @export
#' @import ggplot2
#'
#' @examples
#' data("abiotic_stresses")
#' pca <- compute_pca(abiotic_stresses$normalized_counts)
#' draw_pca_scree(pca)
draw_pca_scree <- function(pca){
  
  scree_plot <- ggplot2::ggplot(pca$scree,
                                ggplot2::aes(
                                  y = explained.variance,
                                  x = component,
                                  fill = component,
                                  label = paste(round(explained.variance, 1), '%')
                                )) +
    ggplot2::theme_bw() +
    ggplot2::geom_bar(stat = "identity") + ggplot2::geom_text(size = max(4, 8-0.5*ncol(pca$c1)),
                                                              vjust = 1.6,
                                                              color = "white") +
    # ggplot2::theme_light() +
    ggplot2::ggtitle("PCA Screeplot") +  ggplot2::theme(legend.position = "none",
                                                        title = ggplot2::element_text(size = 18, face = "bold"), 
                                                        axis.text = ggplot2::element_text(size=14) 
    )
  
  scree_plot
}

#' Quick PCA
#' 
#' 
#' @description Draws variables contributions to principal components,
#' as well as the PCA screeplot.
#' First to fourth principal components are shown.
#' 
#' @param data normalized expression data with samples as columns and genes as rows.
#'
#' @export
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#'
#' @examples
#' data("abiotic_stresses")
#' quick_pca(abiotic_stresses$normalized_counts)
quick_pca <- function(data) {
  
 pca_results <- compute_pca(data = data, kept_axes = 4)
  
  ###FIXME : the last component will not exist if the number of condition is too low.
  if(ncol(pca_results$l1) >= 4){
    gridExtra::grid.arrange(
      draw_specific_pca(pca_results, 1, 2, legend = FALSE),
      draw_specific_pca(pca_results, 2, 3, legend = FALSE),
      draw_specific_pca(pca_results, 3, 4, legend = TRUE),
      draw_pca_scree(pca_results), newpage = FALSE,
      ncol = 2
    )
  } else {
    gridExtra::grid.arrange(
      draw_specific_pca(pca_results, 1, 2, legend = FALSE),
      draw_specific_pca(pca_results, 2, 3, legend = TRUE),
      draw_pca_scree(pca_results),
      ncol = 2
    )
  }
}

#' PCA plot correlation
#' 
#' 
#' @description Draw correlation of each conditions groups to the 
#' principal components. This is done using the great CorLevelPlot package, by
#' Kevin Blighe (https://github.com/kevinblighe/CorLevelPlot).
#' @export
#' @param pca result from the "compute_pca" function. 
#' @param design design matrix
#' @param plotRsquared plot R-squared values
#' @import ggplot2
#' @import CorLevelPlot
#'
#' @examples
#' data("abiotic_stresses")
#' pca <- compute_pca(abiotic_stresses$normalized_counts)
#' pca_plot_correlation(pca, abiotic_stresses$design)
pca_plot_correlation <- function(pca, design = NULL, plotRsquared = FALSE){
  
  ###First, we check that design is correct. The numbers in the design formula must be n = 0, n+1, n+2... And nothing else.
  if(!is.null(design)){
    for(i in colnames(design)){
      factorline <- as.numeric(factor(design[[i]])) - 1
      if(!all(factorline == design[[i]])){
        stop(paste0(
          "There is a problem with your design.",
          " The column ", i ," does not respect the design synthax.\n",
          "This column contains c(", paste(design[[i]], collapse = ","),
          "). Here is an int about what it should contain c(",
          paste(factorline, collapse = ","), ").", collapse  = ""
        ))
        # return(FALSE)
      }
    }
  }
  
  if(is.null(design)){
    samples <- colnames(pca[["tab"]])
    conditions <- unique(stringr::str_split_fixed(samples, '_', 2)[, 1])
    design <- data.frame(row.names = samples)
    design[,conditions] <- NA
    design <- sapply(colnames(design), function(x){stringr::str_split_fixed(samples, '_', 2)[, 1] %in% x}) + 0
    rownames(design) <- samples
  } else {
    samples <- colnames(pca[["tab"]])
    conditions <- rownames(design)
    design_condition <- colnames(design)
    metadata <-  setNames(data.frame(matrix(ncol = length(design_condition), nrow = 0)), design_condition)
    for(i in samples){
      sample_name <- stringr::str_split_fixed(i, '_', 2)[, 1] 
      corresponding_line <- design[sample_name == conditions,]
      metadata[i,] <- corresponding_line
    }
    design <- metadata
  }
  
  # source : https://github.com/kevinblighe/CorLevelPlot
  CorLevelPlot::CorLevelPlot(data = cbind(pca$co, design),
                             x = colnames(pca$co)[1:(length(colnames(pca$co)) - 2)],
                             y = colnames(design),
                             cexTitleX = 2.0,
                             rotTitleX = 0,
                             fontTitleX = 2,
                             titleY = "Design",
                             cexTitleY = 2.0,
                             rotTitleY = 90,
                             fontTitleY = 2,
                             posLab = "topright",
                             # col = c("blue1", "skyblue", "white", "pink", "red1"),
                             col = c("#c00000", "#d94b2d", "white", "#5fbf64", "#2f6f46"),
                             posColKey = "bottom",
                             cexLabColKey = 1.5,
                             cexCorval = 1.5,
                             fontCorval = 2,
                             rotLabX = 45,
                             scale = TRUE,
                             main = "Correlation",
                             colFrame = "white", 
                             plotRsquared = plotRsquared
  )
}

#' download_plot_hd
#' 
#' 
#' @description use in shiny to download hd versions of plots. Only aim to make
#' code more readable.
#' 
#' @param plot a plot object, returned by ggplot or lattice
#' @param file a file name to store the plot in
#' @param type type of the plot object (ggplot, lattice or other)
#' @param format plot output format : png, pdf, svg, tiff.
#' @param res plot resolution. Only used for png and tiff
#' @param width plot width
#' @param height plot height
#' @param plot_error display a plot showing an error message instead of plotting
#' anything.
#'
#' @export
#' @importFrom ggplot2 ggsave
#' @noRd
download_plot_hd <- function(plot = NULL, file = NULL, type = "ggplot", format = "png", res=300, width = 16, height = 10, plot_error = FALSE){
  
  ###An easy way to plot an error message. Used to simplify error handling in shiny.
  if(plot_error){
    golem::print_dev("plot_error")
    plot = NULL
    plot = ggplot2::ggplot() + ggplot2::annotate("text",  x = 4, y = 25, size=8, label = "There was an error downloading the plot.\nYou can contact the authors if you need\nmore informations.") + ggplot2::theme_void()
    ggplot2::ggsave(filename = file, plot = plot, device = format, width = 8, height = 3)
    return()
  }
  
  if (! format %in% c("pdf", "png", "svg", "tiff")) {
    stop("Format must be one of the following : pdf, png, tiff or svg.")
  }
  
  ###Just to avoid too big plots.
  if (width > 50 || height > 50 || width <= 1 || height <= 1) {
    stop("Width and height must be between 1 and 50")
  }

  # golem::print_dev("Saving a plot.")
  # if( type == "ggplot"){
    # ggplot2::ggsave(plot = plot, device = format, filename = file, width = width, height = height, dpi = res)
  # } else {
  if(!plot_error){
    if(format=="png"){
      png(file, width = width, height = height, res = res, units = "in")
      print(plot)
    } else if (format=="pdf"){
      pdf(file, width = width, height = height)
      print(plot)
    } else if (format=="svg"){
      svg(file, width = width, height = height)
      print(plot)
    } else if (format == "tiff"){
      tiff(file, width = width, height = height, res = res, units = "in", compression = "lzw")
      print(plot)
    } else {
      stop("Output plot format not suported.")
    }
    dev.off()
  }
  # } 
}

#' Counts ready for a correlation, with the input contract enforced
#'
#' @param data a matrix or data.frame of counts, samples as columns
#' @param conds conditions to keep
#' @return the selected counts
#' @noRd
usable_counts <- function(data, conds) {
  if (is.data.frame(data)) data <- as.matrix(data)
  if (!is.matrix(data) || !is.numeric(data))
    stop("`data` must be a numeric matrix of counts, samples as columns")
  if (is.null(colnames(data)))
    stop("`data` needs sample names as column names")

  prefix <- stringr::str_split_fixed(colnames(data), '_', 2)[, 1]
  absent <- setdiff(conds, prefix)
  if (length(absent))
    warning("condition(s) not found in the expression data, ignored : ",
            paste(absent, collapse = ", "))
  keep <- prefix %in% conds
  if (!any(keep))
    stop("The required conditions were not found in the expression data")

  data <- data[, keep, drop = FALSE]
  if (ncol(data) < 2L) stop("at least two samples are needed, got ", ncol(data))
  if (nrow(data) < 3L) stop("at least three genes are needed, got ", nrow(data))
  if (any(data < 0, na.rm = TRUE))
    stop("negative values in `data` : raw or normalised counts are expected")
  # petites valeurs non entieres : presque surement du log deja pris
  if (max(data, na.rm = TRUE) < 50 && any(data %% 1 != 0, na.rm = TRUE))
    warning("`data` looks already log transformed ; counts are expected, ",
            "log2(x + 1) is applied by the function itself")
  data
}


#' sample_correlation
#'
#' @description Correlation matrix between samples, computed on log2(counts + 1).
#' Shared by the static and interactive correlation heatmaps.
#'
#' Expects counts, raw or normalised, never already transformed : the log is
#' applied here. Note that it only affects \code{pearson} : \code{spearman} and
#' \code{kendall} work on ranks, which a monotonic transform leaves untouched.
#'
#' @param data a matrix of counts, samples as columns. At least two samples and
#' three genes, no negative value.
#' @param conds conditions to keep. Default : all of them. Conditions that are
#' not in the data are ignored, with a warning.
#' @param correlation_method correlation method for the cor function. Can be pearson,
#' kendall or spearman.
#'
#' @return a square, unrounded correlation matrix
#' @export
#' @importFrom stats cor
#' @examples
#' data("abiotic_stresses")
#' sample_correlation(abiotic_stresses$raw_counts)
sample_correlation <-
  function(data,
           conds = unique(stringr::str_split_fixed(colnames(data), '_', 2)[, 1]),
           correlation_method = "pearson") {
    stats::cor(log2(usable_counts(data, conds) + 1), method = correlation_method)
  }


#' detect_sample_outliers
#'
#' @description Flags suspicious samples from the sample to sample correlation
#' matrix. Companion to the correlation heatmaps : same matrix, named samples
#' instead of a pattern to spot by eye.
#'
#' Three separate triggers, any one of which flags a sample. They are not
#' statistically independent : all three derive from the same correlation
#' matrix, and \code{gap} and \code{overall} are both built from a sample's
#' correlation to its replicates and to the other conditions.
#' \itemize{
#'   \item \code{deviation} too low : the sample agrees with its replicates less
#'   than they agree with each other. The reference is the median correlation
#'   \emph{between} the other replicates, so the sample never contributes to its
#'   own reference. Needs at least three replicates, \code{NA} below that ;
#'   \item \code{gap} too low : the sample is barely closer to its own replicates
#'   than to the other conditions, or actually closer to another condition ;
#'   \item \code{overall} too low, \emph{and} low within its own group : the
#'   sample correlates poorly with everything. The second half matters, without
#'   it a whole condition that is biologically distant flags every one of its
#'   members.
#' }
#' The three are needed : a degraded sample correlates badly with everything, so
#' its within and between correlations drop together and \code{gap} barely moves ;
#' and where whole conditions are distant, the spread of \code{overall} is
#' dominated by that distance, so only \code{deviation} sees a sample that is
#' merely worse than its own replicates.
#'
#' Centres are medians, not means, so one bad replicate does not drag its whole
#' group below the threshold. A sample is flagged when it is both \code{k} MADs
#' under the median and at least \code{delta} below it : the relative part
#' adapts to datasets whose correlations span different ranges, the absolute
#' part keeps a near zero spread from turning rounding noise into a warning.
#'
#' Limits worth keeping in mind : a flag is a reason to look, not proof of a
#' mislabelled sample or a technical failure. With two replicates
#' \code{deviation} cannot be computed at all, and with three replicates of
#' which two are degraded, the healthy one can be the flagged one. The verdict
#' also depends on \code{conds} : the thresholds are computed on the samples
#' that are kept, so a different selection can give a different answer for the
#' same sample.
#'
#' @param data a matrix of counts, samples as columns. This will be converted to
#' log2+1. Samples with constant or non finite counts carry no signal : they are
#' reported as unusable rather than scored.
#' @param conds conditions to keep. Default : all of them
#' @param correlation_method correlation method for the cor function. Can be pearson,
#' kendall or spearman.
#' @param k number of MADs below the median under which a sample is flagged, on
#' each trigger. Default : 5. Lower it to widen the net, raise it to quieten the
#' warning. \code{stats::mad} scales by 1.4826, so a MAD matches a standard
#' deviation on normal data ; correlation scores are not normal, so do not read
#' \code{k} as a false positive rate.
#' @param delta minimum departure from the median, in correlation units, below
#' which a sample is not flagged whatever the MAD says. Default : 0.05.
#'
#' @return a data.frame, one row per sample, most suspicious first : sample,
#' condition, within (median correlation to its own replicates), between (median
#' correlation to the other conditions), gap, overall (median correlation to
#' every other sample), deviation, flagged, reason. Unusable samples come first,
#' with NA scores. A condition with a single replicate gives NA on within, gap
#' and deviation, and is only checked on overall.
#' @export
#' @importFrom stats median mad sd ave
#' @examples
#' data("abiotic_stresses")
#' head(detect_sample_outliers(abiotic_stresses$raw_counts))
detect_sample_outliers <-
  function(data,
           conds = unique(stringr::str_split_fixed(colnames(data), '_', 2)[, 1]),
           correlation_method = "pearson",
           k = 5,
           delta = 0.05) {

    positive_scalar <- function(v)
      is.numeric(v) && length(v) == 1L && is.finite(v) && v > 0
    if (!positive_scalar(k))     stop("`k` must be a single finite positive number")
    if (!positive_scalar(delta)) stop("`delta` must be a single finite positive number")

    X <- log2(usable_counts(data, conds) + 1)

    # un echantillon entierement non fini emporterait tous les genes : on l'ecarte
    # d'abord, puis les genes non finis restants, puis les colonnes sans variance.
    void <- apply(X, 2, function(v) !any(is.finite(v)))
    X <- X[, !void, drop = FALSE]
    if (ncol(X)) X <- X[apply(X, 1, function(v) all(is.finite(v))), , drop = FALSE]
    flat <- if (nrow(X) < 2L) rep(TRUE, ncol(X)) else apply(X, 2, stats::sd) == 0
    unusable <- c(names(void)[void], colnames(X)[flat])
    X <- X[, !flat, drop = FALSE]
    if (ncol(X) < 2L) stop("fewer than two usable samples left after quality checks")

    cm <- stats::cor(X, method = correlation_method)
    samples <- colnames(cm)
    condition <- stringr::str_split_fixed(samples, '_', 2)[, 1]
    n <- length(samples)

    within <- between <- overall <- deviation <- rep(NA_real_, n)
    for (i in seq_len(n)) {
      same  <- setdiff(which(condition == condition[i]), i)
      other <- which(condition != condition[i])
      if (length(same))  within[i]  <- stats::median(cm[i, same])
      if (length(other)) between[i] <- stats::median(cm[i, other])
      overall[i] <- stats::median(cm[i, -i])
      if (length(same) >= 2L) {
        pairs <- cm[same, same, drop = FALSE]
        deviation[i] <- within[i] - stats::median(pairs[upper.tri(pairs)])
      }
    }
    gap <- within - between
    # un groupe entier bas est une distance biologique, pas un defaut technique
    group_overall <- stats::ave(overall, condition,
                                FUN = function(v) stats::median(v, na.rm = TRUE))

    low <- function(x) {
      if (all(is.na(x))) return(rep(NA, length(x)))
      centre <- stats::median(x, na.rm = TRUE)
      spread <- stats::mad(x, na.rm = TRUE)
      relative <- if (is.finite(spread) && spread > 0)
        x < centre - k * spread else rep(TRUE, length(x))
      out <- relative & (x < centre - delta)
      out[is.na(x)] <- NA
      out
    }
    fired <- function(v) v %in% TRUE
    bad_dev <- low(deviation)
    bad_gap <- low(gap)
    bad_all <- fired(low(overall)) & fired(low(overall - group_overall))

    # les libelles decrivent le calcul : la formulation forte n'est utilisee que
    # lorsque le signe de gap la justifie
    label <- cbind(
      ifelse(fired(bad_dev),
             "agrees with its replicates less than they agree with each other", NA),
      ifelse(fired(bad_gap) & !is.na(gap) & gap < 0,
             "closer to another condition than to its own replicates",
             ifelse(fired(bad_gap),
                    "barely closer to its replicates than to other conditions", NA)),
      ifelse(bad_all,
             "low correlation to every sample, including its own replicates", NA))
    reason <- apply(label, 1, function(v) {
      v <- v[!is.na(v)]; if (length(v)) paste(v, collapse = ", ") else NA_character_
    })

    # meme regle que low() : un critere dont la MAD est nulle ne classe pas non plus
    mads <- function(x) {
      spread <- stats::mad(x, na.rm = TRUE)
      if (all(is.na(x)) || !is.finite(spread) || spread == 0)
        return(rep(NA_real_, length(x)))
      (x - stats::median(x, na.rm = TRUE)) / spread
    }
    severity <- pmin(mads(gap), mads(overall), mads(deviation), na.rm = TRUE)

    res <- data.frame(
      sample = samples, condition = condition,
      within = within, between = between, gap = gap,
      overall = overall, deviation = deviation,
      flagged = fired(bad_dev) | fired(bad_gap) | bad_all, reason = reason,
      stringsAsFactors = FALSE
    )
    res <- res[order(severity, na.last = TRUE), ]
    if (length(unusable))
      res <- rbind(data.frame(
        sample = unusable,
        condition = stringr::str_split_fixed(unusable, '_', 2)[, 1],
        within = NA_real_, between = NA_real_, gap = NA_real_,
        overall = NA_real_, deviation = NA_real_, flagged = TRUE,
        reason = "no usable signal : constant or non finite counts",
        stringsAsFactors = FALSE), res)
    rownames(res) <- NULL
    res
  }


#' draw_correlation_heatmap
#'
#' @description draw a heatmap of pearson correlation between conditions.
#' Display only two digits.
#'
#' @param data a matrix of count. This will be converted to log2+1
#' @param conds if NULL, shows all the conditions, else if character vector, shows only the required ones
#' @param correlation_method correlation method for the cor function. Can be pearson,
#' kendall or spearman.
#' @param low_color color for low correlation. Default : #db4760
#' @param high_color color for high correlation. Default : #4169e1
#' @param mid_color color for values in between high and low. Default : white
#' @param font_size font size for the text in cells.
#' @param limits vector with 3 elements, the lower limit, the middle, and the high.
#'
#' @export
#' @import ggplot2 reshape2 stats
#' @examples
#' data("abiotic_stresses")
#' draw_correlation_heatmap(abiotic_stresses$normalized_counts)
draw_correlation_heatmap <-
  function(data = NULL,
           conds = unique(stringr::str_split_fixed(colnames(data), '_', 2)[, 1]),
           correlation_method = "pearson",
           low_color = "#db4760",
           high_color = "#4169e1",
           mid_color = "white",
           font_size = 3,
           limits = NULL) {

  # rounded to 2 digits : that is what the cell labels show
  correlation = round(sample_correlation(data, conds, correlation_method), 2)
  data_melt <- reshape2::melt(correlation)
  if(is.null(limits)){
    lowest = min(data_melt[["value"]], na.rm = TRUE)
    limits = c(lowest, (lowest + 1) / 2, 1)
  }
  ggheatmap <- ggplot2::ggplot(data_melt, ggplot2::aes(Var2, Var1, fill = value))+
    ggplot2::geom_tile(color = "white", linewidth = 0.1) +
    # scale_fill_gradient2(low = "#9b1c31", high = "#92D9A2", mid = "white",
    ggplot2::scale_fill_gradient2(low = low_color, high = high_color, mid = mid_color,
                                  limit = c(limits[1],limits[3]), midpoint = limits[2], space = "Lab",
                                  name=paste0(correlation_method,"\nCorrelation")) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                       size = 8, hjust = 1)) +
    ggplot2::labs(y= "Samples", x = "Samples") +
    ggplot2::geom_text(ggplot2::aes(Var2, Var1, label = value), color = "black", size = font_size) +
    ggplot2::scale_y_discrete(limits=rev) +
    ggtitle(paste0(correlation_method," Correlation between samples"))

  return(ggheatmap)
}


#' draw_correlation_heatmap_interactive
#'
#' @description Interactive version of \code{draw_correlation_heatmap}, for narrow
#' boxes and high sample counts : hovering gives both sample names and the
#' correlation, so axis labels and cell values are not needed.
#' Returns a plotly object, which \code{download_plot_hd} cannot export : use
#' \code{draw_correlation_heatmap} for downloads.
#'
#' @param data a matrix of counts. This will be converted to log2+1
#' @param conds if NULL, shows all the conditions, else if character vector, shows only the required ones
#' @param correlation_method correlation method for the cor function. Can be pearson,
#' kendall or spearman.
#' @param low_color color for low correlation. Default : #F7FCF5
#' @param mid_color color for values in between high and low. Default : #74C476
#' @param high_color color for high correlation. Default : #00441B
#' @param limits vector with 2 elements, the lower and the higher limit of the
#' color scale. Default : the range of the observed correlations.
#' @param title plot title, or NULL for none.
#'
#' @export
#' @importFrom stats cor
#' @examples
#' data("abiotic_stresses")
#' draw_correlation_heatmap_interactive(abiotic_stresses$normalized_counts)
draw_correlation_heatmap_interactive <-
  function(data = NULL,
           conds = unique(stringr::str_split_fixed(colnames(data), '_', 2)[, 1]),
           correlation_method = "pearson",
           low_color = "#F7FCF5",
           mid_color = "#74C476",
           high_color = "#00441B",
           limits = NULL,
           title = NULL) {

    correlation <- sample_correlation(data, conds, correlation_method)
    samples <- colnames(correlation)
    if (is.null(limits)) limits <- range(correlation, na.rm = TRUE)

    # samples top to bottom, to match the static version
    axis_x <- list(title = "", categoryorder = "array", categoryarray = samples,
                   tickfont = list(size = 9))
    axis_y <- list(title = "", categoryorder = "array", categoryarray = rev(samples),
                   tickfont = list(size = 9))

    plotly::plot_ly(
      x = samples, y = samples, z = correlation, type = "heatmap",
      colors = grDevices::colorRampPalette(c(low_color, mid_color, high_color))(64),
      zmin = limits[1], zmax = limits[2],
      hovertemplate = paste0("%{y}<br>%{x}<br>", correlation_method,
                             " r = %{z:.3f}<extra></extra>"),
      colorbar = list(title = list(text = paste0(correlation_method, "\ncorrelation"),
                                   font = list(size = 10)),
                      tickfont = list(size = 9), thickness = 12)
    ) |>
      plotly::layout(title = title, xaxis = axis_x, yaxis = axis_y,
                     margin = list(l = 60, b = 60, t = if (is.null(title)) 10 else 40))
  }
