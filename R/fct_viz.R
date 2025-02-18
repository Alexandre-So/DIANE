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
#' DIANE::draw_distributions(abiotic_stresses$normalized_counts, type = boxplot)
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
#'
#' @export
#' @importFrom ggplot2 ggsave
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
  require(ggplot2)
    
    conditions <-
      colnames(data)[stringr::str_split_fixed(colnames(data), '_', 2)[, 1] %in% conds]
    if (length(conditions) == 0) {
      stop("The required conditions were not found in the expression data")
    }
    
  data <- data[,colnames(data) %in% conditions]
    
  data <- log2(data+1)
  
  correlation = round(cor(data, method = correlation_method), 2)
  data_melt <- reshape2::melt(correlation)
  if(is.null(limits)){
    limits = c(
      min(data_melt[["value"]]),
      ((min(data_melt[["value"]]) + 1) / 2),
      1
    )
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
