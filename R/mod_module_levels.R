#' module_levels UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList 
mod_module_levels_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$style({ ##TODO : put this in a css file. 
      ".download_plot_button {
        margin-top: 10px;
      }"
    }),
    
    # shiny::fluidRow(
      # shiny::column(12,
                    shiny::h1("Explore normalized gene expression"),
                    shiny::hr(),
      # ),
      
      shinybusy::add_busy_spinner(
        spin = "self-building-square",
        position = 'top-left',
        margins = c(70, 1200)
      ),
    
    shiny::fluidRow(
      shinydashboard::tabBox(
        title = "Explore normalized data",
        width = 12,
        height = "80vh",
        shiny::tabPanel(title = "PCA",
                        shiny::uiOutput(ns("pca_ui")),
                        shiny::fluidRow(shiny::column(12, shiny::includeMarkdown(system.file(
                            "extdata", "pca.md", package = "DIANE"))))
        ),
        shiny::tabPanel(title = "Visualize gene expression levels",
                        shiny::fluidRow(
                          shinydashboardPlus::box(
                            title = "Genes and conditions choice",
                            solidHeader = FALSE,
                            status = "success",
                            collapsible = TRUE,
                          closable = FALSE,
                          width = 12,
                          
                          shiny::uiOutput(ns("gene_choice")),
                          
                          shiny::uiOutput(ns("condition_choice")),
                          
                          shiny::column(2,
                            shiny::checkboxInput(inputId = ns("take_log_count"), label = "Take log2 of count", value = FALSE, width = "100%")
                            ),
                            shiny::column(2,
                              shiny::checkboxInput(inputId = ns("start_y_from_zero"), label = "Y axis start from 0", value = FALSE, width = "100%")
                            )
                            
                          ),
                          shinydashboardPlus::box(solidHeader = FALSE,
                                                  status = "success",
                                                  collapsible = TRUE,
                                                  closable = FALSE,
                                                  width = 12,
                                                  shiny::plotOutput(ns("expression_plot"), height = "700px"),
                                                  shiny::column(width = 12, align = "right", class="download_plot_button",
                                                                shinyWidgets::downloadBttn(outputId = ns("expression_plot_download"), "Download expression plot", style = "material-flat",
                                                                                           color = "success", icon = shiny::icon("image"), size = "sm"))
                          )
                        )  
        ),
        shiny::tabPanel(title = "Correlation plot",
                        shiny::fluidRow(
                          shinydashboardPlus::box(
                            title = "Parameters",
                            solidHeader = FALSE,
                            status = "success",
                            collapsible = TRUE,
                            closable = FALSE,
                            width = 12,
                            shiny::uiOutput(ns("correlation_condition_choice_ui"))
                          ),
                          shinydashboardPlus::box(
                            title = "Plot",
                            solidHeader = FALSE,
                            status = "success",
                            collapsible = TRUE,
                            closable = FALSE,
                            width = 12,
                            shiny::plotOutput(outputId = ns("correlation_heatmap"), height = "700px"),
                            shiny::column(width = 12, align = "right", class="download_plot_button",
                                          shinyWidgets::downloadBttn(outputId = ns("download_correlation_plot"), "Download correlation plot", style = "material-flat",
                                                                     color = "success", icon = shiny::icon("image"), size = "sm"))
                          )
                          )
                        )
      )
    ))
}

#' module_levels Server Function
#'
#' @noRd
mod_module_levels_server <- function(input, output, session, r){
  ns <- session$ns
  
  ###PCA computation when page load.
  pca_raw_results <- shiny::reactive({
    shiny::req(r$normalized_counts, r$conditions)
    compute_pca(r$normalized_counts, kept_axes = 10)
  })
  
  output$condition_choice <- shiny::renderUI({
    shiny::req(r$normalized_counts, r$conditions)
    
    shinyWidgets::checkboxGroupButtons(
      inputId = ns('input_conditions'),
      label = "Conditions to include to the expression levels plot:",
      choices = unique(r$conditions),
      justified = TRUE,
      checkIcon = list(yes = shiny::icon("ok",
                                         lib = "glyphicon")),
      selected = unique(r$conditions)
    )
  })
  
  
  output$correlation_condition_choice_ui <- shiny::renderUI({
    shiny::req(r$normalized_counts, r$conditions)
    
    shinyWidgets::checkboxGroupButtons(
      inputId = ns('correlation_condition_choice'),
      label = "Conditions to include to correlation plot",
      choices = unique(r$conditions),
      justified = TRUE,
      checkIcon = list(yes = shiny::icon("ok",
                                         lib = "glyphicon")),
      selected = unique(r$conditions)
    )
  })
  
  output$gene_choice <- shiny::renderUI({
    shiny::req(r$normalized_counts)
    
    shiny::textInput(ns("genes"), 
                     label = "Genes to plot, as identified in the Gene 
                     column of expression data. For several genes, they must be comma separated, without space, as in the example:", 
                     width = '100%',
                     value = paste0(sample(rownames(r$normalized_counts), 4), collapse = ','))
  })
  
  #   ____________________________________________________________________________
  #   profiles                                                                ####
  
  expression_plot_plot <- shiny::reactive({
    shiny::req(r$normalized_counts, r$conditions, input$genes)
    genes <- unlist(strsplit(input$genes, ','))
    shiny::req(length(genes) > 0)
    shiny::req(length(genes) < 10)
    
    shiny::req(sum(genes %in% rownames(r$normalized_counts)) > 0)
    
    draw_expression_levels(as.data.frame(r$normalized_counts),
                           conds = input$input_conditions,
                           genes = genes, gene.name.size = 22, log2_count = input$take_log_count, start_from_zero = input$start_y_from_zero)
  })
  
  
  output$expression_plot <- shiny::renderPlot({
    shiny::req(r$normalized_counts, r$conditions, input$genes)
    expression_plot_plot()
  })
  
  
  #   ____________________________________________________________________________
  #   pca                                                                     ####
  
  ##  ............................................................................
  ##  PCA renderUI                                                            ####
  
  ###UI for all the pca related things. Contains 3 tabs with 3 plotoutputs.
  output$pca_ui <- shiny::renderUI({
    if(is.null(r$normalized_counts)) {
      shinydashboardPlus::descriptionBlock(
        number = "Please normalize and filter raw data in previous tab",
        numberColor = "orange",
        rightBorder = FALSE
      )
    } else {
      # else shiny::plotOutput(ns('pca_plot'), height = "800px")
      shiny::tags$div(id = "tabset_advanced_pca_div",
                      shinydashboard::tabBox(
                        id = "tabset_advanced_pca",
                        # height = "450px",
                        width = NULL,
                        tabPanel("PCA Summary",
                                 shiny::plotOutput(ns('pca_plot'), height = "800px"),
                                 shiny::column(width = 12, align = "right", class="download_plot_button",
                                               ##TODO : put this button in a renderUI for error handling.
                                               shinyWidgets::downloadBttn(outputId = ns("download_quickpca"), "Download PCA plot", style = "material-flat",
                                                                          color = "success", icon = shiny::icon("image"), size = "sm"))
                        ),
                        tabPanel("Specific PCA plot",
                                 shiny::tagList(
                                   shiny::column(6, align = "center", class="download_plot_button",
                                                 shiny::selectInput(ns("component_1_choice"), "First component",
                                                                    as.character(1:(ncol(pca_raw_results()$co)-2)),
                                                                    selected = "1")
                                   ),
                                   shiny::column(6, align = "center",
                                                 shiny::selectInput(ns("component_2_choice"), "Second component",
                                                                    as.character(1:(ncol(pca_raw_results()$co)-2)),
                                                                    selected = "2")
                                   ),
                                   shiny::column(12, align = "center",
                                                 shiny::plotOutput(ns("specific_pca_plot"), height = "800px"),
                                                 shiny::column(width = 12, align = "right", class="download_plot_button",
                                                               shinyWidgets::downloadBttn(outputId = ns("download_specific_pca_plot"), "Download specific PCA plot", style = "material-flat",
                                                                                          color = "success", icon = shiny::icon("image"), size = "sm"))
                                   )
                                 )
                        ),
                        tabPanel("PCA correlation plot",
                                 shiny::plotOutput(outputId = ns("pca_plot_correlation")),
                                 shiny::column(width = 12, align = "right", class="download_plot_button",
                                               shiny::uiOutput(outputId = ns("pca_plot_correlation_dl_ui"))
                                 )
                        )
                      )
      )
    }
    
    
    
  })
  
  
  ##  ............................................................................
  ##  Quick PCA plot                                                          ####
  
  # quickpca_plot <- shiny::reactive({
  #   shiny::req(r$normalized_counts)
  #   golem::print_dev("quickpca_plot reactive")
  #   quick_pca(r$normalized_counts)
  #   recordPlot() #Thanks to https://github.com/rstudio/shiny/issues/781
  # })
  # 
  # output$pca_plot <- shiny::renderPlot({
  #   shiny::req(r$normalized_counts, quickpca_plot())
  #   golem::print_dev("quick pca plot")
  #   replayPlot(quickpca_plot())
  # })
  
  output$pca_plot <- shiny::renderPlot({
    shiny::req(r$normalized_counts)
    golem::print_dev("quick pca plot")
    quick_pca(r$normalized_counts)
  })
  
  ##  ............................................................................
  ##  PCA plot specific                                                       ####
  
  ###For the second tab. Plot two specific components.
  specific_pca_plot_plot <- shiny::reactive({
    req(pca_raw_results(), input$component_1_choice, input$component_2_choice)
    golem::print_dev("specific_pca_plot_plot reactive")
    draw_specific_pca(
      pca_raw_results(),
      component_1 = input$component_1_choice,
      component_2 = input$component_2_choice,
      legend = TRUE
    )
  })
  
  output$specific_pca_plot <- shiny::renderPlot({
    req(pca_raw_results(), input$component_1_choice, input$component_2_choice)
    golem::print_dev("specific_pca_plot")
    specific_pca_plot_plot()
  })

  
  ##  ............................................................................
  ##  PCA plot correlation                                                    ####
  
  ###Third tab, PCA correlation plot.
  pca_plot_correlation_plot <- shiny::reactive({
    req(pca_raw_results())
    golem::print_dev("pca_plot_correlation_plot reactive")
    if (is.null(r$design)) {
      golem::print_dev("no design provided")
      pca_plot_correlation(pca_raw_results())
    } else {
      golem::print_dev("A design is provided")
      pca_plot_correlation(pca_raw_results(), design = r$design)
    }
  })
  
  ###The plot
  output$pca_plot_correlation <- shiny::renderPlot({
    req(pca_raw_results())
    pca_plot_correlation_plot()
  })
  
  ###The download button rendered using renderUI.
  output$pca_plot_correlation_dl_ui <- shiny::renderUI({
    ###Check that there was no error with the plot computation. We don't want to show the button if the plot fails.
    validate(
      need(try(pca_plot_correlation_plot(),silent = TRUE), message = FALSE)
    )
    shinyWidgets::downloadBttn(
      outputId = ns("download_pca_plot_correlation"),
      "Download PCA correlation plot",
      style = "material-flat",
      color = "success",
      icon = shiny::icon("image"),
      size = "sm"
    )
  })
  
  
  ##  ............................................................................
  # Correlation heatmap                                                       ####
  
  correlation_heatmap_plot <- shiny::reactive({
    req(r$normalized_counts, input$correlation_condition_choice)
    golem::print_dev("correlation heatmap reactive")
    draw_correlation_heatmap(data = r$normalized_counts, conds = input$correlation_condition_choice)
    
  })
  
  output$correlation_heatmap <- shiny::renderPlot({
    shiny::validate(
      need(r$normalized_counts, message = "You must normalize your count data."),
      need(input$correlation_condition_choice, message = "No condition seleced")
    )
    correlation_heatmap_plot()
  })
  
#   ____________________________________________________________________________
#   Download Handler                                                        ####
  
  
  ############################
  ###DownloadHandler for plots
  output$download_quickpca <- downloadHandler(
    filename = function() {
      time = format(Sys.time(), "%Y-%m-%d_%H-%M")
      paste0(time, "_pca_DIANE.", r[["plots_params"]][["format"]])
    },
    contentType = "image",
    content = function(file) {
      download_plot_hd(plot = quick_pca(r$normalized_counts), file = file, format = r[["plots_params"]][["format"]], res = r[["plots_params"]][["res"]], width = r[["plots_params"]][["width"]], height = r[["plots_params"]][["height"]], type = "ggplot")
    }
  )
  
  output$download_specific_pca_plot <- downloadHandler(
    filename = function() {
      time = format(Sys.time(), "%Y-%m-%d_%H-%M")
      paste0(time, "_specific_PCA_DIANE.", r[["plots_params"]][["format"]])
    },
    contentType = "image",
    content = function(file) {
      download_plot_hd(specific_pca_plot_plot(), file = file, format = r[["plots_params"]][["format"]], res =  r[["plots_params"]][["res"]], width = r[["plots_params"]][["width"]], height = r[["plots_params"]][["height"]], type = "ggplot")
    }
  )
  
  output$download_pca_plot_correlation <- downloadHandler(
    filename = function() {
      time = format(Sys.time(), "%Y-%m-%d_%H-%M")
      paste0(time, "_PCA_correlation_DIANE.", r[["plots_params"]][["format"]])
    },
    contentType = "image",
    content = function(file) {
      download_plot_hd(pca_plot_correlation_plot(), file = file, format = r[["plots_params"]][["format"]], res =  r[["plots_params"]][["res"]], width = r[["plots_params"]][["width"]], height = r[["plots_params"]][["height"]], type = "lattice")
    }
  )
  
  output$expression_plot_download <- downloadHandler(
    filename = function() {
      time = format(Sys.time(), "%Y-%m-%d_%H-%M")
      paste0(time, "_expression_ploy_DIANE.", r[["plots_params"]][["format"]])
    },
    contentType = "image",
    content = function(file) {
      ###Need to check that the reactive is good.
      if(! any(class(try(expression_plot_plot(), silent = TRUE)) %in%  "try-error")){
        download_plot_hd(expression_plot_plot(), file = file, format = r[["plots_params"]][["format"]], res =  r[["plots_params"]][["res"]], width = r[["plots_params"]][["width"]], height = r[["plots_params"]][["height"]], type = "ggplot")
      } else {
        download_plot_hd(file = file, plot_error = TRUE, format = r[["plots_params"]][["format"]])
      }
    }
  )
  
  output$download_correlation_plot <- downloadHandler(
    filename = function() {
      time = format(Sys.time(), "%Y-%m-%d_%H-%M")
      paste0(time, "_correlation_DIANE.", r[["plots_params"]][["format"]])
    },
    contentType = "image",
    content = function(file) {
      download_plot_hd(correlation_heatmap_plot(), file = file, format = r[["plots_params"]][["format"]], res =  r[["plots_params"]][["res"]], width = r[["plots_params"]][["width"]], height = r[["plots_params"]][["height"]], type = "ggplot")
    }
  )
  
}
  
## To be copied in the UI
# mod_module_levels_ui("module_levels_ui_1")

## To be copied in the server
# callModule(mod_module_levels_server, "module_levels_ui_1")

