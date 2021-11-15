#' import_gene_list UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_import_gene_list_ui <- function(id) {
  ns <- NS(id)

  tagList(
  #   tags$head(tags$style(HTML('
  # #gene_list_import_id > h5 {
  #   color: rgb(255, 255, 255) !important;
  #   font-size: 18px;
  # }
  #     '
  #   ))),
    
    shiny::fluidRow(

    shinydashboardPlus::box(
      title = "Informations", id = "gene_list_import_id",
      width = 6,
      solidHeader = FALSE,
      status = "success",
      collapsible = FALSE,
      closable = FALSE,
      # shiny::h4("Gene list to import"),
      shiny::column(4,
                    shiny::uiOutput(outputId = ns("total_genes_info"), inline = TRUE)
      ),
      shiny::column(4,
                    shiny::uiOutput(outputId = ns("genes_in_list"), inline = TRUE)
      ),
      shiny::column(4,
                    shiny::uiOutput(outputId = ns("valid_genes_number"), inline = TRUE)
      ),
      shiny::hr(),
      shiny::textAreaInput(
        inputId = ns("input_gene_list"),
        label = "Gene list",
        value = NULL,
        resize = "vertical",
        height = "200px"
      ),
      shiny::actionButton(inputId = ns("import_gene_list"), label = "Import gene list")
    )
  ))
}
    
#' import_gene_list Server Functions
#'
#' @noRd 
mod_import_gene_list_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    gene_list <- shiny::reactive({
      list <- gsub("[[:space:]]", "", unlist(strsplit(input$input_gene_list, split = "\n"))) ###Remve 
      list <- list[! list %in% "None"]
      list
    })
    
    output$total_genes_info <- shiny::renderUI({
      number_of_genes <- nrow(r$raw_counts)
      # shinydashboardPlus::boxPad(color = ifelse(number_of_genes > 0, "blue", "red"),
      #                            shinydashboardPlus::descriptionBlock(
      #                              header = number_of_genes,
      #                              text = "Genes in dataset",
      #                              rightBorder = FALSE,
      #                              marginBottom = TRUE
      #                            ), style = "flex: 1; height: 100%;"
      # )
      shinydashboard::valueBox(
        value = number_of_genes,
        subtitle = "Genes in dataset",
        color = ifelse(number_of_genes > 0, "blue", "red"),
        width = 12
      )
    })
    
    output$genes_in_list <- shiny::renderUI({
      number_of_genes <- length(gene_list)
      shinydashboardPlus::boxPad(color = ifelse(number_of_genes > 0, "blue", "red"),
                                 shinydashboardPlus::descriptionBlock(
                                   header = number_of_genes,
                                   text = "Genes in gene list",
                                   rightBorder = FALSE,
                                   marginBottom = TRUE
                                 ), style = "flex: 1; height: 100%"
      )
    })
    
    
    
    
    
    
    
    output$valid_genes_number <- shiny::renderUI({
      # number_of_genes <- rownames(r$raw_counts)
      # number_of_genes_in_list <- 
      number_genes_in_table <- sum(gene_list() %in% rownames(r$raw_counts))
      shinydashboardPlus::boxPad(color = ifelse(number_genes_in_table == length(gene_list()), "green", "orange"),
                                 shinydashboardPlus::descriptionBlock(
                                   header = number_genes_in_table,
                                   text = "Common genes",
                                   rightBorder = FALSE,
                                   marginBottom = TRUE
                                 ), style = "flex: 1;  height: 100%"
      )
    })
    
 
  })
}
    
## To be copied in the UI
# mod_import_gene_list_ui("import_gene_list_ui_1")
    
## To be copied in the server
# mod_import_gene_list_server("import_gene_list_ui_1")
