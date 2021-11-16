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
    tags$head(tags$style(HTML('
   .gene_information_squaure .description-header {
    color: rgb(255, 255, 255) !important;
    font-size: 24px;
   }
  
     .gene_information_squaure .description-text {
    color: rgb(255, 255, 255) !important;
  }
      '
    ))),
    
    ######################### Title and text
    
    shiny::h1("Import custom gene list"),
    shiny::hr(),
    
    shiny::fluidRow(
      
      
      shiny::column(6,
                    shiny::uiOutput(ns("gene_information"))
      )),
    br(),
    hr(),
      shiny::fluidRow(
        shinydashboardPlus::box(
          title = "Informations", id = "gene_list_import_id",
          width = 6,
          solidHeader = FALSE,
          status = "success",
          collapsible = FALSE,
          closable = FALSE,
          # shiny::h4("Gene list to import"),
          shiny::textAreaInput(
            inputId = ns("input_gene_list"),
            label = "Gene list",
            value = NULL,
            resize = "vertical",
            height = "200px"
          ),
          shiny::uiOutput(outputId = ns("import_gene_list_button_ui"))
        ),
        shinydashboardPlus::box(
          title = "Stored gene list", id = "gene_list_import_id",
          width = 6,
          solidHeader = FALSE,
          status = "success",
          collapsible = FALSE,
          closable = FALSE,
          shiny::uiOutput(ns("stored_gene_list_ui"))
      ))
  )
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
      unique(list)
    })
    
    ###FIXME : one render UI for all of this is enough...
    
    output$gene_information <- shiny::renderUI({
      
      print("iris est gentille !")
      
      if(is.null(r$raw_counts)) {
        shinydashboardPlus::descriptionBlock(
          number = "Please first import some count data in the previous tab.",
          numberColor = "orange",
          rightBorder = FALSE
        )
      } else {
      total_number_of_genes <- nrow(r$raw_counts)
      number_of_genes <- length(gene_list())
      number_genes_in_table <- sum(gene_list() %in% rownames(r$raw_counts))
      
      shiny::tagList(
      shiny::tags$div(
        class = "gene_information_squaure",
        shiny::column(3,
                      shinydashboardPlus::boxPad(
                        color = ifelse(total_number_of_genes > 0, "aqua", "red"),
                        shinydashboardPlus::descriptionBlock(
                          header = total_number_of_genes,
                          text = "Genes in dataset",
                          rightBorder = FALSE,
                          marginBottom = TRUE
                        ),
                        style = "flex: 1; height: 100px;"
                      )
        ),
        shiny::column(3,
                      shinydashboardPlus::boxPad(
                        color = ifelse(number_of_genes > 0, "teal", "gray"),
                        shinydashboardPlus::descriptionBlock(
                          header = number_of_genes,
                          text = "Genes in gene list",
                          rightBorder = FALSE,
                          marginBottom = TRUE
                        ),
                        style = "flex: 1; height: 100px;;"
                      )
        ),
        shiny::column(3,
                      shinydashboardPlus::boxPad(
                        color = ifelse(number_of_genes == 0, "gray", ifelse(number_genes_in_table == number_of_genes, "green", ifelse(number_genes_in_table == 0, "red", "orange"))),
                        shinydashboardPlus::descriptionBlock(
                          header = number_genes_in_table,
                          text = "Common genes",
                          rightBorder = FALSE,
                          marginBottom = TRUE
                        ),
                        style = "flex: 1;  height: 100px;"
                      )
                      
                      
        ),
        if(number_genes_in_table != number_of_genes && number_of_genes > 0){
          shiny::tagList(
            shiny::column(12,
                          br(),
            shiny::HTML(
              "<b>Warning</b> : some provided genes are absent from the count matrix and will not be used. For exemple :<br>"
            ),
            shiny::HTML(as.character(paste0(
              head(gene_list()[! gene_list() %in% rownames(r$raw_counts)]), "<br>"
            ))
            ))
          )
        }

      ))
      }
      # shiny::column(12,
      #               hr(),
      #               shiny::uiOutput(ns("invalid_genes_list"))),
      # shiny::hr(), 
      # 
    })

      ##### --------------------------------------
      
    output$import_gene_list_button_ui <- shiny::renderUI({
      req(r$raw_counts)
      if(sum(gene_list() %in% rownames(r$raw_counts)) > 0){
        shiny::tagList(
          shiny::textInput(
            inputId = ns("gene_list_name"),
            label = "List name",
            value = "My gene list 1"
          ),
          shiny::actionButton(inputId = ns("import_gene_list"), label = "Import gene list")
        )
      } else {
        shinydashboardPlus::descriptionBlock(
          number = "You must provide at least one valid gene in your gene list",
          numberColor = "orange",
          rightBorder = FALSE
        )
      }
    })
    
    shiny::observeEvent(input$import_gene_list, {
      r$custom_gene_list[input$gene_list_name] <- gene_list()[gene_list() %in% rownames(r$raw_counts)]
    })
    
    output$stored_gene_list_ui <- shiny::renderUI({
      req(length(names(r$custom_gene_list)) > 0)
      shinyWidgets::radioGroupButtons(
        inputId = ns("stored_gene_list"),
        label = "Stored gene list",
        choices = names(r$custom_gene_list),
        justified = TRUE,
        direction = "vertical",
        checkIcon = list(yes = shiny::icon("ok",
                                           lib = "glyphicon"))
      )
    })
    
    # output$invalid_genes_list <- shiny::renderUI({
    #   if(!all(gene_list() %in% rownames(r$raw_counts))){
    #     shiny::tagList(
    #       shiny::HTML(
    #         "<b>Warning</b> : some provided genes are absent from the count matrix and will not be used. For exemple :<br>"
    #       ),
    #       shiny::HTML(as.character(paste0(
    #         head(gene_list()[! gene_list() %in% rownames(r$raw_counts)]), "<br>"
    #       ))
    #       ),
    #       hr()
    #     )
    #   } else {
    #     NULL
    #   }
    # })

    
 
  })
}
    
## To be copied in the UI
# mod_import_gene_list_ui("import_gene_list_ui_1")
    
## To be copied in the server
# mod_import_gene_list_server("import_gene_list_ui_1")
