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
    font-size: 20px;
   }
   
   .gene_information_squaure .description-block {
    text-align: left;
    margin: 0 0 0 0;
   }
   
      .gene_information_squaure .description-percentage {
    color: rgb(255, 255, 255) !important;
    font-size: 30px;
    font-weight: 700;
    margin: 0 0 10px 0;
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
          width = 3,
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
          title = "Stored gene list",
          width = 3,
          solidHeader = FALSE,
          status = "success",
          collapsible = FALSE,
          closable = FALSE,
          shiny::uiOutput(ns("stored_gene_list_ui"))
        ),
        shinydashboardPlus::box(
          title = "Custom gene list preview",
          width = 6,
          solidHeader = FALSE,
          status = "success",
          collapsible = FALSE,
          closable = FALSE,
          # shiny::plotOutput(ns("custom_gene_list_heatmap_preview"))
          shiny::plotOutput(ns("custom_gene_list_heatmap_preview"))
          ###TODO : reactive value for heatmap height
        )
      )
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
                          number = total_number_of_genes,
                          header = "Genes",
                          text = "in dataset", 
                          rightBorder = FALSE,
                          marginBottom = TRUE
                        ),
                        style = "border-radius: 12px;"
                      )
        ),
        shiny::column(3,
                      shinydashboardPlus::boxPad(
                        color = ifelse(number_of_genes > 0, "teal", "gray"),
                        shinydashboardPlus::descriptionBlock(
                          number = number_of_genes,
                          header = "Genes",
                          text = "in gene list", 
                          rightBorder = FALSE,
                          marginBottom = TRUE
                        ),
                        style = "border-radius: 12px;"
                      )
        ),
        shiny::column(3,
                      shinydashboardPlus::boxPad(
                        color = ifelse(number_of_genes == 0, "gray", ifelse(number_genes_in_table == number_of_genes, "green", ifelse(number_genes_in_table == 0, "red", "orange"))),
                        shinydashboardPlus::descriptionBlock(
                          number = number_genes_in_table,
                          header = "Genes",
                          text = "shared in both", 
                          rightBorder = FALSE,
                          marginBottom = TRUE
                        ),
                        style = "border-radius: 12px;"
                      )
                      
                      
        ),
        if(number_genes_in_table != number_of_genes && number_of_genes > 0){
          shiny::tagList(
            shiny::column(6,
                          # br(),
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
    })

      ##### --------------------------------------
      
    output$import_gene_list_button_ui <- shiny::renderUI({
      req(r$raw_counts)
      if(sum(gene_list() %in% rownames(r$raw_counts)) > 1){
        shiny::tagList(
          shiny::textInput(
            inputId = ns("gene_list_name"),
            label = "List name",
            value = "My gene list 1", width = "40%",
          ),
          shiny::actionButton(inputId = ns("import_gene_list"), label = "Import gene list"),
        )
      } else {
        shinydashboardPlus::descriptionBlock(
          number = "You must provide at least two valid genes in your gene list",
          numberColor = "orange",
          rightBorder = FALSE
        )
      }
    })
    
    ###TODO
    # Check that the name pattern cannot match a pattern from DEA/clustering
    #NOTE - or maybe just separate both custom and not custom gene list (using sublist name for exemple...)
    # Check that the name pattern does not exist
    # Check that there is more than xx genes in the list
    # Empty the UI when gene list is uploaded
    # Check that name is not too long.
    shiny::observeEvent(input$import_gene_list, {
      if (stringr::str_detect(pattern = paste0(paste0("^",unique(r$conditions), " "), collapse = "|"), string =  input$gene_list_name)) { ###Search for a string that begins with the name of a condition, of by the name of a condition with a parenthesis.
        shinyalert::shinyalert("This name may be the result of Differential expression analysis. You cannot choose it.",
                               type = "error")
      } else if (input$gene_list_name %in% names(r$custom_gene_list)) { #Check that the list does not exist.
        shinyalert::shinyalert("This gene list already exist",
                               type = "error")
      } else if (nchar(input$gene_list_name) > 50 | nchar(input$gene_list_name) < 2){
        shinyalert::shinyalert("You cannot use more than 50 characters/less than 2 characters in your gene list name.",
                               type = "error")
      } else if (stringr::str_detect(pattern = "[^a-zA-Z0-9 ]", string = input$gene_list_name)) 
        shinyalert::shinyalert("You can only use characters, numbers and spaces in you list name.",
                               type = "error")
      else {
        r$custom_gene_list[[input$gene_list_name]] <- gene_list()[gene_list() %in% rownames(r$raw_counts)]
        shiny::updateTextAreaInput(inputId = "input_gene_list", value = "")
      }
    })
    
    output$stored_gene_list_ui <- shiny::renderUI({
      # req(length(names(r$custom_gene_list)) > 0)
      if(length(names(r$custom_gene_list)) > 0){
        shinyWidgets::radioGroupButtons(
          inputId = ns("stored_gene_list"),
          label = "Stored gene list",
          choices = names(r$custom_gene_list),
          justified = TRUE,
          direction = "vertical",
          checkIcon = list(yes = shiny::icon("ok",
                                             lib = "glyphicon"))
        )
      } else {
        shinydashboardPlus::descriptionBlock(
          number = "You have no custom gene list yet. You can create one using the gene list tab on the left.",
          numberColor = "orange",
          rightBorder = FALSE
        )
      }
    })
    
    output$custom_gene_list_heatmap_preview <- shiny::renderPlot({
      shiny::req(r$raw_counts)
      shiny::req(input$stored_gene_list)
      
      d <- r$raw_counts[rowSums(r$raw_counts) > 0 & rownames(r$raw_counts) %in% r$custom_gene_list[[input$stored_gene_list]],]
      
      draw_heatmap(d, title = paste0(input$stored_gene_list, " preview"), subset = rownames(d))
    })
    


    
 
  })
}
    
## To be copied in the UI
# mod_import_gene_list_ui("import_gene_list_ui_1")
    
## To be copied in the server
# mod_import_gene_list_server("import_gene_list_ui_1")
