#' context UI Function
#'
#' @description A shiny Module for the application context.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList 
mod_context_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::h1("Dashboard for the Inference and Analysis of Networks from Expression data" ),
    shiny::fluidRow(
      shiny::column(6,
        img(src = "www/DIANE_workflow.png", width = "100%")
      ),
      shiny::column(6,
        shiny::includeMarkdown(system.file("extdata", "welcome.md", package = "DIANE")),
      )
    ),
    shiny::hr(),
    shiny::fluidRow(
      shiny::column(8,
        shiny::includeMarkdown(system.file("extdata", "details.md", package = "DIANE")),
      ),
      shiny::column(4,
        shiny::br(),
        shiny::br(),
        shiny::br(),
        # shiny::h2("Partner institutions"),
        img(src = "www/partners_vertical.png", width = "100%"),
        # shiny::h2("Non-exhaustive ecosystem of featured packagess"),
        img(src = "www/logos_vertical.png", width = "100%")
      )
    )
  )
}
    
#' context Server Function
#'
#' @noRd
mod_context_server <- function(input, output, session, r){
  ns <- session$ns
  
  # browser()
  
  # observe({
  #   browser()
  #   query <- parseQueryString(session$clientData$url_search)
  #   r$included_genus <- query$organism
  #    print(query)
  # })
  
  # Get query parameters for further use.
  # query <-session$clientData$url_search
  
  
}
    
## To be copied in the UI
# mod_context_ui("context_ui_1")
    
## To be copied in the server
# callModule(mod_context_server, "context_ui_1")
 
