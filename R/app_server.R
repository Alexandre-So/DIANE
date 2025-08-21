#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 
options(shiny.maxRequestSize=70*1024^2)
app_server <- function(input, output, session) {
  
#   ____________________________________________________________________________
#   reactive values                                                         ####

  r <- shiny::reactiveValues(
    raw_counts = NULL,
    normalized_counts = NULL,
    normalized_counts_pre_filter = NULL,
    tcc = NULL,
    conditions = NULL,
    design = NULL,
    DEGs = list(),
    DEGs_infos = list(), ###Used to store some informations about DEG list (conditions, cutoff, etc.)
    top_tags = list(),
    clusterings = list(),
    current_comparison = NULL,
    current_network = NULL,
    regulators = NULL,
    use_demo = NULL,
    networks = list(),
    splicing_aware = NULL,
    gene_info = NULL,
    organism = NULL,
    integrated_dataset =  NULL,
    custom_go = NULL,
    session_id = as.character(floor(runif(1)*1e20)),
    seed = golem::get_golem_options("seed"),
    output_field_separator = ",",
    plots_params = list(
      format = "pdf",
      res = 300,
      width = 20,
      height = 14
    ),
    included_genus = NULL, ### Allow to filter genus
    preselected_organism = NULL, ### Allow to pre-select a specific organism
    preselected_dataset = NULL ### Allow to pre-select a specific dataset.
  )
  
  shiny::observe({
    query <- shiny::parseQueryString(session$clientData$url_search)
    r$included_genus <- query$organism
    r$preselected_organism <- query$organism_selected
    r$preselected_dataset <- query$data_selected
    print(query)
  })
  
  
  
  #   ____________________________________________________________________________
  #   logs                                                                    ####
  
  LOG_FILE = "./logs/loggit.log"
  SESSION_ID_FILE = "./logs/next_id.txt"
  
  if(golem::get_golem_options("server_version")){
    if (!file.exists(SESSION_ID_FILE)){
      file.create(SESSION_ID_FILE)
      write(1, SESSION_ID_FILE )
    }
  
  
    session_id <- readLines(SESSION_ID_FILE)
    close( file( SESSION_ID_FILE, open="w" ) )
    write(as.numeric(session_id) + 1, SESSION_ID_FILE )
    
    loggit::set_logfile(LOG_FILE)
    loggit::set_timestamp_format("%Y-%m-%d %H:%M:%S")
    
    
    loggit::loggit(custom_log_lvl = TRUE,
                   log_lvl = session_id,
                   log_msg = "connection")
    
    r$session_id <- session_id
  }
  
#   ____________________________________________________________________________
#   Server modules                                                          ####

  shiny::callModule(mod_context_server, "context_ui_1", r)
  shiny::callModule(mod_import_data_server, "import_data_ui_1", r)
  shiny::callModule(mod_normalisation_server, "normalisation_ui_1", r)
  
  shiny::callModule(mod_module_levels_server, "module_levels_ui_1", r)
  shiny::callModule(mod_differential_expression_analysis_server, 
                    "differential_expression_analysis_ui_1", r)
  
  # clustering modules
  shiny::callModule(mod_clustering_server, "clustering_ui_1", r)
  shiny::callModule(mod_cluster_exploration_server, 
                    "cluster_exploration_ui_1", r)
  
  shiny::callModule(mod_network_inference_server, "network_inference_ui_1", r)
  shiny::callModule(mod_network_analysis_server, "network_analysis_ui_1", r)

  shiny::callModule(mod_datasets_server, "datasets_ui_1")
  shiny::callModule(mod_legal_mentions_server, "legal_mentions_ui_1")
  
  mod_versions_server("versions_ui_1")
  
  output$general_debug_button <- shiny::renderUI({
    if(golem::app_dev()){
      shiny::column(12,
      shiny::actionButton("debug", "debug"),
      shiny::actionButton("addSep", "addSep")
      )
    } else {
      NULL
    }
  })
  
  observeEvent(input$plot_params, {
    golem::print_dev("plot_params clic")
    showModal(
      shiny::fluidRow(
        modalDialog(
          tags$h1("Plot parameters"),
          shiny::helpText(
            "Here you can adjust the parameters of the plots downloaded using the \"download button\" available next to some plots in DIANE. This allow you to download high resolution / publication quality versions of the displayed plots in various formats."
          ),
          shiny::HTML(
            paste0("<span style='color: #737373'>",shiny::icon("circle-info"), "</span> <span style='color: #737373'>Note that the res argument only affect png and tiff format.</span>")
          ),
          shiny::hr(),
          shiny::column(6,
                        shiny::numericInput(inputId = "plot_width", label = "width", value = r[["plots_params"]][["width"]], , min = 3, max = 50)
          ),
          shiny::column(6,
                        shiny::numericInput(inputId = "plot_height", label = "height", value = r[["plots_params"]][["height"]], min = 3, max = 50)
          ),
          shiny::column(6,
                        shiny::numericInput(inputId = "plot_res", label = "res", value = r[["plots_params"]][["res"]], min = 72, max = 600)
          ),
          shiny::column(6,
                        shiny::selectInput("plot_format", label = "Plot format", choices = c("png", "pdf", "tiff", "svg"), selected = r[["plots_params"]][["format"]], multiple = F)
          )
        ))
    )
  })
  
  ###Update images download properties. the related input are not available in modules.
  shiny::observeEvent(input$plot_width, {
    golem::print_dev('width update')
    r[["plots_params"]][["width"]] <- input$plot_width
  })
  shiny::observeEvent(input$plot_height, {
    golem::print_dev('height update')
    r[["plots_params"]][["height"]] <- input$plot_height
  })
  shiny::observeEvent(input$plot_format, {
    golem::print_dev('format update')
    r[["plots_params"]][["format"]] <- input$plot_format
  })
  shiny::observeEvent(input$plot_res, {
    golem::print_dev('res update')
    r[["plots_params"]][["res"]] <- input$plot_res
  })
  
  
  # Global options 
  shiny::observeEvent(input$global_options, {
    golem::print_dev("Global options clic")
    showModal(
      shiny::fluidRow(
        modalDialog(
          tags$h1("Global apps parameters"),
          shiny::helpText("This will set some parameters that will be used in some places in the app."),
          # shiny::HTML(
          #   paste0("<span style='color: #737373'>",shiny::icon("circle-info"), "</span> <span style='color: #737373'>Note that the res argument only affect png and tiff format.</span>")
          # ),
          shiny::hr(),
          shiny::column(6,
            shinyWidgets::awesomeRadio(
              inputId ='output_file_field_separator',
              label = 'Output file field separator : ',
              c(
                Comma = ',',
                Semicolon = ';',
                Tab = '\t'
              ),
              selected = r[["output_field_separator"]],
              inline = TRUE,
              status = "success"
            ),
          ),
        ))
    )
  })
  
  shiny::observeEvent(input$output_file_field_separator, {
    golem::print_dev('output_field_separator update')
    r[["output_field_separator"]] <- input$output_file_field_separator
    golem::print_dev(paste0("OFS set to \'", r[["output_field_separator"]], "\'"))
  })
  
  observeEvent(input$debug, {
    browser()
  })
  
  observeEvent(input$addSep, {
    print("---------------------------------------------")
  })
  
}
