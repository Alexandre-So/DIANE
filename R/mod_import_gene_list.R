#' import_gene_list UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_import_gene_list_ui <- function(id){
  ns <- NS(id)
  tagList(
    ###boxPad renders a solid colored pad : the description texts need to be
    ###turned white and resized to stay readable on it.
    tags$head(tags$style(HTML('
   .gene_information_square .description-header {
    color: rgb(255, 255, 255) !important;
    font-size: 20px;
   }

   .gene_information_square .description-block {
    text-align: left;
    margin: 0 0 0 0;
   }

   .gene_information_square .description-percentage {
    color: rgb(255, 255, 255) !important;
    font-size: 30px;
    font-weight: 700;
    margin: 0 0 10px 0;
   }

   .gene_information_square .description-text {
    color: rgb(255, 255, 255) !important;
   }
      '
    ))),

    shiny::h1("Import a custom gene list"),
    shiny::hr(),

    shinybusy::add_busy_spinner(
      spin = "self-building-square",
      position = 'top-left',
      margins = c(70, 1200)
    ),

    shiny::uiOutput(ns("prerequisite")),

    shiny::fluidRow(
      shinydashboardPlus::box(
        title = "New gene list",
        solidHeader = FALSE,
        status = "success",
        collapsible = TRUE,
        closable = FALSE,
        width = 4,
        
        

        shiny::HTML(paste0(
          '<h4 style="display: inline-block;">Gene identifiers</h4>', ### Display: inline-block; allow the question mark to be on the same line.
          shinyWidgets::dropdownButton(
            size = 'xs',
            shiny::includeMarkdown(system.file("extdata", "gene_list_import.md",
                                               package = "DIANE")),
            circle = TRUE,
            status = "success",
            icon = shiny::icon("question"),
            width = "600px",
            inline = TRUE,
            tooltip = shinyWidgets::tooltipOptions(title = "More details")
          )
        )),

        shiny::textAreaInput(
          inputId = ns("pasted_genes"),
          label = "Paste them here",
          height = "220px",
          resize = "vertical"
        ),

        shiny::fileInput(
          inputId = ns("gene_list_file"),
          label = "Or upload a file",
          accept = c(".txt", ".csv", ".tsv")
        ),
        
        shiny::fluidRow(shiny::column(12, shiny::uiOutput(ns("counters")))),

        shiny::hr(),

        shiny::textInput(
          inputId = ns("gene_list_name"),
          label = "Gene list name",
          value = ""
        ),

        shinyWidgets::actionBttn(
          inputId = ns("import_btn"),
          label = "Import gene list",
          style = "material-flat",
          color = "success"
        )
      ),

      shinydashboardPlus::box(
        title = "Stored gene lists",
        solidHeader = FALSE,
        status = "success",
        collapsible = TRUE,
        closable = FALSE,
        width = 4,
        shiny::uiOutput(ns("stored_lists"))
      ),

      shinydashboardPlus::box(
        title = "Preview",
        solidHeader = FALSE,
        status = "success",
        collapsible = TRUE,
        closable = FALSE,
        width = 4,
        shiny::plotOutput(ns("preview_heatmap"), height = "600px")
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

    ###Maximum list size.
    max_genes <- 5000

    ###Datapath of the file already turned into a gene list. Without it, an
    ###imported file keeps feeding the counters and a second click duplicates it.
    consumed_file <- shiny::reactiveVal(NULL)

    ###Genes are validated against the normalized matrix, not the raw counts :
    ###it is the matrix every consumer indexes, so a gene dropped by the low
    ###count filter would produce NA rows in clustering.
    known_genes <- shiny::reactive({
      shiny::req(r$normalized_counts)
      rownames(r$normalized_counts)
    })

    file_lines <- shiny::reactive({
      f <- input$gene_list_file
      if (is.null(f)) return(character(0))
      if (identical(f$datapath, consumed_file())) return(character(0))

      tryCatch(
        readLines(f$datapath, warn = FALSE),
        error = function(e) {
          shinyalert::shinyalert("This file could not be read.",
                                 "Please provide a text file with one gene identifier per line.",
                                 type = "error")
          character(0)
        }
      )
    })

    ###Union of both inputs, no precedence : parse_gene_list deduplicates.
    submitted_genes <- shiny::reactive({
      parse_gene_list(c(input$pasted_genes, file_lines()))
    })

    valid_genes <- shiny::reactive({
      intersect(submitted_genes(), known_genes())
    })

    unknown_genes <- shiny::reactive({
      setdiff(submitted_genes(), known_genes())
    })

    #   ____________________________________________________________________________
    #   Summaries                                                               ####

    output$prerequisite <- shiny::renderUI({
      if (is.null(r$normalized_counts)) {
        shinydashboardPlus::descriptionBlock(
          number = "Please normalize and filter your expression data in the Normalisation tab first",
          numberColor = "orange",
          rightBorder = FALSE
        )
      }
    })

    output$counters <- shiny::renderUI({
      shiny::req(r$normalized_counts)

      n_dataset <- length(known_genes())
      n_submitted <- length(submitted_genes())
      n_found <- length(valid_genes())

      found_color <- if (n_submitted == 0) "gray"
        else if (n_found == n_submitted) "green"
        else if (n_found == 0) "red"
        else "orange"

      shiny::tagList(shiny::tags$div(
        class = "gene_information_square",

        shiny::column(4,
          shinydashboardPlus::boxPad(
            color = "aqua",
            shinydashboardPlus::descriptionBlock(
              number = n_dataset,
              text = "in dataset",
              rightBorder = FALSE,
              marginBottom = TRUE
            ),
            style = "border-radius: 12px;"
          )
        ),

        shiny::column(4,
          shinydashboardPlus::boxPad(
            color = if (n_submitted > 0) "teal" else "gray",
            shinydashboardPlus::descriptionBlock(
              number = n_submitted,
              text = "in gene list",
              rightBorder = FALSE,
              marginBottom = TRUE
            ),
            style = "border-radius: 12px;"
          )
        ),

        shiny::column(4,
          shinydashboardPlus::boxPad(
            color = found_color,
            shinydashboardPlus::descriptionBlock(
              number = n_found,
              text = "in both",
              rightBorder = FALSE,
              marginBottom = TRUE
            ),
            style = "border-radius: 12px;"
          )
        ),

        if (length(unknown_genes()) > 0) {
          shiny::column(6,
            shiny::HTML("<b>Warning</b> : some provided genes are absent from the
                        expression matrix and will not be used. For example :<br>"),
            shiny::HTML(paste0(head(unknown_genes()), "<br>"))
          )
        }
      ))
    })

    #   ____________________________________________________________________________
    #   Import                                                                  ####

    ###r$top_tags is only ever added to, never removed. The duplicate check below
    ###refuses every taken name, DEA ones included, so an imported list can never
    ###inherit a stale top_tags : is.null(r$top_tags[[name]]) stays the reliable
    ###"no differential statistics" signal the Venn diagram relies on.
    shiny::observeEvent((input$import_btn), {
      shiny::req(r$normalized_counts)

      name <- trimws(input$gene_list_name)
      msg <- check_gene_list_name(name, existing_names = names(r$gene_lists))

      if (!isTRUE(msg))
        shinyalert::shinyalert(msg, type = "error")
      shiny::req(isTRUE(msg))

      genes <- valid_genes()

      if (length(genes) < 2)
        shinyalert::shinyalert(
          "A gene list needs at least two genes present in the expression matrix.",
          type = "error"
        )
      shiny::req(length(genes) >= 2)

      if (length(genes) > max_genes)
        shinyalert::shinyalert(
          paste0("A gene list cannot hold more than ", max_genes, " genes."),
          type = "error"
        )
      shiny::req(length(genes) <= max_genes)

      r$gene_lists[[name]] <- genes

      ###Written as a whole, never field by field, so that reusing a name cannot
      ###inherit metadata from a previous list.
      r$gene_lists_infos[[name]] <- list(
        origin = "custom",
        n_submitted = length(submitted_genes()),
        n_kept = length(genes)
      )

      if (golem::get_golem_options("server_version"))
        loggit::loggit(
          custom_log_lvl = TRUE,
          log_lvl = r$session_id,
          log_msg = "custom gene list import"
        )

      shiny::updateTextAreaInput(session, "pasted_genes", value = "")
      shiny::updateTextInput(session, "gene_list_name", value = "")
      consumed_file(input$gene_list_file$datapath)
    })

    #   ____________________________________________________________________________
    #   Stored lists and preview                                                ####

    output$stored_lists <- shiny::renderUI({
      if (length(r$gene_lists) == 0) {
        return(
          shinydashboardPlus::descriptionBlock(
            number = "No gene list yet. Import one, or run a differential expression analysis.",
            numberColor = "orange",
            rightBorder = FALSE
          )
        )
      }

      ###Lists created before this module have no origin recorded, they are DEA ones.
      origins <- vapply(names(r$gene_lists), function(n) {
        origin <- r$gene_lists_infos[[n]]$origin
        if (is.null(origin)) "DEA" else origin
      }, character(1))

      shinyWidgets::radioGroupButtons(
        inputId = ns("selected_list"),
        label = NULL,
        choiceValues = names(r$gene_lists),
        choiceNames = paste0(names(r$gene_lists), " (", lengths(r$gene_lists),
                             " genes, ", origins, ")"),
        justified = TRUE,
        direction = "vertical",
        checkIcon = list(yes = shiny::icon("ok",
                                           lib = "glyphicon"))
      )
    })

    output$preview_heatmap <- shiny::renderPlot({
      shiny::req(r$normalized_counts, input$selected_list)
      shiny::req(r$gene_lists[[input$selected_list]])

      ###Capped : draw_heatmap builds a regex out of the subset, slow on thousands.
      genes <- head(r$gene_lists[[input$selected_list]], 200)

      draw_heatmap(
        data = r$normalized_counts,
        subset = genes,
        log = TRUE,
        profiles = TRUE,
        title = paste0(input$selected_list, " - first ", length(genes), " genes")
      )
    })

  })
}

## To be copied in the UI
# mod_import_gene_list_ui("import_gene_list_ui_1")

## To be copied in the server
# mod_import_gene_list_server("import_gene_list_ui_1", r)
