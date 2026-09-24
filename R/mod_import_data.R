


#' Organisms offered in the import module
#'
#' Called from the UI, so the list is already in the served HTML.
#'
#' @param included_genus Genus to restrict the list to, from the URL query.
#' @noRd
organism_choices <- function(included_genus = NULL) {
  ## TODO : check if these packages are always loaded. Could reduce RAM usage.
  ## TODO : check for arabidopsis.
  choices <- c("Arabidopsis thaliana")
  if (requireNamespace("org.Mm.eg.db", quietly = TRUE))
    choices <- c(choices, "Mus musculus")

  if (requireNamespace("org.Hs.eg.db", quietly = TRUE))
    choices <- c(choices, "Homo sapiens")

  if (requireNamespace("org.Ce.eg.db", quietly = TRUE))
    choices <- c(choices, "Caenorhabditis elegans")

  if (requireNamespace("org.Dm.eg.db", quietly = TRUE))
    choices <- c(choices, "Drosophilia melanogaster")

  if (requireNamespace("org.EcK12.eg.db", quietly = TRUE))
    choices <- c(choices, "Escherichia coli")


  choices <- c("Other", choices)

  # Give name (genus) to pre-integrated data. We call them 'model".
  names(choices) <- c("Other", rep("Model", length(choices)-1))

  # import custom data
  custom_orgs <- names(DIANE::organisms_index)
  genus_custom_orgs <- c()
  # Give a name to custom orgs. Either genus, or just the name of the organism.
  for(i in custom_orgs){
    if(!is.null(DIANE::organisms_index[[i]][["genus"]])){
      genus_custom_orgs <- c(genus_custom_orgs, DIANE::organisms_index[[i]][["genus"]])
    } else {
      genus_custom_orgs <- c(genus_custom_orgs, i)
    }
  }
  names(custom_orgs) <- genus_custom_orgs

  choices <- c(choices, custom_orgs)

  # Chose organism based on url query and integrated data
  if(!is.null(included_genus)){
    if(all(included_genus %in% names(choices))){
      choices <- choices[names(choices) %in% included_genus]
    }
  }

  unname(choices)
}


#' import_data UI Function
#'
#' @description A shiny Module to import expression data.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param included_genus Genus to restrict the organism list to, from the URL.
#' @param preselected_organism Organism selected on startup, from the URL.
#' @importFrom shinydashboard valueBoxOutput
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_import_data_ui <- function(id,
                               included_genus = NULL,
                               preselected_organism = NULL) {
  ns <- NS(id)
  tagList(
    shinybusy::add_busy_spinner(
      spin = "self-building-square",
      position = 'top-left',
      margins = c(70, 1100)
    ),
    
    ######################### Title and text
    
    shiny::h1("Upload expression data and experimental design"),
    shiny::hr(),
    
    
    
    #   ____________________________________________________________________________
    #   File upload                                                             ####
    shiny::fluidRow(
      shinydashboardPlus::box(
        title = "Expression file upload",
        width = 4,
        solidHeader = FALSE,
        status = "success",
        collapsible = TRUE,
        closable = FALSE,
        
        # Organism first : it constrains which data sources are available.
        # Static, so input$org_select reaches the server on connection.
        shiny::selectInput(
          ns("org_select"),
          label = shiny::HTML(paste0(
            'Your organism ',
            shinyWidgets::dropdownButton(
              right = FALSE,
              size = 'xs',
              label = "Organism description",
              shiny::htmlOutput(ns('organism_description')),
              circle = TRUE,
              status = "success",
              inline = TRUE,
              icon = shiny::icon("question"),
              width = "550px",
              tooltip = shinyWidgets::tooltipOptions(title = "Informations about selected organism")
            )
          )),
          choices = organism_choices(included_genus),
          selected = if (!is.null(preselected_organism) &&
                         preselected_organism %in% names(DIANE::organisms_index))
            preselected_organism else "Arabidopsis thaliana"
        ),

        # BS3 has no btn-outline-* : the selected option is filled green, the
        # others stay white, so a single green carries the current choice.
        shiny::tags$style(shiny::HTML(sprintf("
          #%1$s .btn.radiobtn {
            background-color: #ffffff;
            background-image: none;
            color: #2f6f46;
            border: 1px solid #5FBF64;
            box-shadow: none;
            text-shadow: none;
          }
          #%1$s .btn.radiobtn:hover {
            background-color: #eef7ef;
          }
          #%1$s .btn.radiobtn.active,
          #%1$s .btn.radiobtn.active:hover {
            background-color: #5FBF64;
            color: #ffffff;
            border-color: #5FBF64;
          }
          #%1$s .btn.radiobtn.disabled,
          #%1$s .btn.radiobtn.disabled:hover {
            background-color: #f5f5f5;
            background-image: none;
            color: #adadad;
            border-color: #dddddd;
            opacity: 1;
          }
        ", ns("data_source")))),

        # Data source, as an exclusive choice rather than a toggle
        shinyWidgets::radioGroupButtons(
          ns("data_source"),
          label = "Where does the expression data come from ?",
          choiceNames  = list(htmltools::tagAppendChild(
            shiny::textOutput(ns("integrated_label"), inline = TRUE),
            "Integrated dataset"),
            "My own files"),
          choiceValues = c("integrated", "upload"),
          selected = "integrated",
          justified = TRUE,
          status = "success"
        ),
        # shiny::uiOutput(ns("data_source_hint")),

        shiny::uiOutput(ns("dataset_selection_ui")),

        # shiny::tabsetPanel(id="dataset-description-tabsetPanel"),
        shiny::hr(style = "margin-top: 0px; margin-bottom: 10px;"),

        # UI for integrated dataset
        # shiny::tabsetPanel(id="dataset-description-tabsetPanel",
        # shinydashboard::tabBox(id="dataset-description-tabsetPanel",  width = 12,
          # shiny::tabPanel(title = "Dataset description",
        shiny::htmlOutput(ns('dataset_description')),
        # ),
          # shiny::tabPanel("Organism description",
                      # "iris"
                          # shiny::htmlOutput(ns('organism_description'))
          # )
         # ),

        
        
        # Expected ID format : only actionable once the organism is known,
        # and only in upload mode
        ### Now rendered inside the expression upload_field(), where it is used.
        # shiny::uiOutput(ns("gene_ids")),

        shiny::uiOutput(ns("count_import_ui")),
        shiny::uiOutput(ns("custom_organism_ui")),
        
        # shiny::htmlOutput(ns("dataset_description")),
        
        shiny::fluidRow(
          shinydashboard::valueBoxOutput(ns("data_dim")),
          shinydashboard::valueBoxOutput(ns("conditions")),
          shinydashboard::valueBoxOutput(ns("samples"))
        ),
        shiny::fluidRow(
          col_4(shiny::uiOutput(ns("variants_summary"))),
          col_4(shiny::uiOutput(ns("organism_summary"))),
          col_4(shiny::uiOutput(ns(
            "gene_info_summary"
          )))
        )

        ### Previous version, kept until the seed in the global options modal is validated.
        # #   ____________________________________________________________________________
        # #   seed settings                                                           ####


        # # shiny::fluidRow(

        # shiny::HTML(paste0(
          # 'Seed ensuring reproducibility (optional, can be left as default)',
          # shinyWidgets::dropdownButton(
          # right = TRUE,
          # size = 'xs',
          # label = "Design file requirements",
          # shiny::includeMarkdown(system.file("extdata", "seed.md", package = "DIANE")),
          # circle = TRUE,
          # status = "success",
          # inline = TRUE,
          # icon = shiny::icon("question"),
          # width = "550px",
          # tooltip = shinyWidgets::tooltipOptions(title = "More details")
          # )
          # )),

          # shiny::column(3,
            # shiny::uiOutput(ns("seed_field"))
          # ),
          # shiny::column(5,
          # shinyWidgets::actionBttn(
            # ns("change_seed"),
            # label = "Change seed",
            # style = "material-flat",
            # color = "warning",
            # width = "100%"
          # )),

          # shiny::column(4,
          # shinyWidgets::actionBttn(
            # ns("set_seed"),
            # label = "Set seed",
            # style = "material-flat",
            # color = "success", width = '100%',

          # )
        # # )

      ),
      
      
      #   ____________________________________________________________________________
      #   Previews                                                                ####
      
      
      shinydashboardPlus::box(
        title = "Sample to sample correlation",
        width = 4,
        solidHeader = FALSE,
        status = "success",
        collapsible = TRUE,
        closable = FALSE,
        # square : the matrix is
        plotly::plotlyOutput(ns("heatmap_preview"), height = 420),
        shiny::uiOutput(ns("outlier_warning")),
        footer = "Replicates of a condition should form a block along the diagonal.
      A sample that resembles other conditions more than its own replicates draws a
      visible cross, and is worth checking before going further."
      ),
      
      
      
      
      #   ____________________________________________________________________________
      #   design                                                                  ####
      
      shinydashboardPlus::box(
        title = "Design",
        width = 4,
        solidHeader = FALSE,
        status = "success",
        collapsible = TRUE,
        closable = FALSE,
        shiny::uiOutput(ns("design_import_ui")),
        DT::dataTableOutput(ns("design_preview")),
        footer = "Describe the levels of each factors for your conditions"
      )
    ),
    
    shiny::br(),
    shiny::hr(),


    #   ____________________________________________________________________________
    #   Raw data quality control                                                ####

    shiny::fluidRow(
      shinydashboardPlus::box(
        title = "Raw data quality control",
        width = 12,
        solidHeader = FALSE,
        status = "success",
        collapsible = TRUE,
        closable = FALSE,
        shiny::fluidRow(
          col_6(shiny::plotOutput(ns("depth_preview"), height = 420)),
          col_6(shiny::plotOutput(ns("detected_preview"), height = 420))
        ),
        footer = "Read together, those two separate a shallow library, which sees
      less of everything, from a degraded one, which sees fewer genes. The first
      stays usable, the second does not."
      ),

      # Not collapsible : this table's rendering is what loads r$raw_counts, and
      # shiny stops rendering what is hidden.
      shinydashboardPlus::box(
        title = "Raw count table",
        width = 12,
        solidHeader = FALSE,
        status = "success",
        collapsible = FALSE,
        closable = FALSE,
        DT::dataTableOutput(ns("raw_data_preview"))
      )
    )
  )
}

#' import_data Server Function
#' @importFrom utils read.csv
#' @importFrom utils head
#' @importFrom stats heatmap
#' @importFrom shinydashboard renderValueBox
#' @importFrom shinydashboard valueBox
#' @noRd
mod_import_data_server <- function(input, output, session, r) {
  ns <- session$ns

  #   ____________________________________________________________________________
  #   Data source                                                             ####

  # Kept logical : r$use_demo is read as such by mod_normalisation
  use_demo <- shiny::reactive({
    shiny::req(input$data_source)
    input$data_source == "integrated"
  })

  #   ____________________________________________________________________________
  #   Data reset                                                              ####
  
  # resets the global reactive variables that were maybe already created
  # when demo usage is toggled :
  
  shiny::observeEvent(priority = 50, {
    use_demo()
    # r$selected_preloaded_dataset
    input$org_select
  }, {
    golem::message_dev("Reseting all the values")
    r$raw_counts = NULL
    r$normalized_counts = NULL
    r$normalized_counts_pre_filter = NULL
    r$conditions = NULL
    r$design = NULL
    r$gene_lists = list()
    r$tcc = NULL
    r$clusterings = list()
    r$current_comparison = NULL
    r$current_network = NULL
    r$top_tags = list()
    r$fit = NULL
    # r$regulators = NULL
    r$use_demo = use_demo()
    r$splicing_aware = NULL
    # r$gene_info = NULL
    # r$organism = NULL
    r$integrated_dataset = NULL
    # r$integrated_dataset = NULL
    r$custom_go = NULL
    
    # golem::message_dev("Reseting pre-selected organisms and dataset")
    # r$preselected_organism <-  NULL
    # r$preselected_dataset <-  NULL
    
  })
  
  
  ### Previous version, kept until the seed in the global options modal is validated.
  # #   ____________________________________________________________________________
  # #   seed setting                                                            ####

  # output$seed_field <- shiny::renderUI({
    # shiny::req(r$seed)
    # shiny::numericInput(
      # ns("seed"),
      # min = 0,
      # max = 2 ^ 8,
      # label = NULL,
      # value = r$seed,
      # width = "100%"
    # )
  # })


  # shiny::observeEvent(input$change_seed, {
    # r$seed = round(runif(n = 1, min = 0, max = 2 ^ 7))
    # shiny::updateNumericInput(session,
                              # ns("seed"),
                              # value = r$seed)
  # })

  # shiny::observeEvent(input$set_seed, {
    # r$seed <- input$seed
    # print(paste("changed global seed to", r$seed))
  # })
  
  
  #   ____________________________________________________________________________
  #   expression file                                                         ####
  
  
  raw_data <- shiny::reactive({
    req(r$organism)
    golem::print_dev("raw_data reactive")
    
    ###FIXME : we could put this in the first else, by putting req(input$raw_data) after
    r$raw_counts = NULL
    r$normalized_counts = NULL
    r$normalized_counts_pre_filter = NULL
    r$conditions = NULL
    r$design = NULL
    r$gene_lists = list()
    r$tcc = NULL
    r$clusterings = list()
    r$current_comparison = NULL
    r$current_network = NULL
    r$top_tags = list()
    r$fit = NULL
    r$regulators = NULL
    r$use_demo = use_demo()
    r$splicing_aware = NULL
    r$gene_info = NULL
    r$custom_go = NULL
    
    if (use_demo()) { ###Import demo count data. Demo also stands for integrated datasets.
      
      req(r$integrated_dataset)
      req(all(r$integrated_dataset %in% dataset_choices()))
      golem::print_dev("Import demo count data.")
      
      # Import DIANE legacy demo data if Arabidopsis and this specific dataset is selected.
      if(r$integrated_dataset == "Abiotic Stresses" & r$organism == "Arabidopsis thaliana"){
        r$use_demo = use_demo()
        d <- DIANE::abiotic_stresses[["raw_counts"]]
      } else {
        r$use_demo = use_demo()
        d <- DIANE::integrated_datasets[[r$organism]][[r$integrated_dataset]][["count"]]
        # browser()
      }
    }
    else{ ###Import user defined count data
      req(input$raw_data)
      golem::print_dev("User defined count data")
      path = input$raw_data$datapath
      
      r$raw_counts = NULL
      r$normalized_counts = NULL
      r$normalized_counts_pre_filter = NULL
      r$conditions = NULL
      r$design = NULL
      r$gene_lists = list()
      r$tcc = NULL
      r$clusterings = list()
      r$current_comparison = NULL
      r$current_network = NULL
      r$top_tags = list()
      r$fit = NULL
      r$regulators = NULL
      r$use_demo = use_demo()
      r$splicing_aware = NULL
      r$gene_info = NULL
      r$custom_go = NULL
      
      d <-
        read.csv(
          path,
          sep = input$sep,
          header = TRUE,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      if ("Gene" %in% colnames(d)) {
        d <-
          read.csv(
            path,
            sep = input$sep,
            header = TRUE,
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
        if ("Gene" %in% colnames(d)) {
          if (length(unique(d$Gene)) == length(d$Gene)) {
            rownames(d) <- d$Gene
            d <- d[, colnames(d) != "Gene"]
          }
          else{
            shinyalert::shinyalert(
              "Invalid input data",
              "It seems that you have duplicated gene/transcripts
              IDs in your input file.
              Please remove duplicates and re-upload your file",
              type = "error"
            )
            stop()
          }
        }
      }
      else{
        shinyalert::shinyalert(
          "Invalid input data",
          "Did you correctly set the separator?
           Does your data contains a column named \"Gene\"?",
          type = "error"
        )
        stop()
      }
    }
    
    ############### checking organism compatibility
    shiny::req(r$organism)
    if (r$organism != "Other") {
      golem::print_dev("Organism != other")
      # Check compatibility for legacy organisms, using regex
      if ((! r$organism %in% names(DIANE::organisms_index)) && !check_IDs(rownames(d), r$organism)) {
      # if (!check_IDs(rownames(d), r$organism)) {
        if (r$organism == "Arabidopsis thaliana")
          ex = "AT1G62510.1 or AT1G62510"
        
        if (r$organism == "Homo sapiens")
          ex = "ENSG00000000419"
        
        if (r$organism == "Mus musculus")
          ex = "ENSMUSG00000087910"
        
        if (r$organism == "Drosophilia melanogaster")
          ex = "FBgn0000036"
        
        if (r$organism == "Caenorhabditis elegans")
          ex = "WBGene00000042"
        
        if (r$organism == "Escherichia coli")
          ex = "acpS"
        
        # if(r$organism %in% names(DIANE::organisms_index))
        #   ex = sample(rownames(DIANE::organism(r$organism)[["annotation"]]), 1)
        
        
        shinyalert::shinyalert(
          "Invalid gene IDs",
          paste(
            "Some or all of the gene IDs in your Gene column do not match
          the expected pattern for the selected organism.
          For",
            r$organism,
            "they should be in the form",
            ex,
            "for example."
          ),
          type = "error"
        )
      
      # was outside of condition first. 
      shiny::req(check_IDs(rownames(d), r$organism))
        #stop()
        # Check ID for custom organisms, using rownames in count matrix and annotation.
        # && so a model organism never loads a custom annotation just to be rejected.
      } else if (r$organism %in% names(DIANE::organisms_index) &&
                 (!all(rownames(d) %in% rownames(DIANE::organism(r$organism)[["annotation"]])))) {

        annotated_genes <- rownames(DIANE::organism(r$organism)[["annotation"]])

        # Take gene_exemple if exist, otherwise take a random gene for exemple.
        ex = ifelse(!is.null(DIANE::organisms_index[[r$organism]][["gene_exemple"]]),
                    DIANE::organisms_index[[r$organism]][["gene_exemple"]],
                    sample(annotated_genes, size = 1))

        # Percentage of missing genes.
        missing_genes <-  (1- round(sum(rownames(d) %in% annotated_genes) / length(rownames(d)), digits = 3)) * 100
        
        # Popup if high number of missing genes.
        if(missing_genes > 10){
        # Display alert that shows percentage of genes without annotation.
        shinyalert::shinyalert(
          "Invalid gene IDs",
          paste(missing_genes,
            "% of the gene IDs in your Gene column are not in the gene 
            annotation of the selected organism.
            For",
            r$organism,
            "they should be in the form",
            ex,
            "for example."
          ),
          type = ifelse(missing_genes > 50, yes = "error", "warning")
        )} else {
          shiny::showNotification(
            ui = paste(
              missing_genes,
              "% of the gene IDs in your Gene column are not in the gene
            annotation."
            ),
            duration = 5,
            type = "warning"
          )
        }
        
      }
    }
    
    r$conditions <-
      stringr::str_split_fixed(colnames(d), "_", 2)[, 1]
    r$splicing_aware <- are_splice_variants(row.names(d))
    r$raw_counts <- d
    d
  })
  
  #   ____________________________________________________________________________
  #   splicing summary                                                        ####
  output$variants_summary <- shiny::renderUI({
    shiny::req(r$conditions)
    shiny::req(!is.null(r$splicing_aware))
    
    if (r$splicing_aware) {
      numberColor = "blue"
      number = "Alternative splicing aware"
      header = "gene identifiers"
    }
    else{
      numberColor = "blue"
      number = "No Alternative splicing information"
      header = "in gene identifiers"
    }
    shinydashboardPlus::descriptionBlock(
      number = number,
      numberColor = numberColor,
      text = header,
      rightBorder = TRUE,
    )
  })
  
  
  #   ____________________________________________________________________________
  #   Design import UI                                                        ####
  
  ### Previous version, kept until upload_field() is validated on this box.
  # output$design_import_ui <- shiny::renderUI({
    # req(!use_demo())
    # shiny::tagList(
      # shinyWidgets::awesomeRadio(
        # ns('sep_design'),
#
        # 'Separator : ',
        # c(
          # Comma = ',',
          # Semicolon = ';',
          # Tab = '\t'
        # ),
#
        # inline = TRUE,
        # status = "success"
      # ),
#
#
#
      # shiny::fileInput(
        # ns('design'),
        # label = shiny::HTML(paste0(shinyWidgets::dropdownButton(
          # right = TRUE,
          # size = 'xs',
          # label = "Design file requirements",
          # shiny::includeMarkdown(system.file("extdata", "designFile.md",
                                             # package = "DIANE")),
          # circle = TRUE,
          # status = "success",
          # inline = TRUE,
          # icon = shiny::icon("question"),
          # width = "550px",
          # tooltip = shinyWidgets::tooltipOptions(title = "More details")
        # ),
        # 'Choose CSV/TXT design file (optional)'
        # )),
        # accept = c(
          # 'text/csv',
          # 'text/comma-separated-values,text/plain',
          # '.csv',
          # '.txt'
        # )
      # )
      # )
  # })

  output$design_import_ui <- shiny::renderUI({
    req(!use_demo())
    upload_field(
      file_id = ns("design"),
      label = "Design file (optional)",
      separator_id = ns("sep_design"),
      help_md = "designFile.md",
      help_width = "550px",
      help_opens = "left",
      accept = c("text/csv", "text/comma-separated-values,text/plain",
                 ".csv", ".txt")
    )
  })
  
  
  #   ____________________________________________________________________________
  #   design loading                                                          ####
  
  design <- shiny::reactive({
    req(r$organism)
    golem::print_dev("Design reactive")
    if (use_demo()) { ###Import demo count data
      req(r$integrated_dataset)
      if(r$integrated_dataset == "Abiotic Stresses"){
        r$use_demo = use_demo()
        # data("abiotic_stresses", package = "DIANE")
        # d <- abiotic_stresses$design
        d <- DIANE::abiotic_stresses[["design"]]
      } else {
        # TODO: if there is not design ?? variable is set to NULL.
        r$use_demo = use_demo()
        d <- DIANE::integrated_datasets[[r$organism]][[r$integrated_dataset]][["design"]]
      }
    } else {
      req(r$conditions)
      req(input$design)
      path = input$design$datapath
      d <- read.csv(
        sep = input$sep_design,
        path,
        header = TRUE,
        stringsAsFactors = FALSE,
        row.names = "Condition"
      )
      if (sum(rownames(d) %in% r$conditions) < dim(d)[1]) {
        shinyalert::shinyalert(
          "Invalid design rownames...",
          paste(
            "The Condition column in your design file should be the experimental
                conditions:",
            paste(r$conditions, collapse = ', ')
          ),
          type = "error"
        )
        stop()
      }
    }
    
    r$design <- d
    d
  })
  
  
  #   ____________________________________________________________________________
  #   organism                                                                ####
  
  ### Previous version, kept until the static selectInput is validated.
  # # Reactive vector of organism to chose from.
  # org_choices <- shiny::reactive({
    # ## TODO : check if these packages are always loaded. Could reduce RAM usage.
    # ## TODO : check for arabidopsis.
    # choices <- c("Arabidopsis thaliana")
    # if (requireNamespace("org.Mm.eg.db", quietly = TRUE))
      # choices <- c(choices, "Mus musculus")

    # if (requireNamespace("org.Hs.eg.db", quietly = TRUE))
      # choices <- c(choices, "Homo sapiens")

    # if (requireNamespace("org.Ce.eg.db", quietly = TRUE))
      # choices <- c(choices, "Caenorhabditis elegans")

    # if (requireNamespace("org.Dm.eg.db", quietly = TRUE))
      # choices <- c(choices, "Drosophilia melanogaster")

    # if (requireNamespace("org.EcK12.eg.db", quietly = TRUE))
      # choices <- c(choices, "Escherichia coli")


    # choices <- c("Other", choices)

    # # Give name (genus) to pre-integrated data. We call them 'model".
    # names(choices) <- c("Other", rep("Model", length(choices)-1))

    # # import custom data
    # custom_orgs <- names(DIANE::organisms_index)
    # genus_custom_orgs <- c()
    # # Give a name to custom orgs. Either genus, or just the name of the organism.
    # for(i in custom_orgs){
      # if(!is.null(DIANE::organisms_index[[i]][["genus"]])){
        # genus_custom_orgs <- c(genus_custom_orgs, DIANE::organisms_index[[i]][["genus"]])
      # } else {
        # genus_custom_orgs <- c(genus_custom_orgs, i)
      # }
    # }
    # names(custom_orgs) <- genus_custom_orgs

    # choices <- c(choices, custom_orgs)

    # # Chose organism based on url query and integrated data
    # if(!is.null(r$included_genus)){
      # if(all(r$included_genus %in% names(choices))){
        # choices <-choices[names(choices) %in% r$included_genus]
      # }
    # }

    # golem::print_dev(unname(choices))
    # unname(choices)
  # })


  # output$org_selection <- shiny::renderUI({

    # # Check if URL organism is in the list.
    # org_select <- "Arabidopsis thaliana"
    # if(!is.null(r$preselected_organism)){
      # if(r$preselected_organism %in% names(DIANE::organisms_index)){
        # org_select <- r$preselected_organism
      # }
    # }

    # shiny::selectInput(
      # ns("org_select"),
      # # label = "Your organism :",
      # label = shiny::HTML(paste0(
        # 'Your organism ',
        # shinyWidgets::dropdownButton(
        # right = FALSE,
        # size = 'xs',
        # label = "Organism description",
        # shiny::htmlOutput(ns('organism_description')),
        # circle = TRUE,
        # status = "success",
        # inline = TRUE,
        # icon = shiny::icon("question"),
        # width = "550px",
        # tooltip = shinyWidgets::tooltipOptions(title = "Informations about selected organism")
        # )
      # )),
      # choices = org_choices(),
      # selected = org_select
    # )
  # })
  
  # Organisms without an integrated dataset ("Other" among them) : disable the
  # option instead of silently moving the user's choice. Depends on the organism
  # only, never on input$data_source, otherwise the update below loops.
  shiny::observe({
    shiny::req(r$organism)
    if (length(dataset_choices()) > 0) {
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "data_source",
        disabledChoices = character(0)
      )
    } else {
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "data_source",
        selected = "upload",
        disabledChoices = "integrated"
      )
    }
  })

  # Says why the integrated option is greyed out, next to the button itself
  # output$data_source_hint <- shiny::renderUI({
  #   shiny::req(r$organism)
  #   shiny::req(length(dataset_choices()) == 0)
  #   shiny::helpText(
  #     paste("No integrated dataset is available for", r$organism,
  #           "- import your own expression file below.")
  #   )
  # })
  
  ## TODO : Not used anymore. Could be commented out.
  # This was the old style tooltip.
  # output$org_install <- shiny::renderText({
  #   print("output$org_install. This should not be.")
  #   if (!golem::get_golem_options("server_version")) {
  #     "<b>The organisms listed below are the one detected on the system.</b> <br>
  #   To use new organisms, please close DIANE and install the corresponding
  #   package from R or Rstudio consoles.<br>
  # 
  #   <code> if (!requireNamespace(\"BiocManager\", quietly = TRUE))
  #     install.packages(\"BiocManager\") </code> <br>
  # 
  #   For Human : <code> BiocManager::install(\"org.Hs.eg.db\") </code> <br>
  #   For Mouse : <code> BiocManager::install(\"org.Mm.eg.db\") </code> <br>
  #   For Caenorhabditis elegans : <code> BiocManager::install(\"org.Ce.eg.db\") </code> <br>
  #   For E coli : <code> BiocManager::install(\"org.EcK12.eg.db\") </code> <br>
  #   For fruit fly : <code> BiocManager::install(\"org.Dm.eg.db\") </code> <br>
  # 
  #   Then, when you launch DIANE again, your organism should appear
  #   in the following selection menu.
  # 
  #   For now, only Arabidopsis, Human and Mouse are working.
  #   "
  #   }
  #   else{
  #     "For now, you can choose between all the organisms above"
  #   }
  # })
  
  #   ____________________________________________________________________________
  #   Custom datasets loading                                                 ####
  
  # Store selected organism (witht a high priority.)
  shiny::observe(priority = 40,{
    r$organism <- input$org_select
    golem::print_dev(paste("r$organism : ", r$organism))
  })

  # Contain a vector of possible datasets for any organism.
  dataset_choices <- shiny::reactive({
    req(r$organism)
    if(r$organism == "Arabidopsis thaliana"){
      c("Abiotic Stresses", names(DIANE::integrated_datasets[[r$organism]]))
    } else {
      names(DIANE::integrated_datasets[[r$organism]])
    }
  })
  
  
  output$integrated_label <- shiny::renderText({
    if (length(dataset_choices()) > 0) "Integrated dataset" else "No integrated dataset"
  })
  
  # TODO : could be hidden !
  # Allow user to chose an integrated dataset.
  # TODO : Could be an UIupdate. 
  # NOTE : the req(dataset_choices()) was not there before. I had a bug without him I think, but cannot find it anymore.
  # Be carefull.
  output$dataset_selection_ui <- shiny::renderUI({
    shiny::req(use_demo())
    req(dataset_choices())
    # if(!is.null(dataset_choices())){
    selected_dataset <- NULL
    shiny::isolate({
      if(!is.null(dataset_choices())){
        selected_dataset <- dataset_choices()[dataset_choices() %in% r$preselected_dataset]
      }
    })
    
    shiny::selectInput(
      ns("dataset_selection"),
      label = "Integrated dataset selection",
      choices = dataset_choices(), ###Will be "" if no existing dataset.
      selected = shiny::isolate(selected_dataset)
    )
    # }


    # }
  })

  ### Cheap : rendered while the user is still on another tab.
  shiny::outputOptions(output, "dataset_selection_ui", suspendWhenHidden = FALSE)


  # Store integrated dataset value.
  shiny::observeEvent({
    input$dataset_selection
    use_demo()
    r$organism ## to fix loading problem. Dataset was not loaded when an organism without integrated dataset was selected.
  }, {
    if(use_demo()){
      req(input$dataset_selection) ## to fix loading problem
      req(r$organism)
      req(dataset_choices())
      # browser()
      r$integrated_dataset <- input$dataset_selection
      golem::print_dev(paste0("Dataset and organism : ", r$integrated_dataset, " - ", r$organism))
    }
  })
  
  # The "no integrated dataset" case is now signalled by data_source_hint,
  # next to the disabled button rather than inside a tab panel.

        
  #   ____________________________________________________________________________
  #   Organism description                                                    ####
  
  ## Informations about integrated organism data.
  output$organism_description <- shiny::renderText({
    req(r$organism)
    if (r$organism %in% c(
      "Arabidopsis thaliana",
      "Escherichia coli",
      "Drosophilia melanogaster",
      "Caenorhabditis elegans",
      "Homo sapiens",
      "Mus musculus"
    )) {
      "<h4>Organism descripton</h4><p>This organism was installed using the corresponding orgdb package from bioconductor.
            You can check the specific version in the \"Software versions\" tab<p>"
    } else {
      "Nothing"
      req(r$organism)
      # req(use_demo())
      
      organism_informations <- DIANE::organisms_index[[r$organism]][["informations"]]
      organism_description = ""
      string = "<div class='descriptive-field'>"
      url_pattern <- "(http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+)" ###Use to detect URL. Need that the field contains ONLY an url.
      if(!is.null(organism_informations)){
        ###We need to have everything stored in a string. So we just create this string field by field. And then we just print her.
        for(i in names(organism_informations)){
          if(stringr::str_detect(string = organism_informations[[i]], pattern = url_pattern)){
            text_with_url <- stringr::str_replace_all(string =organism_informations[[i]], pattern = url_pattern, replacement = paste0("<a target=\"_blank\" href=","\\1",">","\\1","</a>"))
            string = paste(string, paste(tags$b(i), " : ", text_with_url, "</br>"), " ")
          } else {
            string = paste(string, paste(tags$b(i), " : ", organism_informations[[i]], "</br>"), " ")
          }
        }
        organism_description <- paste0(string, "</div>")
      } else {
        organism_description <- "<p>No organism description provided<p>"
      }
      paste0(
        "<h4>Organism descripton</h4>",
        organism_description
      )
      
    }
    # These organism are installed via orgdb. 
    
  })
  
  
  #   ____________________________________________________________________________
  #   import user data UI                                                     ####
  
  ### Previous version, kept until upload_field() is validated on this box.
  # output$count_import_ui <- shiny::renderUI({
    # shiny::req(!use_demo())
    # print("output$data_import_ui")
    # shiny::tagList(
      # shiny::h3("Import expression file", style="text-decoration: underline"),
      # shinyWidgets::awesomeRadio(
        # ns('sep'),
        # 'Separator : ',
        # c(
          # Comma = ',',
          # Semicolon = ';',
          # Tab = '\t'
        # ),
        # inline = TRUE,
        # status = "success"
      # ),
#
      # shiny::fileInput(
        # ns('raw_data'),
        # label = shiny::HTML(paste0('Choose CSV/TXT expression file',
                                   # shinyWidgets::dropdownButton(
                                     # size = 'xs',
                                     # label = "Input file requirements",
                                     # shiny::includeMarkdown(
                                       # system.file("extdata", "expressionFile.md", package = "DIANE")
                                     # ),
                                     # circle = TRUE,
                                     # status = "success",
                                     # inline = TRUE,
                                     # icon = shiny::icon("question"),
                                     # width = "1200px",
                                     # tooltip = shinyWidgets::tooltipOptions(title = "More details")
                                   # )                     
        # )),
        # accept = c(
          # 'text/csv',
          # 'text/comma-separated-values,text/plain',
          # '.csv',
          # '.txt'
        # )
      # ),
    # )
  # })

  output$count_import_ui <- shiny::renderUI({
    shiny::req(!use_demo())
    shiny::tagList(
    upload_field(
      file_id = ns("raw_data"),
      label = "Expression file",
      separator_id = ns("sep"),
      help_md = "expressionFile.md", 
      help_opens = "right",
      help_width = "900px",
      hint = shiny::uiOutput(ns("gene_ids")),
      accept = c("text/csv", "text/comma-separated-values,text/plain",
                 ".csv", ".txt")
    ),
    shiny::br()
    )
  })
  
  
  #   ____________________________________________________________________________
  #   gene infos upload                                                           ####
  
  ### Previous version, kept until upload_field() is validated on this box.
  # output$custom_organism_ui <- shiny::renderUI({
    # shiny::req(r$organism == "Other")
    # shiny::tagList(
      # shiny::h3("Import gene information file", style="text-decoration: underline"),
      # shinyWidgets::awesomeRadio(
        # ns('sep_gene_info'),
        # status = "success",
        # 'Separator : ',
        # c(Tab = '\t'),
        # inline = TRUE
      # ),
#
      # shiny::fileInput(
        # inputId = ns('gene_info_input'),
        # label = HTML(paste0('Choose CSV/TXT gene information file (optional)',
                            # shinyWidgets::dropdownButton(
                              # size = 'xs',
                              # label = "Gene information file requirements",
                              # shiny::includeMarkdown(system.file("extdata", "infoFile.md",
                                                                 # package = "DIANE")),
                              # circle = TRUE,
                              # status = "success",
                              # inline = TRUE,
                              # icon = shiny::icon("question"),
                              # width = "1200px",
                              # tooltip = shinyWidgets::tooltipOptions(title = "More details")
                            # )
        # )),
        # accept = c(
          # 'text/csv',
          # 'text/comma-separated-values,text/plain',
          # '.csv',
          # '.txt'
        # )
      # )
      # )
  # })

  output$custom_organism_ui <- shiny::renderUI({
    shiny::req(r$organism == "Other")
    shiny::tagList(
      upload_field(
        file_id = ns("gene_info_input"),
        label = "Gene information file (optional)",
        separator_id = ns("sep_gene_info"),
        help_md = "infoFile.md",
        help_width = "900px",
        separator = "\t",
        accept = c("text/csv", "text/comma-separated-values,text/plain",
                   ".csv", ".txt")
      ),
    shiny::br(),
    )
  })
  
  #   ____________________________________________________________________________
  #   Dataset description                                                     ####
  
  output$dataset_description <- shiny::renderText({
    req(r$organism)
    req(r$integrated_dataset)
    # req(use_demo())
    
    dataset_informations <- DIANE::integrated_datasets[[r$organism]][[r$integrated_dataset]][["description"]]
    dataset_description = ""
    string = "<div class='descriptive-field'>"
    url_pattern <- "(http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+)" ###Use to detect URL. Need that the field contains ONLY and url.
    if(!is.null(dataset_informations)){
      ###We need to have everything stored in a string. So we just create this string field by field. And then we just print her.
      for(i in 1:nrow(dataset_informations)){
        if(stringr::str_detect(string = dataset_informations[i,2], pattern = url_pattern)){
          text_with_url <- stringr::str_replace_all(string = dataset_informations[i,2], pattern = url_pattern, replacement = paste0("<a  target=\"_blank\" href=","\\1",">","\\1","</a>"))
          string = paste(string, paste(tags$b(dataset_informations[i,1]), " : ", text_with_url, "</br>"), " ")
        } else {
          string = paste(string, paste(tags$b(dataset_informations[i,1]), " : ", dataset_informations[i,2], "</br>"), " ")
        }
      }
      dataset_description <- paste0(string, "</div><hr>")
    } else if (r$integrated_dataset == "Abiotic Stresses" & r$organism == "Arabidopsis thaliana"){
      dataset_description <- '
                  <b>Dataset name</b> : Response to abiotic stress</br>
                  <b>Organism</b> : Arabidopsis thaliana</br>
                  <b>Description</b> : This dataset contains the transcriptome of Arabidopsis thaliana plants exposed to global warming induced conditions. The experimental perturbations studied are high tempreature, hight salinity and osmotic changes in the soil. Each factors has two levels, one of them considered as the reference, and the other one as the stress level.</br>
                  <b>Name correspondance</b> : C = control ; H = heat ; S = salt , M = mannitol</br>
                  <b>Authors</b> : Nasser Sewelam, Dominik Brilhaus, Andrea Bräutigam, Saleh Alseekh, Alisdair R Fernie, Veronica G Maurino</br>
                  <b>Article</b> : Molecular plant responses to combined abiotic stresses put a spotlight on unknown and abundant genes</br>
                  <b>DOI</b> : <a  target=\"_blank\" href="https://doi.org/10.1093/jxb/eraa250">https://doi.org/10.1093/jxb/eraa250</a>
                  <hr>
                '
    } else {
      dataset_description <- "<p>No dataset description provided<p><hr>"
    }
    dataset_description
  })

  ### Same, and keeps it out of the heatmap's flush.
  shiny::outputOptions(output, "dataset_description", suspendWhenHidden = FALSE)


  #   ____________________________________________________________________________
  #   genes info                                                              ####
  
  gene_info <- shiny::reactive({
    req(r$raw_counts)
    req(r$conditions)
    req(r$organism)
    
    if (r$organism != "Other") {
      ids <- rownames(r$raw_counts)
      if (r$splicing_aware) {
        ids <- get_locus(rownames(r$raw_counts))
      }
      # if (r$organism == "Lupinus albus") {
      #   d <-
      #     DIANE:::lupine$annotation[intersect(ids, rownames(DIANE:::lupine$annotation)),]
      # }
      # else{
        d <- get_gene_information(ids, r$organism)
      # }
      
    }
    else{
      if (!is.null(input$gene_info_input)) {
        path = input$gene_info_input$datapath
        
        d <- read.csv(
          sep = input$sep_gene_info,
          path,
          header = TRUE,
          stringsAsFactors = FALSE
        )
        
        # FIXME : be less stringent here - column 1 => gene ID, the others => anything
        if (!'label' %in% colnames(d) &
            !'description' %in% colnames(d)) {
          stop("There should be a label and/or description field in the annotation file")
        }
        # takes as rownames only the genes present in the expression file
        d <- d[d$Gene %in% rownames(r$raw_counts), ]
        
        # handles the case where genes are duplicated, and pastes
        # the annotations for the same genes in one row
        if (length(unique(d$Gene)) < length(d$Gene)) {
          new_d <- aggregate(. ~ Gene, data = d, FUN = paste, collapse = ",")
          d <- new_d
        }
        rownames(d) <- d$Gene
        d <- d[ colnames(d) != "Gene"]
      }
      else{
        d <- NULL
      }
    }
    d
  })
  ########### table view

  output$raw_data_preview <- DT::renderDataTable({
    shiny::req(r$organism)
    if(use_demo()){
      shiny::req(r$integrated_dataset)
    }
    raw_data()
    shiny::req(r$raw_counts)
    head(r$raw_counts)
  })
  
  ########## matrix preview

  # low counts dropped : their log inflates variance and blurs the blocks
  preview_counts <- shiny::reactive({
    shiny::req(r$raw_counts)
    r$raw_counts[rowSums(r$raw_counts) > 25, , drop = FALSE]
  })

  output$heatmap_preview <- plotly::renderPlotly({
    golem::print_dev("Print correlation heatmap")
    draw_correlation_heatmap_interactive(preview_counts())
  })

  output$outlier_warning <- shiny::renderUI({
    suspects <- tryCatch(detect_sample_outliers(preview_counts()),
                         error = function(e) NULL)
    shiny::req(suspects)
    suspects <- suspects[suspects$flagged, ]
    shiny::req(nrow(suspects) > 0)

    lines <- paste0("<li><b>", suspects$sample, "</b> &mdash; ", suspects$reason,
                    " (median r = ", round(suspects$overall, 2), ")</li>",
                    collapse = "")
    shiny::div(
      style = "color: #b2482f; font-size: 12px; margin-top: 8px;",
      shiny::icon("exclamation-triangle"),
      shiny::HTML(paste0(
        "<b>", nrow(suspects), " sample",
        if (nrow(suspects) > 1) "s" else "", " worth checking</b>",
        "<ul style='margin: 4px 0 0 0; padding-left: 20px;'>", lines, "</ul>"
      ))
    )
  })

  ########## raw data quality control

  # r$raw_counts, not preview_counts() : dropping low count genes would falsify
  # the depth and empty the count of detected genes.
  qc_plot <- function(expr) {
    tryCatch(expr,
             error = function(e) shiny::validate(shiny::need(FALSE, e$message)))
  }

  output$depth_preview <- shiny::renderPlot({
    shiny::req(r$raw_counts)
    qc_plot(draw_sequencing_depth(r$raw_counts, palette = r$palette))
  })

  output$detected_preview <- shiny::renderPlot({
    shiny::req(r$raw_counts)
    qc_plot(draw_detected_genes(r$raw_counts, palette = r$palette))
  })

  
  
  #   ____________________________________________________________________________
  #   ValueBoxes summaries                                                    ####
  
  output$gene_ids <- shiny::renderUI({
    shiny::req(r$organism)
    shiny::req(!use_demo())  # nothing to satisfy when the data is already in
    # browser()
    if (r$organism == "Other" || r$organism == "other")
      txt <- "No gene ID requirement"
    else if (r$organism  %in% names(DIANE::organisms_index))
      txt <- sample(rownames(DIANE::organism(r$organism)[["annotation"]]), 1)
    # else if (r$organism == "Oryza sativa (rapdb)")
    #   txt <- c("Os01g0100600")
    # else if (r$organism == "Oryza sativa (msu)")
    #   txt <- c("LOC_Os01g11590")
    # else if (r$organism == "Oryza glaberrima")
    #   txt <- c("ORGLA01G0099000")
    else{
      data("regulators_per_organism", package = "DIANE")
      txt <- regulators_per_organism[[r$organism]]
    }
    # Display a gene ID exemple.
    shiny::tags$p(
      class = "uf-hint",
      "Gene ID example: ",
      shiny::tags$code(sample(txt, size = 1))
    )
  })
  
  
  output$data_dim <- shinydashboard::renderValueBox({
    shiny::req(r$raw_counts)
    
    shinydashboard::valueBox(
      value = dim(r$raw_counts)[1],
      subtitle = "genes",
      color = "aqua",
      width = 4
    )
  })
  output$conditions <- shinydashboard::renderValueBox({
    shiny::req(r$conditions)
    
    shinydashboard::valueBox(value = length((unique(r$conditions))),
                             subtitle = "conditions",
                             color = "teal")
  })
  
  output$samples <- shinydashboard::renderValueBox({
    shiny::req(r$raw_counts)
    shinydashboard::valueBox(
      value = dim(r$raw_counts)[2],
      subtitle = "samples",
      color = "olive"
    )
  })
  
  output$gene_info_summary <- shiny::renderUI({
    shiny::req(r$raw_counts)
    shiny::req(r$organism)
    
    ######## setting gene info here
    r$gene_info <- gene_info()
    
    if (is.null(r$gene_info)) {
      numberColor = "orange"
      number = "No additional gene data provided"
      header = ""
      numberIcon = shiny::icon('times')
    }
    else{
      numberColor = "olive"
      number = "Additional gene data available"
      numberIcon = shiny::icon('check')
      header = paste(colnames(r$gene_info), collapse = ', ')
    }
    shinydashboardPlus::descriptionBlock(
      number = number,
      numberColor = numberColor,
      numberIcon = numberIcon,
      text = header,
      rightBorder = FALSE
    )
  })
  
  output$organism_summary <- shiny::renderUI({
    shiny::req(r$organism)
    
    shinydashboardPlus::descriptionBlock(
      number = r$organism,
      numberColor = "teal",
      text = "organism database",
      rightBorder = FALSE
    )
  })
  
  ######### render design
  output$design_preview <- DT::renderDataTable({
    DT::datatable(design(),  options = list(scrollX=TRUE, scrollCollapse=TRUE))
    # DT::datatable(design())
  })
  
}

## To be copied in the UI
# mod_import_data_ui("import_data_ui_1")

## To be copied in the server
# callModule(mod_import_data_server, "import_data_ui_1")
