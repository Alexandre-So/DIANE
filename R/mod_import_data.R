


#' import_data UI Function
#'
#' @description A shiny Module to import expression data.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shinydashboard valueBoxOutput
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_import_data_ui <- function(id) {
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
        
        shiny::fluidRow(
            shiny::column(4,
            shinyWidgets::switchInput(
              ns("use_demo"),
              "Toggle to import your data",
              value = TRUE,
              onLabel = "Embedded data",
              offLabel = "Your dataset",
              onStatus = "success"
              
            )
          )
          ,
          col_8(shiny::uiOutput(ns("gene_ids")))
          
        ),
        
        # Organism selection
        shiny::uiOutput(ns("org_selection")),
            shiny::uiOutput(ns("dataset_selection_ui")),
        
        # # UI for integrated dataset
        # shiny::tabsetPanel(
        #   shiny::tabPanel("Dataset information",
        #            
        #            ),
        #   shiny::tabPanel("Organism informations", "Ouiiiiiiiiiiiiiii")
        # ),
        # Display a message if no datasets are available for a specific organism
        
        shiny::tabsetPanel(id="dataset-description-tabsetPanel",
          shiny::tabPanel(title = "Dataset description", 
                          shiny::htmlOutput(ns('dataset_description')),
                          shiny::htmlOutput(ns("no_dataset_warning"))
          ),
          shiny::tabPanel("Organism description",
                          shiny::htmlOutput(ns('organism_description'))
          )
         ),
        
        
        shiny::uiOutput(ns("count_import_ui")),
        shiny::uiOutput(ns("custom_organism_ui")),
        
        # shiny::htmlOutput(ns("dataset_description")),
        
        shiny::fluidRow(
          shinydashboard::valueBoxOutput(ns("data_dim")),
          shinydashboard::valueBoxOutput(ns("conditions")),
          shinydashboard::valueBoxOutput(ns("samples")),
          
          col_4(shiny::uiOutput(ns("variants_summary"))),
          col_4(shiny::uiOutput(ns("organism_summary"))),
          col_4(shiny::uiOutput(ns(
            "gene_info_summary"
          )))
        ),
        
        
        #   ____________________________________________________________________________
        #   seed settings                                                           ####
        
        
        shiny::uiOutput(ns("seed_field")),
        
        
        shinyWidgets::actionBttn(
          ns("change_seed"),
          label = "Change seed",
          style = "material-flat",
          color = "warning"
        ),
        
        
        shinyWidgets::actionBttn(
          ns("set_seed"),
          label = "Set seed",
          style = "material-flat",
          color = "success"
          
        ),
        col_4(
          shinyWidgets::dropdownButton(
            size = 'xs',
            label = "Input file requirements",
            shiny::includeMarkdown(system.file("extdata", "seed.md", package = "DIANE")),
            circle = TRUE,
            status = "success",
            icon = shiny::icon("question"),
            width = "1200px",
            tooltip = shinyWidgets::tooltipOptions(title = "More details")
          )
        )
      ),
      
      
      #   ____________________________________________________________________________
      #   Previews                                                                ####
      
      
      shinydashboardPlus::box(
        title = "Preview of the expression matrix",
        width = 4,
        solidHeader = FALSE,
        status = "success",
        collapsible = TRUE,
        closable = FALSE,
        shiny::plotOutput(ns("heatmap_preview"), height = 550),
        footer = "This might help you visualize the general aspect of the data and different sequencing depths
      of your conditions."
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
    DT::dataTableOutput(ns("raw_data_preview"))
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
  #   Data reset                                                              ####
  
  # resets the global reactive variables that were maybe already created
  # when demo usage is toggled :
  
  shiny::observeEvent(priority = 50, {
    input$use_demo
    # r$selected_preloaded_dataset
    input$org_select
  }, {
    golem::message_dev("Reseting all the values")
    r$raw_counts = NULL
    r$normalized_counts = NULL
    r$normalized_counts_pre_filter = NULL
    r$conditions = NULL
    r$design = NULL
    r$DEGs = list()
    r$tcc = NULL
    r$clusterings = list()
    r$current_comparison = NULL
    r$current_network = NULL
    r$top_tags = list()
    r$fit = NULL
    # r$regulators = NULL
    r$use_demo = input$use_demo
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
  
  
  #   ____________________________________________________________________________
  #   seed setting                                                            ####
  
  output$seed_field <- shiny::renderUI({
    shiny::req(r$seed)
    shiny::numericInput(
      ns("seed"),
      min = 0,
      max = 2 ^ 8,
      label = "Seed ensuring reproducibility (optional, can be left as default value) :",
      value = r$seed,
      width = "100%"
    )
  })
  
  
  shiny::observeEvent(input$change_seed, {
    r$seed = round(runif(n = 1, min = 0, max = 2 ^ 7))
    shiny::updateNumericInput(session,
                              ns("seed"),
                              value = r$seed)
  })
  
  shiny::observeEvent(input$set_seed, {
    r$seed <- input$seed
    print(paste("changed global seed to", r$seed))
  })
  
  
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
    r$DEGs = list()
    r$tcc = NULL
    r$clusterings = list()
    r$current_comparison = NULL
    r$current_network = NULL
    r$top_tags = list()
    r$fit = NULL
    r$regulators = NULL
    r$use_demo = input$use_demo
    r$splicing_aware = NULL
    r$gene_info = NULL
    r$custom_go = NULL
    
    if (input$use_demo) { ###Import demo count data. Demo also stands for integrated datasets.
      
      req(r$integrated_dataset)
      req(all(r$integrated_dataset %in% dataset_choices()))
      golem::print_dev("Import demo count data.")
      
      # Import DIANE legacy demo data if Arabidopsis and this specific dataset is selected.
      if(r$integrated_dataset == "Abiotic Stresses" & r$organism == "Arabidopsis thaliana"){
        r$use_demo = input$use_demo
        d <- DIANE::abiotic_stresses[["raw_counts"]]
      } else {
        r$use_demo = input$use_demo
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
      r$DEGs = list()
      r$tcc = NULL
      r$clusterings = list()
      r$current_comparison = NULL
      r$current_network = NULL
      r$top_tags = list()
      r$fit = NULL
      r$regulators = NULL
      r$use_demo = input$use_demo
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
      if ((! r$organism %in% names(DIANE::organisms)) && !check_IDs(rownames(d), r$organism)) {
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
        
        # if(r$organism %in% names(DIANE::organisms))
        #   ex = sample(rownames(DIANE::organisms[[r$organism]][["annotation"]]), 1)
        
        
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
      } else if (r$organism %in% names(DIANE::organisms) &
                 (!all(rownames(d) %in% rownames(DIANE::organisms[[r$organism]][["annotation"]])))) {
        
        # Take gene_exemple if exist, otherwise take a random gene for exemple.
        ex = ifelse(!is.null(DIANE::organisms[[r$organism]][["gene_exemple"]]),
                    DIANE::organisms[[r$organism]][["gene_exemple"]],
                    sample(rownames(DIANE::organisms[[r$organism]][["annotation"]]), size = 1))
        
        # Percentage of missing genes.
        missing_genes <-  (1- round(sum(rownames(d) %in% rownames(DIANE::organisms[[r$organism]][["annotation"]])) / length(rownames(d)), digits = 3)) * 100
        
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
        )
        
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
  
  output$design_import_ui <- shiny::renderUI({
    req(!input$use_demo)
    shiny::tagList(
      shinyWidgets::awesomeRadio(
        ns('sep_design'),
        
        'Separator : ',
        c(
          Comma = ',',
          Semicolon = ';',
          Tab = '\t'
        ),
        
        inline = TRUE,
        status = "success"
      ),
      
      
      
      shiny::fileInput(
        ns('design'),
        label = shiny::HTML(paste0(shinyWidgets::dropdownButton(
          size = 'xs',
          label = "Design file requirements",
          shiny::includeMarkdown(system.file("extdata", "designFile.md",
                                             package = "DIANE")),
          circle = TRUE,
          status = "success",
          inline = TRUE,
          icon = shiny::icon("question"),
          width = "550px",
          tooltip = shinyWidgets::tooltipOptions(title = "More details")
        ),
        'Choose CSV/TXT design file (optional)'
        )),
        accept = c(
          'text/csv',
          'text/comma-separated-values,text/plain',
          '.csv',
          '.txt'
        )
      )
      )
  })
  
  
  #   ____________________________________________________________________________
  #   design loading                                                          ####
  
  design <- shiny::reactive({
    req(r$organism)
    golem::print_dev("Design reactive")
    if (input$use_demo) { ###Import demo count data
      req(r$integrated_dataset)
      if(r$integrated_dataset == "Abiotic Stresses"){
        r$use_demo = input$use_demo
        # data("abiotic_stresses", package = "DIANE")
        # d <- abiotic_stresses$design
        d <- DIANE::abiotic_stresses[["design"]]
      } else {
        # TODO: if there is not design ?? variable is set to NULL.
        r$use_demo = input$use_demo
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
  
  # Reactive vector of organism to chose from.
  org_choices <- shiny::reactive({
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
    
    
    choices <- c("other", choices)
    
    # Give name (genus) to pre-integrated data. We call them 'model".
    names(choices) <- c("other", rep("model", length(choices)-1))
    
    # import custom data
    custom_orgs <- names(DIANE::organisms)
    genus_custom_orgs <- c()
    # Give a name to custom orgs. Either genus, or just the name of the organism.
    for(i in custom_orgs){
      if(!is.null(DIANE::organisms[[i]][["genus"]])){
        genus_custom_orgs <- c(genus_custom_orgs, DIANE::organisms[[i]][["genus"]])
      } else {
        genus_custom_orgs <- c(genus_custom_orgs, i)
      }
    }
    names(custom_orgs) <- genus_custom_orgs
    
    choices <- c(choices, custom_orgs)
    
    # Chose organism based on url query and integrated data
    if(!is.null(r$included_genus)){
      if(all(r$included_genus %in% names(choices))){
        choices <-choices[names(choices) %in% r$included_genus]
      }
    }

    golem::print_dev(unname(choices))
    unname(choices)
  })
  
  
  output$org_selection <- shiny::renderUI({
    shiny::selectInput(
      ns("org_select"),
      label = "Your organism :",
      choices = org_choices(),
      selected = "Arabidopsis thaliana"
    )
  })
  
  shiny::observeEvent({
    input$org_select
    input$use_demo
    # r$use_demo
  },{
    req(input$use_demo)
    req(r$organism)
    if(input$org_select == "Other" & input$use_demo == TRUE){
      shinyWidgets::updateSwitchInput(session = session, inputId = "use_demo", value = FALSE)
    }
  })
  
  ## TODO : Not used anymore. Could be commented out.
  output$org_install <- shiny::renderText({
    print("output$org_install. This should not be.")
    if (!golem::get_golem_options("server_version")) {
      "<b>The organisms listed below are the one detected on the system.</b> <br>
    To use new organisms, please close DIANE and install the corresponding
    package from R or Rstudio consoles.<br>

    <code> if (!requireNamespace(\"BiocManager\", quietly = TRUE))
      install.packages(\"BiocManager\") </code> <br>

    For Human : <code> BiocManager::install(\"org.Hs.eg.db\") </code> <br>
    For Mouse : <code> BiocManager::install(\"org.Mm.eg.db\") </code> <br>
    For Caenorhabditis elegans : <code> BiocManager::install(\"org.Ce.eg.db\") </code> <br>
    For E coli : <code> BiocManager::install(\"org.EcK12.eg.db\") </code> <br>
    For fruit fly : <code> BiocManager::install(\"org.Dm.eg.db\") </code> <br>

    Then, when you launch DIANE again, your organism should appear
    in the following selection menu.

    For now, only Arabidopsis, Human and Mouse are working.
    "
    }
    else{
      "For now, you can choose between all the organisms above"
    }
  })
  
  #   ____________________________________________________________________________
  #   Custom datasets loading                                                 ####
  
  # Store selected organism (witht a high priority.)
  shiny::observe(priority = 40,{
    r$organism <- input$org_select
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
  
  # TODO : could be hidden !
  # Allow user to chose an integrated dataset.
  # TODO : Could be an UIupdate. 
  # NOTE : the req(dataset_choices()) was not there before. I had a bug without him I think, but cannot find it anymore.
  # Be carefull.
  output$dataset_selection_ui <- shiny::renderUI({
    shiny::req(input$use_demo)
    # req(dataset_choices())
    # if(!is.null(dataset_choices())){
    shiny::selectInput(
      ns("dataset_selection"),
      label = "Integrated dataset selection",
      choices = dataset_choices(), ###Will be "" if no existing dataset.
      selected = shiny::isolate(r$integrated_dataset)
    )
    # } 
    
      
    # }
  })
  
  
  # Store integrated dataset value.
  shiny::observeEvent({
    input$dataset_selection
    input$use_demo
  }, {
    if(input$use_demo){
      req(r$organism)
      req(dataset_choices())
      # browser()
      r$integrated_dataset <- input$dataset_selection
      golem::print_dev(paste0("Dataset and organism : ", r$integrated_dataset, " - ", r$organism))
    }
  })
  
  ##Print a warning when no dataset are available for selected org
  output$no_dataset_warning <- shiny::renderText({
    # shiny::req(input$use_demo, length(dataset_choices())==0)
    # shiny::req(length(dataset_choices())==0)
    if(input$use_demo && length(dataset_choices())==0){
      "<div style='color: orange'><b>Information</b> : There is no pre-integrated dataset for this organism. But you can import your own count data ! Click on on the big green button above to do so.</div><hr>"
    } else if (!input$use_demo) {
      "<div>Import your own dataset.</div><hr>"
    }
  })
  
        
  #   ____________________________________________________________________________
  #   Organism description                                                    ####
  
  ## Informations about integrated organism data.
  output$organism_description <- shiny::renderText({
    req(r$organism)
    if (r$organism %in% c(
      "Escherichia coli",
      "Drosophilia melanogaster",
      "Caenorhabditis elegans",
      "Homo sapiens",
      "Mus musculus"
    )) {
      "<p>This organism was installed using the corresponding orgdb package from bioconductor.
            You can check the specific version in the \"Software versions\" tab<p><hr>"
    } else {
      "Nothing"
      req(r$organism)
      # req(input$use_demo)
      
      organism_informations <- DIANE::organisms[[r$organism]][["informations"]]
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
        organism_description <- paste0(string, "</div><hr>")
      } else {
        organism_description <- "<p>No organism description provided<p><hr>"
      }
      organism_description
    }
    # These organism are installed via orgdb. 
    
  })
  
  
  #   ____________________________________________________________________________
  #   import user data UI                                                     ####
  
  output$count_import_ui <- shiny::renderUI({
    shiny::req(!input$use_demo)
    print("output$data_import_ui")
    shiny::tagList(
      shinyWidgets::awesomeRadio(
        ns('sep'),
        'Separator : ',
        c(
          Comma = ',',
          Semicolon = ';',
          Tab = '\t'
        ),
        inline = TRUE,
        status = "success"
      ),
      
      shiny::fileInput(
        ns('raw_data'),
        label = shiny::HTML(paste0('Choose CSV/TXT expression file',
                                   shinyWidgets::dropdownButton(
                                     size = 'xs',
                                     label = "Input file requirements",
                                     shiny::includeMarkdown(
                                       system.file("extdata", "expressionFile.md", package = "DIANE")
                                     ),
                                     circle = TRUE,
                                     status = "success",
                                     inline = TRUE,
                                     icon = shiny::icon("question"),
                                     width = "1200px",
                                     tooltip = shinyWidgets::tooltipOptions(title = "More details")
                                   )                     
        )),
        accept = c(
          'text/csv',
          'text/comma-separated-values,text/plain',
          '.csv',
          '.txt'
        )
      ),
    )
  })
  
  
  
  #   ____________________________________________________________________________
  #   gene infos upload                                                           ####
  
  output$custom_organism_ui <- shiny::renderUI({
    shiny::req(r$organism == "Other")
    shiny::tagList(
      shinyWidgets::awesomeRadio(
        ns('sep_gene_info'),
        status = "success",
        'Separator : ',
        c(Tab = '\t'),
        inline = TRUE
      ),
      
      shiny::fileInput(
        inputId = ns('gene_info_input'),
        label = HTML(paste0('Choose CSV/TXT gene information file (optional)',
                            shinyWidgets::dropdownButton(
                              size = 'xs',
                              label = "Gene information file requirements",
                              shiny::includeMarkdown(system.file("extdata", "infoFile.md",
                                                                 package = "DIANE")),
                              circle = TRUE,
                              status = "success",
                              inline = TRUE,
                              icon = shiny::icon("question"),
                              width = "1200px",
                              tooltip = shinyWidgets::tooltipOptions(title = "More details")
                            )
        )),
        accept = c(
          'text/csv',
          'text/comma-separated-values,text/plain',
          '.csv',
          '.txt'
        )
      )
      )
  })
  
  #   ____________________________________________________________________________
  #   Dataset description                                                     ####
  
  output$dataset_description <- shiny::renderText({
    req(r$organism)
    req(r$integrated_dataset)
    # req(input$use_demo)
    
    dataset_informations <- DIANE::integrated_datasets[[r$organism]][[r$integrated_dataset]][["description"]]
    dataset_description = ""
    string = "<div class='descriptive-field'"
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
        
        if (!'label' %in% colnames(d) &
            !'description' %in% colnames(d)) {
          stop("There should be a label and/or description field in the
               annotation file")
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
    if(input$use_demo){
      shiny::req(r$integrated_dataset)
    }
    raw_data()
    shiny::req(r$raw_counts)
    head(r$raw_counts)
  })
  
  ########## matrix preview
  output$heatmap_preview <- shiny::renderPlot({
    shiny::req(r$raw_counts)
    
    golem::print_dev("Print heatmap")
    d <- r$raw_counts[rowSums(r$raw_counts) > 0,]
    # d <- r$raw_counts[sample(which(rowSums(r$raw_counts) > 0), 100),]
    draw_heatmap(d, title = "Expression data preview")
  })
  
  
  
  #   ____________________________________________________________________________
  #   ValueBoxes summaries                                                    ####
  
  output$gene_ids <- shiny::renderUI({
    shiny::req(r$organism)
    # browser()
    if (r$organism == "Other")
      txt <- "No gene ID requirement"
    else if (r$organism  %in% names(DIANE::organisms))
      txt <- sample(rownames(DIANE::organisms[[r$organism]][["annotation"]]), 1)
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
    shinydashboardPlus::descriptionBlock(
      number = "Expected gene IDs are in the form",
      numberColor = "teal",
      header =  sample(txt, size = 1),
      text = paste("for", r$organism),
      rightBorder = FALSE
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
