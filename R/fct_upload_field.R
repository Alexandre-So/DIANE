#' Upload field shared by the import modules
#'
#' Groups a file input and its separator choice in a single block, with the
#' help panel used everywhere else in DIANE : a markdown file from
#' `inst/extdata` shown in a `shinyWidgets::dropdownButton()`.
#' Styling lives in `inst/app/www/upload-field.css`, bundled by `app_ui()`.
#'
#' @param file_id Id of the `fileInput()`, already namespaced by the caller.
#' @param label Title of the block.
#' @param separator_id Id of the separator `radioButtons()`, or `NULL` for
#'   files that have a single column.
#' @param help Short sentence shown under the title.
#' @param help_md Name of a markdown file in `inst/extdata` to show in the
#'   help panel, or `NULL` for no help button.
#' @param help_width Width of the help panel, wide enough for the tables some
#'   of the markdown files contain.
#' @param help_opens Side the help panel opens towards : `"left"` for a box in
#'   the right column.
#' @param hint Short note shown under the help text, an example value most
#'   of the time. A string is rendered as is, a `uiOutput()` as given.
#' @param accept Extensions offered by the file browser.
#' @param separator Separator selected by default.
#' @param example_download_id Id of a `downloadHandler()` serving an example
#'   file, or `NULL`.
#' @param file_details Extra note shown at the bottom of the block.
#'
#' @noRd
upload_field <- function(file_id,
                         label,
                         separator_id = NULL,
                         help = NULL,
                         help_md = NULL,
                         help_width = "600px",
                         help_opens = c("right", "left"),
                         hint = NULL,
                         accept = c(".csv", ".tsv", ".txt"),
                         separator = "\t",
                         example_download_id = NULL,
                         file_details = NULL) {
  help_opens <- match.arg(help_opens)
  stopifnot(
    is.character(file_id), length(file_id) == 1L,
    is.character(label), length(label) == 1L,
    is.null(separator_id) ||
      (is.character(separator_id) && length(separator_id) == 1L &&
         !identical(separator_id, file_id)),
    length(separator) == 1L, separator %in% c(",", ";", "\t"),
    is.character(help_width), length(help_width) == 1L
  )

  ### The title labels the section, rather than repeating it in an aria-label.
  title_id <- paste0(file_id, "_uf_title")

  help_button <- if (!is.null(help_md)) {
    help_path <- system.file("extdata", help_md, package = "DIANE")
    if (help_path == "")
      stop("upload_field(): no file '", help_md, "' in inst/extdata.")
    shinyWidgets::dropdownButton(
      shiny::includeMarkdown(help_path),
      size = "xs",
      circle = TRUE,
      status = "success",
      ### right anchors the panel, so anchoring right makes it open left.
      right = identical(help_opens, "left"),
      inline = TRUE,
      icon = shiny::icon("question"),
      width = help_width,
      tooltip = shinyWidgets::tooltipOptions(title = "More details")
    )
  }

  hint_tag <- if (is.character(hint)) {
    shiny::tags$p(class = "uf-hint", hint)
  } else {
    hint ### uiOutput() for a value the caller computes itself.
  }

  shiny::tags$section(
    class = "upload-field",
    `aria-labelledby` = title_id,

    shiny::tags$div(
      class = "uf-heading",
      shiny::tags$h4(id = title_id, label),
      help_button
    ),

    if (!is.null(help)) shiny::tags$p(class = "uf-help", help),

    hint_tag,

    shiny::tags$div(
      class = "uf-file",
      shiny::fileInput(
        inputId = file_id,
        label = label, # Kept for assistive technology; visually hidden in CSS.
        accept = accept,
        width = "100%"
      )
    ),

    if (!is.null(separator_id))
      shiny::tags$div(
        class = "uf-separator",
        shiny::radioButtons(
          inputId = separator_id,
          label = "Separator",
          choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
          selected = separator,
          inline = TRUE,
          width = "100%"
        )
      ),

    if (!is.null(example_download_id) || !is.null(file_details))
      shiny::tags$div(
        class = "uf-extra",
        if (!is.null(example_download_id))
          shiny::downloadLink(example_download_id, "Download an example file"),
        if (!is.null(file_details)) shiny::tags$span(file_details)
      )
  )
}
