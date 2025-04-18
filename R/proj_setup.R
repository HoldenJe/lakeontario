#' proj_setup
#' @description
#' A project setup function that is used for new RStudio project template
#'
#' @param path file path that is populated during the setup procedure
#' @param ... additional arguments as required
#'
#' @export
#'
proj_setup <- function(path, ...) {

  # Create directories
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0(path, "/Data"), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0(path, "/Data/Raw"), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0(path, "/Data/Processed"), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0(path, "/Data/RBRClean"), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0(path, "/Data/RBRRaw"), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0(path, "/Analysis"), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0(path, "/Documentation"), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0(path, "/Reports"), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0(path, "/Figures"), recursive = TRUE, showWarnings = FALSE)

  # create some header text
  header <- c(
    "# LOMU FEED Trawl Project",
    "*project template generated by* `lakeontario`"
  )

  FolderDescrip <- c(
    "File | Description",
    "---------- | --------------------------------------------------",
    "Data | contains the project data in subdirectories",
    "Data/Raw | Active FEED database should be stored here",
    "Data/Processed | Data that has been cleaned and mapped to the template data base",
    "Data/RBRRaw | RBR raw profiles data",
    "Data/RBRClean | RBR files that have been parsed to individual tow files",
    "Analysis | Analysis and data scrubbing scripts",
    "Documentation | Project documentation files such as survey protocols and data management plan",
    "Reports | Markdown report files or other file formats used to report on the project",
    "Figures | Saved figures to be uploaded to Project Tracker or for inclusion in Rmarkdown reports",
    "<hr />",
    "<br />"
  )

  instructions <- c(
    "---",
    "title: Instructions",
    "output: html_document",
    "---",
    "# Binational Trawl Project Template Intructions  ",
    "<!-- This file is best viewed using the RStudio previewer -->",
    "## Standard Templates  ",
    "Standard templates have been pre-loaded to the existing directories.",
    "  ",
    "## Generalized Workflow  ",
    "1. Add FEED database to Data/Raw  ",
    "2. Run Reports/1_FEED_pointblank_error_check_routine.R daily to check for errors or missing data in FEED.  ",
    "3. Reports/2_FEED_pointblank_error_check_report.Rmd can be used to create a stand alone HTML error report.  ",
    "4. At the completion of the survey run Analysis/1_RBR_bottom_pick.R to create tow mensuration files.  ",
    "5. When FEED data is clean, migrate to GLIS database template using Analysis/2_FEED_to_GLIS_T5.R  ",
    "6. Analysis/3_rprocval_checks_GLIS_T5.R can then be used prior to running GLIS procval to inspect the data quality  ",
    "7. Additionally, Reports/3_error_check_report.Rmd can be used to create a HTML report  ",
    "8. Data/FEEDMonitor.R is a Shiny app that can be ran alongside FEED to monitor otolith and bio samples.  ",
    "  ",
    "## Folder Description and Contents",
    FolderDescrip,
    "  ",
    "  ",
    "<hr />",
    "<br />",
    '<p style="text-align: center;"> *This file was created auto-magically by `lakeontario`*</p>'
  )

  writeLines(instructions, con = file.path(path, "Instructions.md"))

  # collect inputs and paste together as 'Parameter: Value'
  dots <- list(...)
  text <- lapply(seq_along(dots), function(i) {
    key <- names(dots)[[i]]
    val <- dots[[i]]
    paste0(key, ": ", val, "<br />")
  })

  prj_params <- lapply(seq_along(dots), function(i) {
    key <- names(dots)[[i]]
    val <- dots[[i]]
    val
  })

  names(prj_params) <- c("PRJ_LDR", "PRJ_CD", "GLMU", "GR", "PRJ_DATE0", "PRJ_DATE1")

  # collect into single text string
  mdyaml <- c(
    "---",
    "title: README",
    "output: html_document",
    "---"
  )
  contents <- paste(
    paste(header, collapse = "\n"),
    paste(text, collapse = "\n"),
    sep = "  \n"
  )

  abstract <- c(
    "# Abstract",
    " ",
    "Please provide a simple description of the project"
  )

  finishproject <- c(
    "# Run the finish_setup() function to complete project setup",
    " ",
    "lakeontario::finish_setup()",
    " ",
    "# this file can be deleted once sourced"
  )

  # write to index file
  readme <- c(mdyaml, contents, " ", abstract, " ", "# Folder Description", " ", FolderDescrip)
  writeLines(readme, con = file.path(path, "README.md"))
  save(prj_params, file = file.path(path, "params.RData"))
  writeLines(finishproject, con = file.path(path, "FinishSetup.R"))

}
