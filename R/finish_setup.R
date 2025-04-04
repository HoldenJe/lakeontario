#' Finish lakeontario project setup
#' @description Several of the analysis procedures rely on custom functions. Rather than a
#' vignette approach, this function copies template analysis scripts that establish a workflow
#' based on the existing package functions. The template files are "auto-magically" created
#' following the project naming procedures and directories.
#' @returns NULL
#' @export
#' @examples
#' \dontrun{
#' finish_setup()
#' }

finish_setup <- function(){
  # create template files
  usethis::use_template(
    template = "pointblank_FEED_validation.R",
    save_as = file.path("Reports/1_FEED_pointblank_error_check_routine.R"),
    package = "lakeontario"
  )

  usethis::use_template(
    template = "multiagent.Rmd",
    save_as = file.path("Reports/2_FEED_pointblank_error_check_report.Rmd"),
    package = "lakeontario"
  )

  usethis::use_template(
    template = "rprocval_gl5_checks.Rmd",
    save_as = file.path("Reports/3_error_check_report.Rmd"),
    package = "lakeontario"
  )

  usethis::use_template(
    template = "bottom_pick.R",
    save_as = file.path("Analysis/1_RBR_bottom_pick.R"),
    package = "lakeontario"
  )

  usethis::use_template(
    template = "FEED2GLIS.R",
    save_as = file.path("Analysis/2_FEED_to_GLIS_T5.R"),
    package = "lakeontario"
  )

  usethis::use_template(
    template = "rprocval_gl5_checks.R",
    save_as = file.path("Analysis/3_rprocval_checks_GLIS_T5.R"),
    package = "lakeontario"
  )

  usethis::use_template(
    template = "FEEDMonitor.R",
    save_as = file.path("Data/FEEDMonitorApp.R"),
    package = "lakeontario"
  )

  rmarkdown::render("Instructions.md", quiet = TRUE)
  rmarkdown::render("README.md", quiet = TRUE)

  usethis::ui_done("Project setup complete. You can delete FinishSetup.R")
  usethis::ui_info("Please read Instructions.md and update README.md")
  usethis::ui_info("lakeontario project template completed. ><(((º>")
}
