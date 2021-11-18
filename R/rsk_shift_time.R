#' Shift rsk time stamp
#'
#' @param rskfile a path to an rsk file (RBR depth/temp logger)
#' @param shiftdays amount of time, in days, to adjust the timestamps
#'
#' @return saves modified tables to the original rsk file
#' @export
#' @description An rsk file used with RBR loggers is a specialized SQLite database.
#' Given this data structure it is possible to use R to connect to the raw data, manipulate
#' it to fix a time stamp that was recorded improperly as a result of a reset of the
#' internal clock. The function takes an assigned time shift, in days, and applies it
#' to the raw SQLite data such that the RSK file can be read in Ruskin with the correct
#' time stamp.
#' @examples
#'\dontrun{
#' rsk_shift_time("RBRraw/tow1.rsk", shiftdays = 0.5)
#' rsk_shift_time("RBRraw/tow1.rsk", shiftdays = 272)
#'}

rsk_shift_time <- function(rskfile, shiftdays){
  ########################################
  # function reads an rskfile, applies a time shift and writes
  # adjusted data back to the rsk file
  # function ADDs time so if rsk file needs to have time shifted back
  # shiftdays should be input as a negative value
  ########################################

  # check rsk file exists
  if(file.exists(rskfile)) {
    usethis::ui_done("rsk file exists")
  } else {usethis::ui_stop("rsk file does not exist. Check file path.")}

  # convert offset in days to milliseconds
  offset <- as.numeric(shiftdays)*24*60*60*1000

  # connect to rsk file
  con<-DBI::dbConnect(RSQLite::SQLite(), rskfile)

  # breaksdata table
  breaksdat<-dplyr::as_tibble(dplyr::tbl(con, "breaksdata"))
  breaksdat <- breaksdat + offset # this adjusts all 4 entries

  # fix raw data
  rawdat <- dplyr::as_tibble(dplyr::tbl(con, "data"))
  rawdat$tstamp <- rawdat$tstamp + offset

  # fix events
  eventdat <- dplyr::as_tibble(dplyr::tbl(con, "events"))
  eventdat$tstamp <- eventdat$tstamp + offset

  # fix errors table
  errordat <- dplyr::as_tibble(dplyr::tbl(con, "errors"))
  errordat$tstamp <- errordat$tstamp + offset

  # fix chart view table
  chartdat <- dplyr::as_tibble(dplyr::tbl(con, "chartView"))
  chartdat$value[chartdat$viewKey == "time.axis.range.start"] <- as.numeric(chartdat$value[chartdat$viewKey == "time.axis.range.start"]) + offset
  chartdat$value[chartdat$viewKey == "time.axis.range.end"] <- as.numeric(chartdat$value[chartdat$viewKey == "time.axis.range.end"]) + offset

  # fix region view
  regiondat <- dplyr::as_tibble(dplyr::tbl(con, "region"))
  regiondat$tstamp1 <- regiondat$tstamp1 + offset
  regiondat$tstamp2 <- regiondat$tstamp2 + offset

  # write to rsk file
  DBI::dbWriteTable(con, "breaksdata", breaksdat, overwrite = TRUE)
  DBI::dbWriteTable(con, "data", rawdat, overwrite = TRUE)
  DBI::dbWriteTable(con, "events", eventdat, overwrite = TRUE)
  DBI::dbWriteTable(con, "errors", errordat, overwrite = TRUE)
  DBI::dbWriteTable(con, "chartView", chartdat, overwrite = TRUE)
  DBI::dbWriteTable(con, "region", regiondat, overwrite = TRUE)

  # Exit
  DBI::dbDisconnect(con)
  usethis::ui_done("rsk file changed. Verify changes in Ruskin.")
}
