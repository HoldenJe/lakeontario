#' Pick bottom contact time
#'
#' @param onetow expects an RBR temperature profiles
#'
#' @return dataframe with start, end and measured bottom contact time (in seconds) for each tow provided
#' @export
#' @description
#' A function that first runs `rbr_tow_plot()` then provides a cursor for the user to identify the start and end of where
#' the trawl contacted bottom (or in the case of a midwater trawl, reached the target fishing depth).
#'
#' @seealso [rbr_tow_plot()]
#' @examples\dontrun{
#' library(lakeontario)
#' rbrfiles <- dir("Data/RBRClean", full.names = T)
#' towstats <- import_merge_tow_files(rbrfiles)
#' head(towstats)
#' str(towstats)
#' tow_stat_list <- split(towstats, towstats$SERIAL)
#' lapply(onetow, pick_bottom_contact)
#' }
#'
pick_bottom_contact <- function(onetow){
  if(length(unique(onetow$SERIAL))!=1){stop("More than one SERIAL in data")}
  rbr_tow_plot(onetow)
  print("Pick start and end time on the plot")
  pt <- identify(onetow$TIME, onetow$FOOT_DEPTH, n=2)
  touchdown <- onetow$TIME[pt[1]]
  liftoff <- onetow$TIME[pt[2]]
  df <- data.frame(YEAR = unique(onetow$YEAR),
                   VESSEL = unique(onetow$VESSEL),
                   SERIAL = unique(onetow$SERIAL),
                   ON_BOTTOM = touchdown,
                   OFF_BOTTOM = liftoff,
                   ON_BOTTOM_DURATION_SEC = as.numeric(difftime(liftoff, touchdown, units = "sec")))
  df
}
