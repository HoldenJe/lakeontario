# do rprocval tests
library(rprocval)
library(RODBC)
library(lakeontario)
library(usethis)
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)

# Connect to raw data ----
dbase <- file.path("Data/GLIS/LOA_IA23_TW4_T5.accdb")
conn <- odbcConnectAccess2007(dbase,uid = "", pwd = "")
FN011 <- sqlFetch(conn, "FN011")
FN012 <- sqlFetch(conn, "FN012")
FN022 <- sqlFetch(conn, "FN022")
FN026 <- sqlFetch(conn, "FN026")
FN026_Subspace <- sqlFetch(conn, "FN026_Subspace")
FN028 <- sqlFetch(conn, "FN028")
FN121 <- sqlFetch(conn, "FN121")
FN123 <- sqlFetch(conn, "FN123")
FN124 <- sqlFetch(conn, "FN124")
FN125 <- sqlFetch(conn, "FN125")
odbcClose(conn)


fn121_column_check_t5(FN121)
fn123_column_check(FN123)
fn121_error_queries_t5(FN121, FN123)
fn123_error_queries(FN123, FN125)
fn_template5_basic_checks(FN012, FN022, FN026_Subspace, FN028, FN121, FN123, FN124, FN125)
fn124_minmax <- fn124_minmax_TLEN(FN124, FN012)
fulton_errors <- fn125_fulton(FN125, FN012)
lengtherrors125 <- fn125_minmax_TLEN(FN125, FN012)

FN125_errors <- FN125 %>%
  split(.$SPC) %>%
  map(fn125_tl_rwt_lm, makeplot = T) %>%
  bind_rows()

# Check for points that aren't in LO
FN121_spatial <- st_as_sf(FN121, coords = c("DD_LON0", "DD_LAT0"), crs = 4326)
spatial_test <- st_join(FN121_spatial, shape_ontarioshore, join = st_covered_by, left = T)
spatial_errors <- spatial_test %>% filter(is.na(PERIMETER))
if(nrow(spatial_errors >0)){
  usethis::ui_oops("Points exist outside the Lake Ontario boundary")
  spatial_errors
} else {
  usethis::ui_info("All points are within Lake Ontario boundary")
  base_ontarioshore + geom_sf(data = FN121_spatial)
}
