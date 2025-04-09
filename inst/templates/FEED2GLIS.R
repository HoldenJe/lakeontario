# map FEED/RVCAT data to GLIS
# This script is intended to map FEED data to GLIS template database
# Jeremy Holden
# Apr 20, 2024

# INFO ----
# This script is best ran as background job
# some tables will take a long time to write
# The current run time of this script exceeds 10min

# Warning ----
# This script expects the output database to be disposable
# A side effect of running this script is that the output database
# will be erased and recreated.
# If you've been foolish enough to make data edits in the output database
# all those will be gone. Data should be edited in the FEED database.

# load libraries ----
options(warn=-1)
library(RODBC)
suppressPackageStartupMessages(library(dplyr))
library(lakeontario)
suppressPackageStartupMessages(library(lubridate))
library(usethis)
library(tidyr)
library(here)
here()
options(warn=0)

# 32-bit test ----
testR <- Sys.getenv("R_ARCH") == "/i386"
if(testR){
  ui_info("R is running in 32-bit mode")
} else {ui_stop("Access requires a 32-bit connection")}

# script messages ----
ui_info("Starting FEED2GLIS wrapper.")
ui_warn("Your GLIS database is going to be deleted!")

# Define project code ----
load("params.RData") # loads as `prj_params`
load("towstats.Rdata") # btr_mens...
fulton <- read.csv("Data/FultonK_values.csv")
prj_cd <- prj_params$PRJ_CD

# Connect to raw data ----
dbase <- file.path("Data/Raw/OntarioExplorerSBT23.mdb")
ui_info(paste0("Reading data from ", dbase))
conn <- odbcConnectAccess(dbase,uid = "", pwd = "")
#op <- sqlFetch(conn, "Calc_RVCAT OP")
op2 <- sqlFetch(conn, "OP_2Serial")
op3 <- sqlFetch(conn, "OP_3Begin")
op4 <- sqlFetch(conn, "OP_4End")
op5 <- sqlFetch(conn, "OP_5Trawl_OP")
op6 <- sqlFetch(conn, "Calc_RVCAT TR_OP")
codes_spc <- sqlFetch(conn, "Codes_Species")
codes_ports <- sqlFetch(conn, "Codes_Ports")
codes_location <- sqlFetch(conn, "Codes_Location")
tr_catch <- sqlFetch(conn, "Calc_RVCAT TR_CATCH")
tr_fish <- sqlFetch(conn, "TR_7Fish")
tr_lf <- sqlFetch(conn, "Calc_RVCAT TR_LF")
odbcClose(conn)

ui_info("Data successfully loaded.")

# remove failed tows from op2
op2 <- op2 %>% filter(Target != 999)

# remove non-fish from tr_catch - FN123 only accepts fish entries
rvcat123 <- tr_catch %>%
  filter(!SPECIES %in% c(952, 0, 940, 999))
rvcat123_nonfish <- tr_catch %>%
  filter(SPECIES %in% c(952))

rvcat123 <- semi_join(rvcat123, op2, by = c("SERIAL"= "Serial", "OP_Year", "VESSEL"="Vessel"))
rvcat123_nonfish <- semi_join(rvcat123_nonfish, op2, by = c("SERIAL"= "Serial", "OP_Year", "VESSEL"="Vessel"))


# add SPC codes
spc <- inner_join(codes_spc, RV2FN_SPECIES, by = c("SpeciesCode" = "SPECIES"))
rvcat123 <- left_join(rvcat123, RV2FN_SPECIES, by = c("SPECIES" = "SPECIES"))
rvcat125 <- left_join(tr_fish, RV2FN_SPECIES, by = c("Species" = "SPECIES"))

# Connect to template database ----
# get table names for `select`-ing

dbase_template <- "Data/Processed/Great_Lakes_Assessment_Template_5.accdb"
ui_info(paste0("Getting headers from ", dbase_template))

conn_template <- odbcConnectAccess2007(dbase_template, uid = "", pwd = "")
fn011_names <- sqlColumns(conn_template, "FN011")$COLUMN_NAME
fn012_names <- sqlColumns(conn_template, "FN012")$COLUMN_NAME
fn022_names <- sqlColumns(conn_template, "FN022")$COLUMN_NAME
fn026_names <- sqlColumns(conn_template, "FN026")$COLUMN_NAME
fn026_sub_names <- sqlColumns(conn_template, "FN026_Subspace")$COLUMN_NAME
fn028_names <- sqlColumns(conn_template, "FN028")$COLUMN_NAME
fn121_names <- sqlColumns(conn_template, "FN121")$COLUMN_NAME
fn122_names <- sqlColumns(conn_template, "FN122")$COLUMN_NAME
fn123_names <- sqlColumns(conn_template, "FN123")$COLUMN_NAME
fn123nonfish_names <- sqlColumns(conn_template, "FN123_NonFish")$COLUMN_NAME
fn124_names <- sqlColumns(conn_template, "FN124")$COLUMN_NAME
fn125_names <- sqlColumns(conn_template, "FN125")$COLUMN_NAME
fn125lamprey_names <- sqlColumns(conn_template, "FN125_lamprey")$COLUMN_NAME
odbcClose(conn_template)

## create FN tables
ui_info("Creating FN tables from FEED database.")

# fn011 ----
fn011 <- data.frame(
  YEAR = unique(op2$OP_Year),
  PRJ_CD = prj_cd,
  PRJ_NM = "Lake Ontario Binational Spring Prey Fish Assessment",
  PRJ_LDR = prj_params$PRJ_LDR,
  # PRJ_DATE0 = as.character(ymd(min(op$OP_DATE))),
  # PRJ_DATE1 = as.character(ymd(max(op$OP_DATE))),
  PRJ_DATE0 = format(ymd(prj_params$PRJ_DATE0), format = "%Y-%m-%d"),
  PRJ_DATE1 = format(ymd(prj_params$PRJ_DATE1), format = "%Y-%m-%d"),
  COMMENT0 = "Binational spring trawling program that targets Alewife",
  PROTOCOL = prj_params$GR,
  LAKE = "ON")

if(!all(fn011_names == names(fn011))){
  ui_warn("Names don't match template.")
} else {ui_done("Proceed: Names in FN021 match template")}

# fn012 ----
# FEED has a table similar to this
#head(codes_spc)
#head(RV2FN_SPECIES)
# Needs to have SPECIES converted to SPC
spc_caught <- unique(tr_catch$SPECIES)
spc <- spc %>% filter(SpeciesCode %in% spc_caught)

spc <- left_join(spc, fulton, by = c("SpeciesCode" = "SPECIES"))

spc_live <- spc %>%
  mutate(
    PRJ_CD = prj_cd,
    GRP = "06",
    GRP_DES = "Lifestage not determined",
    SIZSAM = "3",
    SIZATT = "TLEN",
    SIZINT = "1",
    BIOSAM = "3",
    FDSAM = "0",
    SPCMRK = "0",
    TISSUE = "0",
    AGEST = "0",
    LAMSAM = "0",
    FLEN_MIN = MinLength*0.9,
    FLEN_MAX = MaxLength*0.9,
    RWT_MIN = exp(Slope*log(MinLength)+Intercept),
    RWT_MAX = exp(Slope*log(MaxLength)+Intercept),
    K_MIN_ERROR = K_min_error,
    K_MIN_WARN = k_min_warn,
    K_MAX_WARN = k_max_warn,
    K_MAX_ERROR = K_max_error,
    SPC_NMCO = NA
  ) %>%
  rename(
    # SPC_NMCO = CommonName,
    TLEN_MIN = MinLength,
    TLEN_MAX = MaxLength
  )

spc_dead <- spc %>%
  mutate(
    PRJ_CD = prj_cd,
    GRP = "14",
    GRP_DES = "Dead",
    SIZSAM = "3",
    SIZATT = "TLEN",
    SIZINT = "1",
    BIOSAM = case_when(
      SPC == "081" ~ "3",
      .default = "0"
    ),
    FDSAM = "0",
    SPCMRK = "0",
    TISSUE = "0",
    AGEST = "0",
    LAMSAM = "0",
    FLEN_MIN = MinLength*0.9,
    FLEN_MAX = MaxLength*0.9,
    RWT_MIN = exp(Slope*log(MinLength)+Intercept),
    RWT_MAX = exp(Slope*log(MaxLength)+Intercept),
    K_MIN_ERROR = K_min_error,
    K_MIN_WARN = k_min_warn,
    K_MAX_WARN = k_max_warn,
    K_MAX_ERROR = K_max_error,
    SPC_NMCO = NA
  ) %>%
  rename(
    # SPC_NMCO = CommonName,
    TLEN_MIN = MinLength,
    TLEN_MAX = MaxLength
  )

spc_live <- spc_live %>%
  mutate(FDSAM = ifelse(SPC %in% c("091", "081", "334"), "2", "0"),
         SPCMRK = ifelse(SPC %in% c("081"), 12, "0"),
         TISSUE = case_when(
           SPC %in% c("093", "094") ~ "AD",
           SPC %in% c("091", "081") ~ "D",
           .default = "0"
         ),
         AGEST = ifelse(SPC %in% c("061", "091", "081", "121", "291", "384", "331", "334"), "A", "0"),
         LAMSAM = ifelse(SPC %in% c("091", "093", "081", "077", "075", "078", "076", "073"), "2", "0")
  )

spc_dead <- spc_dead %>%
  mutate(LAMSAM = ifelse(SPC == "081", "2", "0"))

allspc <- bind_rows(spc_live, spc_dead) %>%
  mutate(LIFE_STAGE = as.numeric(GRP))

spc_life_caught <- tr_catch %>% group_by(SPECIES, LIFE_STAGE) %>%
  summarise()

allspc <- semi_join(allspc, spc_life_caught, by = c("SpeciesCode" = "SPECIES", "LIFE_STAGE"))

fn012 <- allspc %>% select(all_of(fn012_names))

if(!all(fn012_names == names(fn012))){ui_warn("Names don't match template.")
} else {ui_done("Proceed: Names in FN012 match template")}


# fn022 ----
fn022 <- data.frame (
  PRJ_CD = prj_cd,
  SSN = "00",
  SSN_DES = "Early Spring",
  SSN_DATE0 = format(dmy(prj_params$PRJ_DATE0), format = "%Y-%m-%d"),
  SSN_DATE1 = format(dmy(prj_params$PRJ_DATE1), format = "%Y-%m-%d")
)

if(!all(fn022_names == names(fn022))){ui_warn("Names don't match template.")
} else {ui_done("Proceed: Names in FN022 match template")}


# fn026 ----
# map this to Port Table
# sqlColumns(conn, "FN026")$COLUMN_NAME
# head(codes_ports)
# head(codes_location)
summary_ports <- codes_location %>% group_by(PORT) %>%
  summarize(DD_LAT = mean(LATITUDE, na.rm = T),
            DD_LON = mean(LONGITUDE, na.rm = T),
            # SIDEP_GE = min(floor(ApproxDepth/10)*10), # these generate 0s which generate errors
            # SIDEP_LT = max(ceiling(ApproxDepth/10)*10)
            SIDEP_GE = min(ApproxDepth, na.rm = T),
            SIDEP_LT = max(ApproxDepth, na.rm = T)
  )

fn026 <- left_join(codes_ports, summary_ports, by = c("PortCode" = "PORT"))

fn026 <- fn026 %>%
  rename(SPACE = PortCode,
         SPACE_DES = Description) %>%
  mutate(
    PRJ_CD = prj_cd,
    GRDEP_GE = SIDEP_GE,
    GRDEP_LT = SIDEP_LT,
    SPACE_WT = NA
  )

fn026 <- fn026 %>% select(all_of(fn026_names))

### this is a hack to be able to shrink PORT to 2 digits
fn026 <- fn026 %>%
  mutate(SPACE = substr(SPACE, 2,3))

if(!all(fn026_names == names(fn026))){ui_warn("Names don't match template.")
} else {ui_done("Proceed: Names in FN026 match template")}


# fn026_subspace ----
# map this to Location Table
# head(codes_location)

fn026_sub <- codes_location %>%
  rename(SPACE = PORT,
         SUBSPACE = LocationCode,
         SUBSPACE_DES = Description,
         DD_LAT = LATITUDE,
         DD_LON = LONGITUDE
  ) %>%
  mutate(
    PRJ_CD = prj_cd,
    SIDEP_GE = ApproxDepth - 5,
    SIDEP_LT = ApproxDepth + 5,
    GRDEP_GE = ApproxDepth - 5,
    GRDEP_LT = ApproxDepth + 5,
    SUBSPACE_WT = NA
  )

locations_used <- unique(op2$Location)
fn026_sub <- fn026_sub %>% filter(SUBSPACE %in% locations_used)
### this is a hack to be able to shrink PORT to 2 digits
fn026_sub <- fn026_sub %>%
  mutate(SPACE = substr(SPACE, 2,3))

## another hack, remove PORT == (6)33 sites since (6)33 [Presquile is not in the PORT Table right now - add to FEED master template]
fn026_sub <- fn026_sub %>%
  filter(SPACE != "33")

fn026_sub <- fn026_sub %>% select(all_of(fn026_sub_names))

if(!all(fn026_sub_names == names(fn026_sub))){ui_warn("Names don't match template.")
} else {ui_done("Proceed: Names in FN026_sub match template")}

# fn028 ----

fn028 <- data.frame(
  PRJ_CD = prj_cd,
  MODE = "00",
  MODE_DES = "NA",
  GR = "TW69",
  GRUSE = "1",
  ORIENT = "3", # check this code - 57C projects use 3
  EFFDUR_GE = "2",
  EFFDUR_LT = "10",
  EFFTM0_GE = "07:00:00",
  EFFTM0_LT = "20:00:00"
)
if(!all(fn028_names == names(fn028))){ui_warn("Names don't match template.")
} else {ui_done("Proceed: Names in FN028 match template")}


# fn121 ----
rvcat121 <- inner_join(op2, op3, by=c("Serial"))
rvcat121 <- inner_join(rvcat121, op4, by = c("Serial"))
rvcat121 <- inner_join(rvcat121, op5, by = c("Serial"))
rvcat121 <- rvcat121 %>% filter(Target == 690)
rvcat121 <- left_join(rvcat121, btr_mensuration, by = c("OP_Year" = "YEAR", "Serial" = "SERIAL"))
rvcat121 <- left_join(rvcat121, op6, by = c("Serial" = "SERIAL"))

# fn121 also needs to be merged with the trawl mensuration data once completed to fill in a few fields - see below
# head(rvcat121)

fn121 <- rvcat121 %>%
  mutate(
    PRJ_CD = prj_cd,
    SAM = as.character(Serial),
    PROCESS_TYPE = "1",
    SSN = "00",
    MODE = "00",
    EFFDT0 = format(as.POSIXct(OP_Date), format = "%Y-%m-%d"),
    EFFDT1 = format(as.POSIXct(End_Date), format = "%Y-%m-%d"),
    EFFTM0 = format(as.POSIXct(Beg_ESTime), format = "%H:%M:%S"),
    EFFTM1 = format(as.POSIXct(End_ESTime), format = "%H:%M:%S"),
    EFFDUR = TOW_TIME1/60,
    EFFST = "1",
    DD_LAT0 = feed_convert_coords(Beg_Latitude),
    DD_LON0 = -1*feed_convert_coords(Beg_Longitude),
    DD_LAT1 = feed_convert_coords(End_Latitude),
    DD_LON1 = -1*feed_convert_coords(End_Longitude),
    GRID5 = NA,
    SITEM1 = NA,
    GRDEPMAX = foot_max_depth, # need this from mensuration data
    GRDEPMID = NA, # Do not use
    GRDEPMIN = foot_min_depth, # need this from mensuration data
    SECCHI0 = NA,
    SECCHI1 = NA,
    SLIME = NA,
    CREW = NA,
    VESSEL = "ONEXPLORER",
    VESSEL_DIRECTION = NA, # could be heading,
    BOTTOM = NA,
    COVER = NA,
    LEAD_ANGLE = NA,
    LEADUSE = NA,
    DISTOFF = NA,
    VEGETATION = NA,
    O2BOT0 = NA,
    O2BOT1 = NA,
    O2SURF0 = NA,
    O2SURF1 = NA,
    O2GR0 = NA,
    O2GR1 = NA,
    AIRTEM0 = NA,
    AIRTEM1 = NA,
    WIND0 = NA, # could get this but GLIS format is messed up
    WIND1 = NA,
    PRECIP0 = NA,
    PRECIP1 = NA,
    CLOUD_PC0 = NA,
    CLOUD_PC1 = NA,
    WAVEHT0 = NA,
    WAVEHT1 = NA,
    XWEATHER = NA # GLIS codes likely don't match RVCAT
  ) %>%
  rename(
    SUBSPACE = Location,
    SITP = BottomType,
    SITEM0 = Surf_Temp,
    SIDEP0 = Beg_Depth,
    SIDEP1 = End_Depth,
    COMMENT1 = Remark,
    VESSEL_SPEED = Speed,
    WARP = Warp
  )

fn121 <- fn121 %>% select(all_of(fn121_names))
if(!all(fn121_names == names(fn121))){ui_warn("Names don't match template.")
} else {ui_done("Proceed: Names in FN121 match template")}

# fn122 ----
is_waterhaul <- anti_join(rvcat121, rvcat123, by = c("OP_Year", "Vessel" = "VESSEL", "Serial" = "SERIAL"))

fn122 <- rvcat121 %>%
  mutate(
    PRJ_CD = prj_cd,
    SAM = as.character(Serial),
    EFF = "001",
    EFFDST = Speed*TOW_TIME1/60 * 1.852 * 1000, # tow time * vessel speed in knots * convert to km * convert to m
    GRTEM1 = NA,
    WATERHAUL = ifelse(Serial %in% is_waterhaul$Serial, T, F),
    COMMENT2 = NA
  ) %>%
  rename(
    GRDEP0 = Beg_Depth,
    GRDEP1 = End_Depth,
    GRTEM0 = Fishing_Temp
  )

fn122 <- fn122 %>% select(all_of(fn122_names))
if(!all(fn122_names == names(fn122))){ui_warn("Names don't match template.")
} else {ui_done("Proceed: Names in FN122 match template")}


# fn123 ----
biosams <- tr_fish %>%
  group_by(Serial, Species, LifeStage) %>%
  summarize(BIOCNT = n())

rvcat123 <- left_join(rvcat123, biosams,
                      by =c("SERIAL" = "Serial", "SPECIES" = "Species", "LIFE_STAGE" = "LifeStage"))

fn123 <- rvcat123 %>%
  mutate(
    PRJ_CD = prj_cd,
    SAM = as.character(SERIAL),
    EFF = "001",
    GRP = sprintf("%02d", LIFE_STAGE),
    BIOCNT = ifelse(is.na(BIOCNT), 0, BIOCNT),
    CATWT = WEIGHT/1000,
    SUBCNT = NA,
    SUBWT = NA,
    COMMENT3 = NA
  ) %>%
  rename(
    CATCNT = N
  )

fn123 <- fn123 %>% select(all_of(fn123_names))
if(!all(fn123_names == names(fn123))){ui_warn("Names don't match template.")
} else {ui_done("Proceed: Names in FN123 match template")}

# fn123_nonfish ----

fn123nonfish <- rvcat123_nonfish %>%
  filter(SPECIES == 952) %>% # don't think there is a generic vegetation code... so ignore
  mutate(
    PRJ_CD = prj_cd,
    SAM = as.character(SERIAL),
    EFF = "001",
    CATCNT = NA,
    MORTCNT = NA,
    COMMENT3 = NA,
    TAXON = case_when(
      SPECIES == 952 ~ "81338"
    )
  )

fn123nonfish <- fn123nonfish %>% select(all_of(fn123nonfish_names))
if(!all(fn123nonfish_names == names(fn123nonfish))){ui_warn("Names don't match template.")
} else {ui_done("Proceed: Names in FN123 match template")}

# fn124 ----
rvcat124 <- left_join(tr_lf, RV2FN_SPECIES, by = c("Species" = "SPECIES"))
rvcat124 <- semi_join(rvcat124, op2, by = c('OP_Year', 'Vessel', 'Serial'))

fn124 <- rvcat124 %>%
  mutate(
    PRJ_CD = prj_cd,
    SAM = as.character(Serial),
    EFF = "001",
    GRP = sprintf("%02d", Lifestage),
    SIZ = as.character(Length),
    COMMENT4 = NA
  ) %>%
  rename(
    SIZCNT = N
  )
fn124 <- fn124 %>% select(all_of(fn124_names))
if(!all(fn124_names == names(fn124))){ui_warn("Names don't match template.")
} else {ui_done("Proceed: Names in FN124 match template")}


# fn125 ----
rvcat125 <- semi_join(rvcat125, op2, by = c('Serial'))
fn125 <- rvcat125 %>%
  mutate(
    PRJ_CD = prj_cd,
    SAM = as.character(Serial),
    EFF = "001",
    GRP = sprintf("%02d", LifeStage),
    FLEN = NA,
    GIRTH = NA,
    EVISWT = NA,
    SEX = case_when(
      Sex == 0 ~ as.character("9"),
      .default = as.character(Sex)
    ),
    MAT = NA,
    GON = NA,
    GONWT = NA,
    CLIPC = as.character(case_when(
      Fin_Clip == 0 ~ "0",
      Fin_Clip == 2 ~ "45",
      Fin_Clip == 6 ~ "15"
    )), # this should be done in FEED db in Codes_FinClip
    CLIPA = NA,
    NODC = NA,
    NODA = NA,
    TISSUE = case_when(
      SPC == "093" ~ "AD",
      Fin_Tissue == "Y" ~ "D",
      .default = "0"
    ), # need to translate
    AGEST = case_when(
      AgeStructureTaken == 2 ~ "A",
      AgeStructureTaken == 1 ~ "2"
    ), # this should be done in FEED db in Codes_AgeStructure
    FATE = "K",
    FDSAM = "0",
    STOM_CONTENTS_WT = NA,
    COMMENT5 = ""
  ) %>%
  rename(
    FISH = Fish_Number,
    TLEN = Length,
    RWT = Weight
  )

fn125 <- fn125 %>% select(all_of(fn125_names))
if(!all(fn125_names == names(fn125))){ui_warn("Names don't match template.")
} else {ui_done("Proceed: Names in FN125 match template")}


# fn125_lamprey ----
rvcathas_lam <- rvcat125 %>%
  filter(WoundsYN == "Y") %>%
  select(Serial, SPC, LifeStage, Fish_Number, A1:B4) %>%
  pivot_longer(A1:B4, names_to = "LAMIJC_TYPE") %>%
  filter(!is.na(value)) %>%
  mutate(LAMID = "1") %>%  # this is a one-time fix. LAMID rules say start at 1 for each fish
  select(-value)

rvcat_marks <- rvcat125 %>%
  filter(WoundsYN == "Y") %>%
  select(Serial, SPC, LifeStage, Fish_Number) %>%
  mutate(value = "0") %>%
  rename(LAMIJC_TYPE = value) %>%
  mutate(LAMID = "1")

# do update query to change the 1 LT value to B4 (from rvcathas_lam)
rvcatlam <- rows_update(rvcat_marks, rvcathas_lam,
                        by = c("Serial", "SPC", "LifeStage", "Fish_Number", "LAMID"))

fn125_lam <- rvcatlam %>%
  mutate(
    PRJ_CD = prj_cd,
    SAM = as.character(Serial),
    EFF = "001",
    GRP = sprintf("%02d", LifeStage),
    XLAM = NA,
    LAMIJC_SIZE = NA,
    COMMENT_LAM = NA
  ) %>%
  rename(
    FISH = Fish_Number
  )


fn125_lam <- fn125_lam %>% select(all_of(fn125lamprey_names))
if(!all(fn125lamprey_names == names(fn125_lam))){ui_warn("Names don't match template.")
} else {ui_done("Proceed: Names in FN125 match template")}

# Gear ----
Gear_Eff_Process <- data.frame(
  GR = "TW69",
  EFF = "001",
  PROCESS_TYPE = "1",
  EFFDST = NA
)
#
# # Write tables to template database ----
dbase_write <- paste0("Data/Processed/", prj_params$PRJ_CD, "_T5.accdb")
if(file.exists(dbase_write)) {file.remove(dbase_write)}
file.copy(dbase_template, dbase_write)

conn_write <- odbcConnectAccess2007(dbase_write, uid = "", pwd = "")
isverbose = FALSE
sqlSave(conn_write, fn011, tablename = "FN011", append = TRUE, rownames = FALSE, verbose = isverbose)
ui_done("FN011 written to Access")
sqlSave(conn_write, fn012, tablename = "FN012", append = TRUE, rownames = FALSE, verbose = isverbose)
ui_done("FN012 written to Access")
sqlSave(conn_write, fn022, tablename = "FN022", append = TRUE, rownames = FALSE, verbose = isverbose)
ui_done("FN022 written to Access")
sqlSave(conn_write, fn026, tablename = "FN026", append = TRUE, rownames = FALSE, verbose = isverbose)
ui_done("FN026 written to Access")
sqlSave(conn_write, fn026_sub, tablename = "FN026_Subspace", append = TRUE, rownames = FALSE, verbose = isverbose)
ui_done("FN026_Subspace written to Access")
sqlSave(conn_write, fn028, tablename = "FN028", append = TRUE, rownames = FALSE, verbose = isverbose)
ui_done("FN028 written to Access")
sqlSave(conn_write, fn121, tablename = "FN121", append = TRUE, rownames = FALSE, verbose = isverbose)
ui_done("FN121 written to Access")
sqlSave(conn_write, fn122, tablename = "FN122", append = TRUE, rownames = FALSE, verbose = isverbose)
ui_done("FN122 written to Access")
sqlSave(conn_write, fn123, tablename = "FN123", append = TRUE, rownames = FALSE, verbose = isverbose)
ui_done("FN123 written to Access")
sqlSave(conn_write, fn123nonfish, tablename = "FN123_NonFish", append = TRUE, rownames = FALSE, verbose = isverbose)
ui_done("FN123_NonFish written to Access")
sqlSave(conn_write, fn124, tablename = "FN124", append = TRUE, rownames = FALSE, verbose = isverbose)
ui_done("FN124 written to Access")
sqlSave(conn_write, fn125, tablename = "FN125", append = TRUE, rownames = FALSE, verbose = isverbose)
ui_done("FN125 written to Access")
sqlSave(conn_write, fn125_lam, tablename = "FN125_lamprey", append = TRUE, rownames = FALSE, verbose = isverbose)
ui_done("FN125_lamprey written to Access")
sqlSave(conn_write, Gear_Eff_Process, tablename = "Gear_Effort_Process_Types", append = TRUE, rownames = FALSE, verbose = isverbose)
ui_done("Gear table written to Access")
odbcClose(conn_write)
ui_info("Closing all database connections")
odbcCloseAll()
ui_done(paste0("Data has been written to: ", dbase_write))

