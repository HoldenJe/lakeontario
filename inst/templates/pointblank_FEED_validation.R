# pointblank validation for FEED checks
# J. Holden
# March 2024
# this script will connect to a database and use pointblank agents to interrogate the data
# the output is in a simplified html document
# individual agents can be saved as html outputs but unfortunately there is a known
# issue in the package preventing saving multi-agent reports

# set local parameters ----
# specify database connection as required
dbase <- file.choose()

# QA/QC checks
library(dplyr)
library(RODBC)
library(usethis)
library(pointblank)
library(ggplot2)
library(rprocval) # available by: devtools::install_github("HoldenJe/rprocval")
library(purrr)
library(lubridate)

load("params.RData")
sample_year <- year(ymd(myinputs$PRJ_DATE0))
sample_months <- c(month(ymd(myinputs$PRJ_DATE0)):month(ymd(myinputs$PRJ_DATE1)))
# Access requires 32-bit R
testR <- Sys.getenv("R_ARCH") == "/i386"
if(testR){
  ui_info("R is running in 32-bit mode")
} else {ui_oops("Access requires a 32-bit connection")}

# Connect to db ----
#### load data ----
conn <- odbcConnectAccess(dbase, uid = "", pwd = "")
op <- sqlFetch(conn, "Calc_RVCAT OP")
op_times <- sqlFetch(conn, "Calc_RVCAT TR_OP")
tr_op <- sqlFetch(conn, "OP_5Trawl_OP")
target <- sqlFetch(conn, "Calc_RVCAT OP_TARGET")
subsample <- sqlFetch(conn, "TR_1SubSample")
sampleWT <- sqlFetch(conn, "TR_3DSpeciesCtWt")
rvcat_lf <- sqlFetch(conn, "Calc_RVCAT TR_LF")
species <- sqlFetch(conn, "Codes_Species")
tr_fish <- sqlFetch(conn, "TR_7FISH")
odbcClose(conn)

## pointblank set up
al <- action_levels(warn_at = 1) # any failed test will generate a "warning" level response

#### manipulate tables as required ----
sampleN <- sampleWT %>% filter(!(Species %in%c(952, 999, 940, 0)))

op <- op %>%
  mutate(MONTH = month(OP_DATE)) %>%
  mutate(InSSN = ifelse(MONTH %in% sample_months, TRUE, FALSE))

tr_op$ExpectedWarp <- tr_op$Fishing_Depth*3
tr_op <- tr_op %>%
  mutate(WarpDiff = abs(Warp-ExpectedWarp))

## subsample ----
subsample <- subsample %>%
  mutate(ValidMultiplier = case_when(
    SubSampleYN == "N" ~ Multiplier == 1,
    SubSampleYN == "Y" ~ Multiplier > 1
  ))

# sampleWT %>% select(Serial, Species, Weight, Count, SampleWtVsAllSpeciesWt)

## sampleWT ----
sampleWTbatch <- sampleWT %>% group_by(Serial) %>%
  summarize(SampleWeight = max(SampleWeight, na.rm = T),
            SampledWt = sum(Weight)) %>%
  mutate(UnsampledWt = SampleWeight - SampledWt,
         PropSampled = round(SampledWt/SampleWeight,2))

## check max/min lengths
species <- species %>% filter(!is.na(LW_a)) %>%
  rename(Species = SpeciesCode)

rvcat_lf_summary <- rvcat_lf %>% group_by(Serial, Species, Lifestage) %>%
  rename(LifeStage = Lifestage) %>%
  summarize(N_LF = sum(N))

rvcat_lf_wt <- inner_join(rvcat_lf, species)

rvcat_lf_wt <- rvcat_lf_wt %>%
  filter(Lifestage == 6) %>%
  mutate(predWT = LW_a*Length^LW_b*N) %>%
  select(OP_Year, Vessel, Serial, Species, Length, N, predWT) %>%
  group_by(OP_Year, Vessel, Serial, Species) %>%
  summarize(N = sum(N), WT = sum(predWT)) %>%
  mutate(PredAvgWT = WT/N)

AvgSpcWtBatch <- sampleWT %>% select(Serial, Species, Weight, Count) %>%
  filter(Species!=952) %>%
  mutate(AvgWtCatch = Weight/Count)

compare_lf_catwt <- inner_join(rvcat_lf_wt, AvgSpcWtBatch)
compare_lf_catwt <- compare_lf_catwt %>%
  mutate(LogRatio = abs(log10(AvgWtCatch)-log10(PredAvgWT)))

lf_summary <- left_join(sampleN, rvcat_lf_summary) %>%
  filter(LifeStage == 6) %>%
  select(Serial, Species, LifeStage, Weight, Count, N_LF) %>%
  mutate(N_LF_lte_Count = N_LF <= Count)

# trFISH l vs w
fish <- inner_join(tr_fish, species)

fish <- fish %>%
  filter(LifeStage == 6) %>%
  mutate(PredWt = LW_a*Length^(LW_b)) %>%
  mutate(WtRatio = PredWt/Weight) %>%
  mutate(HasWeightError = ifelse(WtRatio > 1.3 | WtRatio < 0.7, "Error", "NoError"))

fish_fn2 <- fish %>% select(Species, Length, Weight) %>%
  rename(SPC = Species, TLEN = Length, RWT = Weight)

fish_rproc_tests <- fish_fn2 %>%
  split(.$SPC) %>%
  map(fn125_tl_rwt_lm, makeplot =T) %>%
  bind_rows()

fish_rproc_tests$qid4_error[is.na(fish_rproc_tests$qid4_error)] <- FALSE

# do pointblank validation ----
op_agent <- create_agent(op, "OP", actions = al) %>%
  col_vals_not_null(vars(YEAR, VESSEL, SERIAL, LAKE, PORT, OP_DATE,
                         END_DEPTH, STATION_DEPTH, LOCATION)) %>%
  col_is_posix(vars(OP_DATE)) %>%
  col_is_numeric(vars(STATION_DEPTH,  WIND_COMPASS,
                      BEG_DEPTH, END_DEPTH, WIND_SPEED)) %>%
  col_is_integer(vars(VESSEL_COMPASS)) %>%
  col_vals_equal(vars(VESSEL), value = 38) %>%
  col_vals_equal(vars(LAKE), value = 6) %>%
  col_vals_equal(vars(YEAR), value = sample_year) %>%
  col_vals_equal(vars(InSSN), value = TRUE) %>%
  interrogate()

tow_times <- create_agent(op_times, "RVCAT TR_OP", actions = al) %>%
  col_vals_between(vars(TOW_TIME1), left = 2, right = 6) %>%
  interrogate()

trop_agent <- create_agent(tr_op, "TR_OP", actions = al) %>%
  col_vals_not_null(vars(Serial, Set_Time, Speed, Fishing_Depth, Tow_Time,
                         Fishing_Temp, Warp)) %>%
  col_vals_between(vars(Warp), left = 10, right = 500) %>%
  col_vals_lte(vars(WarpDiff), value = 15) %>%
  interrogate()

subsample_agent <- create_agent(subsample, "Subsample", actions = al) %>%
  col_vals_not_null(vars(Multiplier)) %>%
  col_vals_gte(vars(Multiplier), value = 1) %>%
  col_vals_equal(vars(ValidMultiplier), TRUE) %>%
  interrogate()

sampleWT_agent <- create_agent(sampleWTbatch, "Subsample Batch", actions = al) %>%
  col_vals_not_null(vars(SampleWeight, SampledWt)) %>%
  col_vals_between(vars(PropSampled), left = 0.8, right = 1) %>%
  interrogate()

lf_agent <- create_agent(lf_summary, "Length Frequency", actions = al) %>%
  col_vals_not_null(vars(Weight, Count, N_LF)) %>%
  col_vals_equal(vars(N_LF_lte_Count), TRUE) %>%
  interrogate()

lf_to_catchWT_agent <- create_agent(compare_lf_catwt,
                                    "Length Freq vs Avg Fish Size",
                                    actions = al) %>%
  col_vals_lte(vars(LogRatio), value = 0.2) %>%
  interrogate()

fish_bio <- create_agent(fish_rproc_tests, "Fish Bio", actions = al) %>%
  col_vals_equal(vars(qid4_error), FALSE) %>%
  interrogate()

lamchecksp <- tr_fish %>% filter(Species %in% c(307, 202, 203))
has_lam <- create_agent(lamchecksp,
                        "Species require") %>%
  col_vals_equal(vars(WoundsYN), "Y") %>%
  interrogate()

multi_report <- create_multiagent(op_agent, trop_agent, tow_times, subsample_agent,
                                  sampleWT_agent, lf_agent, lf_to_catchWT_agent, fish_bio, has_lam)

multi_report
