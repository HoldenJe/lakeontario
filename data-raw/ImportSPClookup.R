library(stringr)
library(usethis)
RV2FN_SPECIES <- read.csv("data-raw/SPCShort2SPECIESRVCAT.csv")
RV2FN_SPECIES$SPC <- str_pad(RV2FN_SPECIES$SPC, 3, pad = "0")

use_data(RV2FN_SPECIES, overwrite = T)
