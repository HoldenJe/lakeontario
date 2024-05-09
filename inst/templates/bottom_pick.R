# Conduct bottom pick from RBR files
# utilizes serveral helper functions in lakeontario to
# pick the touchdown and liftoff points during bottom trawls
# or start and end points of midwater trawls

# load libraries
library(lakeontario)
library(dplyr)
library(ggplot2)
library(RODBC)
library(lubridate)

# get all RBR tow files
rbrfiles_all <- dir("Data/RBRClean", full.names = T)

# import data
towstats <- import_merge_tow_files(rbrfiles_all)

# note: lakeontario provides a "foot" only function if needed
# towstats <- import_foot_rope_files(rbrfiles_all)

# Verify all SERIAL files have been loaded
unique(towstats$SERIAL)

# following function requires each SERIAL to be it's own dataframe
tow_stat_list <- split(towstats, towstats$SERIAL)

# Creating a dedicated window is not required but helpful
windows()

# provides static plots for initial inspection
lapply(tow_stat_list, rbr_tow_plot)

# the following code will plot each tow in the plot window and
# await the user to click on the start and end points of the tow
bottom_contact_times <- lapply(tow_stat_list, pick_bottom_contact)

# converts list to a single data frame
bottom_contact_times <- bind_rows(bottom_contact_times)

# saves files - change name as required
save(bottom_contact_times, file = "bottom_contact_times.Rdata")
write.csv(bottom_contact_times, file = "bottom_contact_times.csv", row.names = F)

# summarizes min/max tow values and an automated "guess" at contact times
# the automated "guess" isn't a reliable as the manual process
# conducted above but provides some useful data for comparison
btr_mens <- lapply(tow_stat_list, get_trawl_stats,
                   max_dist_from_max_dep = 10, rollmeanmaxchange = 0.4)

btr_mensuration <- bind_rows(btr_mens)

# edit filename as required
save(btr_mensuration, file = "towstats.Rdata")

# end
