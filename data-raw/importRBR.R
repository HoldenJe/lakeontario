#' importRBR
#' @description A wrapper for the `oce` function `read.rsk`. This wrapper converts the raw pressure data
#' and converts it to depth in freshwater.
#' @export

importRBR<-function(rskfile) {
  require(oce)
  require(dplyr)
  if(!file.exists(rskfile)) {stop('file does not exist')}
  myrskdat<-read.rsk(rskfile)
  fwdepth<-(myrskdat[['pressure']]-myrskdat[['pressureAtmospheric']])*1.019716
  rskdf<-data.frame(TIME = myrskdat[['time']], DEPTH = fwdepth,
                    TEMPERATURE = myrskdat[['temperature']])
  myvalues<-strsplit(rskfile, "_")
  rskdf$YEAR <- as.numeric(substr(myvalues[[1]][1], nchar(myvalues[[1]][1])-3, nchar(myvalues[[1]][1])))
  rskdf$VESSEL <- myvalues[[1]][3]
  rskdf$SERIAL <- as.numeric(myvalues[[1]][5])
  rskdf$OPtype<-strsplit(myvalues[[1]][6], "\\.")[[1]][1]
  rskdf$OPtype<-tolower(rskdf$OPtype)
  rskdf$OPtype<-substr(rskdf$OPtype, 1,4)
  rskdf$FILE<-rskfile
  rskdf$DEPTH<-round(rskdf$DEPTH, 2)
  rskdf<-filter(rskdf, DEPTH>0)
  rskdf
}

#' import_merge_tow_files
#' @description A wrapper for `importRBR` specific to merging head and footrope files
#' @export

import_merge_tow_files<-function(rbrfiles) {
  footfiles<-grep(rbrfiles, pattern = 'foot', value = T)
  headfiles<-grep(rbrfiles, pattern = 'head', value = T)
  footstats<-bind_rows(lapply(footfiles, importRBR))
  headstats<-bind_rows(lapply(headfiles, importRBR))

  headstats <- headstats %>% rename(HEAD_DEPTH = DEPTH, HEAD_TEMP = TEMPERATURE) %>%
    select(-OPtype, -FILE)
  footstats <- footstats %>% rename(FOOT_DEPTH = DEPTH, FOOT_TEMP = TEMPERATURE)%>%
    select(-OPtype, -FILE)

  towstats <- inner_join(footstats, headstats) %>%
    select(YEAR, VESSEL, SERIAL, TIME, FOOT_DEPTH, FOOT_TEMP, HEAD_DEPTH, HEAD_TEMP)

  towstats$OPENING_HEIGHT <- with(towstats, FOOT_DEPTH - HEAD_DEPTH)

  towstats
}

#' rbr_tow_plot
#' @description plots a tow profile
#' @export

rbr_tow_plot<-function(oneserial) {
  oneserial<- filter(oneserial, FOOT_DEPTH > (max(FOOT_DEPTH)-10))
  mytitle<-paste('Serial', unique(oneserial$SERIAL), " - YEAR ", unique(oneserial$YEAR), sep = ' ')
  par(mfrow=c(2,1))
  par(mar=c(1,4,1,1))
  plot(OPENING_HEIGHT~TIME, oneserial, type = 'l',
       main = mytitle, bty='l', xaxt = 'n')
  par(mar=c(3,4,0.5,1))
  plot(FOOT_DEPTH~TIME, oneserial, type='l', col='red', bty='l',
       ylim=c(max(oneserial$FOOT_DEPTH)+2, max(oneserial$FOOT_DEPTH)-10))
  points(HEAD_DEPTH~TIME, oneserial, type='l', col='blue')
  legend('bottomleft', c('HEAD', 'FOOT'), bty = 'n', lty = 1, col = c('blue', 'red'))

}



#' get_trawl_stats
#' @description Input a list of rsk files that have been imported using `importRBR` to generate tow statistics
#' @export

get_trawl_stats<-function(rawtow, reduceresolution ='10s',
                          rollmeanstat = 10,
                          rollmeanmaxchange = 0.2,
                          mindepthallowed = 10,
                          max_dist_from_max_dep = 15) {
  require(zoo)
  require(lubridate)

  # get surface temp
  surfacetemp<- filter(rawtow, HEAD_DEPTH < 2) %>% group_by(YEAR, SERIAL) %>%
    summarize(SURF_TEMP = round(mean(HEAD_TEMP), 2))

  # identify tow area
  fulltow <- filter(rawtow, FOOT_DEPTH > 2)

  # plot temp profile
  #plot(FOOT_DEPTH~FOOT_TEMP, fulltow, ylim = c(max(fulltow$FOOT_DEPTH), 0))

  # identify tow period
  ## define working parameters
  # reduceresolution<-'10s'
  # rollmeanstat <- 10
  # rollmeanmaxchange <- 0.2
  # mindepthallowed <- 12.5

  fulltow <-fulltow %>% mutate(TIME = round_date(TIME, unit = reduceresolution))
  fulltow <- fulltow %>% group_by(YEAR, SERIAL, TIME) %>%
    summarize(FOOT_DEPTH = mean(FOOT_DEPTH), HEAD_DEPTH = mean(HEAD_DEPTH))
  fulltow$depchange<-c(NA, diff(fulltow$FOOT_DEPTH))
  #plot(depchange~TIME, fulltow, main = "Depth Change", type= 'l')
  fulltow$rmean<-abs(rollmean(fulltow$depchange, rollmeanstat, align = 'center', fill = NA))
  plot(FOOT_DEPTH~TIME, fulltow, ylim = c(max(fulltow$FOOT_DEPTH), 0),
       main = paste('Tow profile - Serial', unique(fulltow$SERIAL), ' - ', unique(fulltow$YEAR)))

  fulltow$dist_from_max<-max(fulltow$FOOT_DEPTH) - fulltow$FOOT_DEPTH
  justfishing<-filter(fulltow, rmean < rollmeanmaxchange,
                      FOOT_DEPTH > mindepthallowed,
                      dist_from_max < max_dist_from_max_dep) # this could be moved to the function to tweak

  points(FOOT_DEPTH~TIME, justfishing, col='red', pch = 16)
  points(HEAD_DEPTH~TIME, justfishing, col='blue', pch = 16)
  legend('bottomleft', c('Headrope', 'Footrope'), col = c('blue', 'red'), bty='n', pch =16)

  fishingstats <- rawtow %>% filter(TIME > min(justfishing$TIME) & TIME < max(justfishing$TIME))%>%
    group_by(YEAR, SERIAL) %>%
    summarize(foot_depth_mean = round(mean(FOOT_DEPTH),2),
              foot_max_depth = round(max(FOOT_DEPTH),2),
              foot_min_depth = round(min(FOOT_DEPTH),2),
              foot_mean_tempC = round(mean(FOOT_TEMP),2),
              foot_max_tempC = round(max(FOOT_TEMP),2),
              foot_min_depth = round(min(FOOT_DEPTH),2),
              tow_duration_est = round(justfishing$TIME[nrow(justfishing)]-justfishing$TIME[1],1),
              head_depth_mean = round(mean(HEAD_DEPTH),2),
              head_max_depth = round(max(HEAD_DEPTH),2),
              head_min_depth = round(min(HEAD_DEPTH),2),
              head_mean_tempC = round(mean(HEAD_TEMP),2),
              head_max_tempC = round(max(HEAD_TEMP),2),
              head_min_depth = round(min(HEAD_TEMP),2),
              opening_height_mean = round(mean(OPENING_HEIGHT),2))
  alltowstats<-inner_join(surfacetemp, fishingstats)
  alltowstats
}

