% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importRBR.R
\name{get_trawl_stats_foot_only}
\alias{get_trawl_stats_foot_only}
\title{get trawl stats for foot rope}
\usage{
get_trawl_stats_foot_only(
  rawtow,
  reduceresolution = "10s",
  rollmeanstat = 10,
  rollmeanmaxchange = 0.2,
  mindepthallowed = 10,
  max_dist_from_max_dep = 15
)
}
\arguments{
\item{rawtow}{a dataframe of temp and depth values}

\item{reduceresolution}{a smoothing parameter. Default = 10s}

\item{rollmeanstat}{a value that indicates how many seconds the rolling mean should be caluclated over}

\item{rollmeanmaxchange}{a value that indicates the max change in mean between values}

\item{mindepthallowed}{the minimum depth that the tow is expected to be in}

\item{max_dist_from_max_dep}{the maximum depth change the max depth and when the tow is still likely to be on bottom}
}
\value{
data frame of summarized tow stats
}
\description{
get trawl stats for foot rope
}
\seealso{
[get_trawl_stats()]
}
