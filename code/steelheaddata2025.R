#'******************************************************************************
#' The goal of the script is to pull relevant steelhead data from the database
#' and update with new stream- and CU- level spawner abundance
#' 
#' 
#' Files produced: 
#' 
#' Resources/Notes:
#'
#'******************************************************************************
#'
require(tidyverse)
library(arsenal)

source("/Users/erichertz/Salmon Watersheds Dropbox/Eric Hertz/X Drive/1_PROJECTS/1_Active/Population Methods and Analysis/population-indicators/code/functions_general.R")

abund_data <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_dataset1cu_output")

stream_data <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_streamspawnersurveys_output")

abund_data_steelhead <- filter(abund_data, species_name == 'Steelhead')

stream_data_steelhead <- filter(stream_data, species_name == 'Steelhead')

write.csv(abund_data_steelhead, "output/dataset1cu_steelhead_20250117.csv", row.names=FALSE)

write.csv(stream_data_steelhead, "output/streamspawnersurveys_steelhead_20250117.csv", row.names=FALSE)

abund_data_fraser_update <- read.csv("output/dataset1_Fraser_20241010.csv")

rs_data_fraser_update <- read.csv("output/dataset5_Fraser_20241010.csv")

summary(comparedf(abund_data_fraser,abund_data_fraser_update))

summary(comparedf(rs_data_fraser,rs_data_fraser_update))
