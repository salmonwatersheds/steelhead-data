###############################################################################
# Compiling steelhead spawner surveys
# Steph Peacock
# August 12, 2026
###############################################################################

library(dplyr)
library(readxl)

###############################################################################
# Pull existing data from database
###############################################################################

source("https://raw.githubusercontent.com/salmonwatersheds/population-indicators/refs/heads/master/code/functions_general.R")

# spawner surveys
dat1 <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_dataset1cu_output") %>% 
	filter(species_name == "Steelhead", !is.na(estimated_count))

dat1_updated <- dat1

# How many CUs? 11
length(unique(dat1$cuid))
dat1 %>% select(cuid, region, cu_name_pse) %>% distinct(cuid, .keep_all = TRUE)

# Import recent spawner survey data for observed_count
dat2 <- read.csv("output/dataset2_spawner-surveys_Steelhead.csv")

###############################################################################
# Update data by region
###############################################################################

#------------------------------------------------------------------------------
# Skeena
#------------------------------------------------------------------------------

# Upper Sustut - CU estimated spawners is equal to Upper Sustut spawner survey
dat1_updated %>% filter(cu_name_pse == "Upper Sustut") 
dat2 %>% 
	filter(stream_name_pse == "UPPER SUSTUT RIVER") %>% 
	select(year, stream_observed_count) %>%
	left_join(dat1_updated %>% filter(cu_name_pse == "Upper Sustut") %>% select(year, estimated_count, observed_count)) # All Match

dat1_updated <- 