###############################################################################
# Compiling steelhead CU-level spawner abundance
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
	filter(species_name == "Steelhead", !is.na(observed_count)|!is.na(estimated_count)) %>%
	arrange(cuid, year)

dat1_updated <- dat1

# How many CUs? 18
length(unique(dat1$cuid))
dat1 %>% select(cuid, region, cu_name_pse) %>% distinct(cuid, .keep_all = TRUE)

# Import recent spawner survey data for observed_count
dat2 <- read.csv("output/dataset2_spawner-surveys_Steelhead.csv")

###############################################################################
# Update estimated_count data by region
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

# Create new data in output format
dat1_upsus <- dat1_updated %>% filter(cu_name_pse == "Upper Sustut" & year == 2024) %>% 
	mutate(year = 2025,
				 estimated_count = dat2$stream_observed_count[dat2$stream_name_pse == "UPPER SUSTUT RIVER" & dat2$year == 2025],
				 observed_count = dat2$stream_observed_count[dat2$stream_name_pse == "UPPER SUSTUT RIVER" & dat2$year == 2025])
dat1_upsus$cu_name_pse

# append 2025
dat1_updated <- dat1_updated %>%
	bind_rows(dat1_upsus) %>%
	arrange(cuid, year)

dat1_updated$source_id[dat1_updated$cu_name_pse == "Upper Sustut"] <- dat2$source_id[dat2$stream_name_pse == "UPPER SUSTUT RIVER" & dat2$year == 2024]

#------------------------------------------------------------------------------
# Nass
#------------------------------------------------------------------------------

# Nass Summer (cuid 480)
english2023 <- read_xlsx("data/English2023_TableF6.xlsx", range = "A3:Q32")

dat1_updated %>% filter(cuid == 480) 

# Source ID for 1994 - 2022 is English et al. 2023
dat1_updated$source_id[which(dat1_updated$cuid == 480)] <- "English_20230930"

# Nisgaa FWD report - Nass Mark recapture estimate
nisgaa2025 <- read_xlsx("data/2025NassStockAssessment_Table20+22.xlsx")

# Compare across datasets
dat1_updated %>% filter(cuid == 480) %>%
	select(year, estimated_count) %>%
	left_join(english2023 %>% select(Year, Escapement) %>% rename(year = Year)) %>%
	left_join(nisgaa2025 %>% select(Year, ESC_Steelhead) %>% rename(year = Year)) %>%
	mutate(rounded = round(Escapement/1000)*1000, 
				 estimated_count - Escapement)

# Findings:
# - English et al, (2023) Table F6 is slightly different from the data in the PSE right now but
# matches what's published in the NFWD post-season report (though the latter is rounded to nearest
# 1000). 
# Use English et al, (2023) and the NFWD numbers for 2023-2025


# Create new data in output format
dat1_nasssummer <- english2023 %>% 
	select(Year, Escapement) %>% 
	rename(year = Year, 
				 estimated_count = Escapement) %>%
	mutate(source_id = "English_20230930") %>%
	bind_rows(nisgaa2025 %>% 
							select(Year, ESC_Steelhead) %>% 
							rename(year = Year,
										 estimated_count = ESC_Steelhead) %>%
							filter(year %in% english2023$Year == FALSE) %>%
						mutate(source_id = "Nisgaa_20251202")) %>%
	arrange(year) %>%
	left_join(dat1_updated %>% filter(cuid == 480) %>%
							select(year, observed_count)
						) %>%
	mutate(dat1_updated %>% filter(cuid == 480 & year == 2022) %>%
							select(region, species_name, species_qualified, cuid, cu_name_pse)) %>%
	select(region, species_name, species_qualified, cuid, cu_name_pse, year, estimated_count, observed_count, source_id)

# Fold in
dat1_updated <- dat1_updated %>%
	filter(cuid != 480) %>% # take out existing data
	bind_rows(dat1_nasssummer) %>% # add in new data
	arrange(cuid)

#------------------------------------------------------------------------------
# Fraser
#------------------------------------------------------------------------------

# Mid Fraser Summer - equal to Chilcotin River
# Thompson Summer - equal to Thompson River est

dat1_fraser <- dat2 %>%
	filter(stream_name_pse %in% c("CHILCOTIN RIVER", "THOMPSON RIVER")) %>%
	select(cuid, year, stream_observed_count, source_id) %>%
	left_join(dat1_updated %>% 
							filter(cuid %in% c(780, 781)) %>%
							select(region, species_name, species_qualified, cuid, cu_name_pse) %>%
							distinct()) %>%
	left_join(dat2 %>% 
							filter(cuid %in% c(780, 781)) %>%
							group_by(paste(cuid, year)) %>%
							summarise(year = unique(year),
												cuid = unique(cuid), 
												observed_count = sum(stream_observed_count, na.rm = TRUE)) %>%
							select(cuid, year, observed_count))

# Compare - good
dat1_fraser %>% filter(cuid == 781) %>%
	select(year, stream_observed_count) %>%
	left_join(dat1 %>% filter(cuid == 781) %>%
							select(year, estimated_count))

dat1_fraser %>% filter(cuid == 780) %>%
	select(year, stream_observed_count) %>%
	left_join(dat1 %>% filter(cuid == 780) %>%
							select(year, estimated_count))

# Fold in
dat1_updated <- dat1_updated %>%
	filter(cuid %in% c(780, 781) == FALSE) %>% # remove old data
	bind_rows(dat1_fraser %>%
							rename(estimated_count = stream_observed_count) %>%
							select(names(dat1_updated))) %>%
	arrange(cuid, year)

#------------------------------------------------------------------------------
# EVIMI
#------------------------------------------------------------------------------

dat1_updated %>% filter(cuid == 985) %>%
	mutate(estimated_count - observed_count)

dat2 %>% filter(cuid == 985) %>% select(stream_name_pse) %>% unique()

# South Coast Winter (980) = CHEAKAMUS RIVER (No updated since 2020)
# EVI Winter (981) = expansion from Englishman and Keogh
source("code/expand_function.R")
expanded_dat_981 <- dat2 %>% 
	filter(cuid == 981) %>% 
	select(stream_name_pse, year, stream_observed_count) %>% 
	expand()


dat1_eviwinter <- expanded_dat_981 %>%
	select(year, expanded_index, simple_sum) %>%
	mutate(source_id = NA) %>%
	rename(estimated_count = expanded_index,
				 observed_count = simple_sum) %>%
	mutate(dat1_updated %>% filter(cuid == 981 & year == 2000) %>%
				 	select(region, species_name, species_qualified, cuid, cu_name_pse)) %>%
	select(names(dat1_updated))

dat1_updated<- dat1_updated %>%
	filter(cuid!= 981) %>% # remove old data
	bind_rows(dat1_eviwinter) %>%
	arrange(cuid, year)

# EVI Summer (985) = TSITIKA RIVER
dat1_evisummer <- dat2 %>% filter(cuid == 985) %>%
	rename(estimated_count = stream_observed_count) %>%
	select(year, estimated_count, source_id) %>%
	left_join(dat1_updated %>% filter(cuid == 985) %>%
				 	select(year, observed_count)) %>%
	mutate(dat1_updated %>% filter(cuid == 985 & year == 2022) %>%
				 	select(region, species_name, species_qualified, cuid, cu_name_pse)) %>%
	select(names(dat1_updated))

dat1_updated <- dat1_updated %>%
	filter(cuid!= 985) %>% # remove old data
	bind_rows(dat1_evisummer) %>%
	arrange(cuid, year)


#------------------------------------------------------------------------------
# WVI
#------------------------------------------------------------------------------

# WVI Summer (984)
dat1_updated %>% filter(cuid == 984) %>%
	mutate(estimated_count - observed_count)

# Expansion from three rivers:
dat2 %>% filter(cuid == 984) %>% select(stream_name_pse) %>% unique()
dat2 %>% filter(cuid == 984) %>% select(source_id) %>% unique()

expanded_dat_984 <- dat2 %>% 
	filter(cuid == 984) %>% 
	select(stream_name_pse, year, stream_observed_count) %>% 
	expand()


dat1_wvisummer <- expanded_dat_984 %>%
	select(year, expanded_index, simple_sum) %>%
	mutate(source_id = "McCulloch_20260804") %>% # All three surveys from him
	rename(estimated_count = expanded_index,
				 observed_count = simple_sum) %>%
	mutate(dat1_updated %>% filter(cuid == 984 & year == 2000) %>%
				 	select(region, species_name, species_qualified, cuid, cu_name_pse)) %>%
	select(names(dat1_updated))

dat1_updated<- dat1_updated %>%
	filter(cuid!= 984) %>% # remove old data
	bind_rows(dat1_wvisummer) %>%
	arrange(cuid, year)

#------------------------------------------------------------------------------
# Columbia
#------------------------------------------------------------------------------

dat1_updated %>% filter(cuid == 1380) %>%
	mutate(estimated_count - observed_count)

# Just natural spawners for estimated count...but we're changing that.
# Now use all spawners.

dat1_col <- dat2 %>% filter(cuid == 1380, stream_name_pse == "OKANAGAN RIVER") %>%
	rename(estimated_count = stream_observed_count) %>%
	select(year, estimated_count, source_id) %>%
	mutate(observed_count = NA) %>%
	mutate(dat1_updated %>% filter(cuid == 1380 & year == 2022) %>%
				 	select(region, species_name, species_qualified, cuid, cu_name_pse)) %>%
	select(names(dat1_updated))

dat1_updated <- dat1_updated %>%
	filter(cuid != 1380) %>% # remove old data
	bind_rows(dat1_col) %>%
	arrange(cuid, year)

###############################################################################
# Update observed_count across all CUs
###############################################################################

n_CUs <- length(unique(dat1_updated$cuid))
cuids <- unique(dat1_updated$cuid)

for(i in 1:n_CUs){
	observed_count <- dat2 %>% 
		filter(cuid == cuids[i]) %>%
		group_by(year) %>%
		summarise(observed_count = sum(stream_observed_count, na.rm = TRUE))
	
	dat1_observed_count <- dat1_updated %>% filter(cuid == cuids[i], !is.na(observed_count)) %>%
		select(year, observed_count)
	
	if(dim(dat1_observed_count)[1] == 0 & dim(observed_count)[1] == 0){
		
	} else {
		update.i <- dat1_updated %>% filter(cuid == cuids[i]) %>%
			select(-observed_count) %>%
			full_join(observed_count) %>%
			arrange(year) %>%
			select(year, estimated_count, observed_count, source_id) %>%
			mutate(dat1_updated %>% filter(cuid == cuids[i], year == dat1_updated$year[dat1_updated$cuid == cuids[i]][1]) %>%
						 	select(region, species_name, species_qualified, cuid, cu_name_pse)) %>%
			select(names(dat1_updated))
	
	# comp.i <- update.i %>% select(year, estimated_count, observed_count) %>%
	# 	left_join(dat1 %>% filter(cuid == cuids[i]) %>%
	# 							select(year, observed_count) %>%
	# 							rename(old_observed_count = observed_count))
	# 
	# print(comp.i)
	# 
	# updateQ <- readline(prompt = "Based on data, merge new data? Enter Y/N: ")
	# #--------------------
	# if(updateQ == "Y"){
	dat1_updated <- dat1_updated %>%
		filter(cuid != cuids[i]) %>%
		bind_rows(update.i) %>%
		arrange(cuid, year)
	# }
	}
}

dat1_updated$observed_count <- round(dat1_updated$observed_count)
dat1_updated$estimated_count <- round(dat1_updated$estimated_count)


###############################################################################
# write.csv
###############################################################################

write.csv(dat1_updated, file = "output/dataset1_spawner-abundance_Steelhead.csv", row.names = FALSE)
write.csv(dat1_updated,
					paste0(get_XDrive(),"1_PROJECTS/1_Active/Population Methods and Analysis/population-data/steelhead-data/output/archive/dataset1_spawner-abundance_Steelhead_", Sys.Date(), ".csv"),
					row.names = FALSE)
