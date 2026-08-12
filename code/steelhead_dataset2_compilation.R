###############################################################################
# Compiling steelhead spawner surveys
# Steph Peacock
# June 2, 2026
###############################################################################

library(dplyr)
library(readxl)

###############################################################################
# Pull existing data from database
###############################################################################

source("https://raw.githubusercontent.com/salmonwatersheds/population-indicators/refs/heads/master/code/functions_general.R")

# spawner surveys
dat2_all <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_streamspawnersurveys_output")

# Find max pointid and streamid for new sites
pointid_max <- max(dat2_all$pointid)
streamid_max <- max(dat2_all$streamid)

# Subset steelhead
dat2 <- dat_all %>% 
	filter(species_name == "Steelhead") %>%
	filter(!is.na(stream_observed_count))

dat2_updated <- dat2

# Read in lookup file with stream names
cu_stream_lookup <- read.csv("data/cu-stream-lookup_steelhead.csv")

###############################################################################
# Compile data
###############################################################################

#------------------------------------------------------------------------------
# Fraser
#------------------------------------------------------------------------------

# Update source_id for existing
dat2_updated %>% filter(stream_name_pse %in% c("CHILCOTIN RIVER", "THOMPSON RIVER"))

ind <- which(dat2_updated$stream_name_pse %in% c("CHILCOTIN RIVER", "THOMPSON RIVER") & dat2_updated$year %in% c(1972:2021))
dat2_updated$source_id[ind] <- "Bison_20210820"

dat2_updated %>% filter(region == "Fraser", is.na(source_id))

dat2_updated$source_id[dat2_updated$stream_name_pse == "BRIDGE RIVER" & dat2_updated$year == 2000] <- "Hagen_20010412"

# COQUIHALLA RIVER (Beere_2014)
dat2_updated$stream_survey_method[dat2_updated$stream_name_pse == "COQUIHALLA RIVER" & dat2_updated$year %in% c(1974:2013)] <- "Snorkel Survey"
dat2_updated$stream_survey_quality[dat2_updated$stream_name_pse == "COQUIHALLA RIVER" & dat2_updated$year %in% c(1974:2013)] <- "Low"
dat2_updated$source_id[dat2_updated$stream_name_pse == "COQUIHALLA RIVER" & dat2_updated$year %in% c(1974:2013)] <- "Beere_2014"

# LITTLE CAMPBELL RIVER 
# ** Note that there is wild/hatchery breakdown for this! Just summed here.
dat2_updated$stream_survey_method[dat2_updated$stream_name_pse == "LITTLE CAMPBELL RIVER" & dat2_updated$year %in% c(1982:2013)] <- "Fence Count"
dat2_updated$stream_survey_quality[dat2_updated$stream_name_pse == "LITTLE CAMPBELL RIVER" & dat2_updated$year %in% c(1982:2013)] <- "Medium"
dat2_updated$source_id[dat2_updated$stream_name_pse == "LITTLE CAMPBELL RIVER" & dat2_updated$year %in% c(1982:2013)] <- "Beere_2014"

# ** Note that lat/lon were wrong in previous data, placing this surey on Vancouver Island
dat2_updated$pointid[dat2_updated$stream_name_pse == "LITTLE CAMPBELL RIVER"] <- pointid_max + 1
dat2_updated$latitude[dat2_updated$stream_name_pse == "LITTLE CAMPBELL RIVER"] <- 49.022370
dat2_updated$longitude[dat2_updated$stream_name_pse == "LITTLE CAMPBELL RIVER"] <- -122.694413

# SETON RIVER
dat2_updated$stream_survey_method[dat2_updated$stream_name_pse == "SETON RIVER" & dat2_updated$year == 2019] <- "Resistivity Counter"
dat2_updated$source_id[dat2_updated$stream_name_pse == "SETON RIVER" & dat2_updated$year == 2019] <- "Buchanan_20200831"

# BRIDGE RIVER
dat2_updated[dat2_updated$stream_name_pse == "BRIDGE RIVER",] # Note years are currently wrong; no survey in 2016 due to high water (White et al. 2021)
dat2_updated$year[dat2_updated$stream_name_pse == "BRIDGE RIVER"] <- c(2000, 2014, 2015, 2017, 2018, 2019)
dat2_updated$stream_survey_quality[dat2_updated$stream_name_pse == "BRIDGE RIVER" & dat2_updated$year >= 2014] <- c("Medium-High", "Medium-High", "Low", "Low", "Medium")
dat2_updated$source_id[dat2_updated$stream_name_pse == "BRIDGE RIVER" & dat2_updated$year >= 2014] <- "White_20210224"

#----------------------
# Add Thompson Summer tributaries 2017-2021 from IFC updates
# Note: Thompson River still included even though sum of other counts; otherwise time series would end. Prior to 2017 we only have aggregate counts.
bison_tribs <- read.csv("data/InteriorFraser_Bison.csv") %>%
	left_join(cu_stream_lookup) %>%
	mutate(species_name = "Steelhead", species_qualified = "SH", indicator = "Y") %>%
	select(region, species_name, species_qualified, cuid, cu_name_pse, pointid, streamid, stream_name_pse, indicator, latitude, longitude, year, stream_observed_count, stream_survey_method, stream_survey_quality, source_id)

dat2_updated <- dat2_updated %>%
	filter(paste(streamid, year) %in% paste(bison_tribs$streamid, bison_tribs$year) == FALSE) %>% # Use bison_tribs if existing
	bind_rows(bison_tribs)

# Assign new streamid for new survyes
dat2_updated$streamid[dat2_updated$stream_name_pse == "COLDWATER RIVER"] <- streamid_max + 1
dat2_updated$streamid[dat2_updated$stream_name_pse == "SPIUS CREEK"] <- streamid_max + 2

#------------------------------------------------------------------------------
# Vancouver Island
#------------------------------------------------------------------------------

#---------
# ENGLISHMAN RIVER to 2026
#---------
# Import raw data
dat_englishman <- read_xlsx("data/Mcculloch2026/Englishman River adult steelhead through 2026 PSF summary.xlsx", sheet = "Summary Data", range = "D1:E46") %>%
	filter(!is.na(Year))

# Format for dataset 2
dat2_englishman <- data.frame(
	year = dat_englishman$Year,
	stream_observed_count = dat_englishman$`Population Estimate`
) %>%
	# Add survey details (consistent among years)
	mutate(stream_survey_method = "Area Under the Curve",
				 stream_survey_quality = "Medium",
				 source_id = "McCulloch_20260804") %>%
	# Add survey location etc. 
	mutate(dat2 %>% 
							filter(stream_name_pse == "ENGLISHMAN RIVER", year == 1990) %>%
							select(region, species_name, species_qualified, cuid, cu_name_pse, pointid, streamid,	stream_name_pse, indicator, latitude, longitude)) %>%
	# Re-order to match dataste 2
	select(region,	species_name,	species_qualified,	cuid,	cu_name_pse,	pointid,	streamid,	stream_name_pse,	indicator,	latitude,	longitude,	year,	stream_observed_count,	stream_survey_method,	stream_survey_quality,	source_id)

# Fold in
dat2_updated <- dat2_updated %>%
	filter(stream_name_pse != "ENGLISHMAN RIVER") %>% # Remove existing data
	bind_rows(dat2_englishman) %>% # Add in new data
	arrange(cuid, stream_name_pse)

#---------
# GORDON RIVER
#---------
# Import raw data
dat_gordon <- read_xls("data/Mcculloch2026/Gordon SR Counts 1985-2026.xls", sheet = "Gordon Summer", range = "A1:B42") %>%
	filter(!is.na(`Steelhead Observed`))

# Format for dataset 2
dat2_gordon <- data.frame(
	year = as.numeric(dat_gordon$Date),
	stream_observed_count = dat_gordon$`Steelhead Observed`
) %>%
	# Add survey details (consistent among years)
	mutate(stream_survey_method = "Snorkel Survey",
				 stream_survey_quality = "Low",
				 source_id = "McCulloch_20260804") %>%
	# Add survey location etc. 
	mutate(dat2 %>% 
							filter(stream_name_pse == "GORDON RIVER", year == 1990) %>%
							select(region, species_name, species_qualified, cuid, cu_name_pse, pointid, streamid,	stream_name_pse, indicator, latitude, longitude)) %>%
	# Re-order to match dataste 2
	select(region,	species_name,	species_qualified,	cuid,	cu_name_pse,	pointid,	streamid,	stream_name_pse,	indicator,	latitude,	longitude,	year,	stream_observed_count,	stream_survey_method,	stream_survey_quality,	source_id)

# Fold in
dat2_updated <- dat2_updated %>%
	filter(stream_name_pse != "GORDON RIVER") %>% # Remove existing data
	bind_rows(dat2_gordon) %>% # Add in new data
	arrange(cuid, stream_name_pse)

#---------
# GOLD RIVER
#---------
# Import raw data (both Gold and Heber Summer SH are in one dataset)
dat_hebergold <- read_xls("data/Mcculloch2026/Heber SR 1975-2025.xls", sheet = "Gold vs Heber", range = "A2:C54") 
names(dat_hebergold) <- c("Year", "Heber", "Gold")

dat_gold <- dat_hebergold %>%
	select(Year, Gold) %>%
	filter(!is.na(Gold))

# Format for dataset 2
dat2_gold <- data.frame(
	year = dat_gold$Year,
	stream_observed_count = dat_gold$Gold
) %>%
	# Add survey details (consistent among years)
	mutate(stream_survey_method = "Area Under the Curve",
				 stream_survey_quality = "Medium",
				 source_id = "McCulloch_20260804") %>%
	# Add survey location etc. 
	mutate(dat2 %>% 
							filter(stream_name_pse == "GOLD RIVER", year == 2005) %>%
							select(region, species_name, species_qualified, cuid, cu_name_pse, pointid, streamid,	stream_name_pse, indicator, latitude, longitude)) %>%
	# Re-order to match dataste 2
	select(region,	species_name,	species_qualified,	cuid,	cu_name_pse,	pointid,	streamid,	stream_name_pse,	indicator,	latitude,	longitude,	year,	stream_observed_count,	stream_survey_method,	stream_survey_quality,	source_id)

# Quality prior to 2000 was low
dat2_gold$stream_survey_quality[dat2_gold$year < 2000] <- "Low"

# Fold in
dat2_updated <- dat2_updated %>%
	filter(stream_name_pse != "GOLD RIVER") %>% # Remove existing data
	bind_rows(dat2_gold) %>% # Add in new data
	arrange(cuid, stream_name_pse)

#---------
# Heber RIVER
#---------
# Import raw data (both Gold and Heber Summer SH are in one dataset)
dat_heber <- dat_hebergold %>%
	select(Year, Heber) %>%
	filter(!is.na(Heber))

# Format for dataset 2
dat2_heber <- data.frame(
	year = dat_heber$Year,
	stream_observed_count = dat_heber$Heber
) %>%
	# Add survey details (consistent among years)
	mutate(stream_survey_method = "Area Under the Curve",
				 stream_survey_quality = "Medium",
				 source_id = "McCulloch_20260804") %>%
	# Add survey location etc. 
	mutate(dat2 %>% 
							filter(stream_name_pse == "HEBER RIVER", year == 2005) %>%
							select(region, species_name, species_qualified, cuid, cu_name_pse, pointid, streamid,	stream_name_pse, indicator, latitude, longitude)) %>%
	# Re-order to match dataste 2
	select(region,	species_name,	species_qualified,	cuid,	cu_name_pse,	pointid,	streamid,	stream_name_pse,	indicator,	latitude,	longitude,	year,	stream_observed_count,	stream_survey_method,	stream_survey_quality,	source_id)

# Quality prior to 2000 was low
dat2_heber$stream_survey_quality[dat2_heber$year < 2000] <- "Low"

# Fold in
dat2_updated <- dat2_updated %>%
	filter(stream_name_pse != "HEBER RIVER") %>% # Remove existing data
	bind_rows(dat2_heber) %>% # Add in new data
	arrange(cuid, stream_name_pse)

#---------
# TSITIKA RIVER
#---------
# Import raw data 
dat_tsitika <- read_xls("data/Mcculloch2026/Tsitika SR 1976-2025.xls", sheet = "template", range = "B1:C42") %>%
	filter(!is.na(`# Steelhead`))

# Format new data for dataset 2
dat2_tsitika <- data.frame(
	year = dat_tsitika$Year,
	stream_observed_count = dat_tsitika$`# Steelhead`
) %>%
	# Add survey details (consistent among years)
	mutate(stream_survey_method = "Snorkel Survey",
				 stream_survey_quality = "Low",
				 source_id = "McCulloch_20260804") %>%
	# Add survey location etc. 
	mutate(dat2 %>% 
							filter(stream_name_pse == "TSITIKA RIVER", year == 2005) %>%
							select(region, species_name, species_qualified, cuid, cu_name_pse, pointid, streamid,	stream_name_pse, indicator, latitude, longitude)) %>%
	# Re-order to match dataste 2
	select(region,	species_name,	species_qualified,	cuid,	cu_name_pse,	pointid,	streamid,	stream_name_pse,	indicator,	latitude,	longitude,	year,	stream_observed_count,	stream_survey_method,	stream_survey_quality,	source_id)

# Fold in
dat2_updated <- dat2_updated %>%
	filter(stream_name_pse != "TSITIKA RIVER") %>% # Remove existing data
	bind_rows(dat2_tsitika) %>% # Add in new data
	arrange(cuid, stream_name_pse) 

#------------------------------------------------------------------------------
# Skeena
# --------------------------------------------------------

#---------
# Upper Sustut
#---------

# Import raw data 
dat_sustut <- read_xlsx("data/Tyee Test and Upper Sustut Fence_ Steelhead Time Series.xlsx", sheet = "Sustut Fence Time Series", range = "A1:B25") 

# Some differences with existing data...plot?
plot(dat2$year[dat2$stream_name_pse == "UPPER SUSTUT RIVER"], dat2$stream_observed_count[dat2$stream_name_pse == "UPPER SUSTUT RIVER"], "o", col = grey(0.6))
points(dat_sustut$Year, dat_sustut$Count, "o", col = 2)
# Note: older data from SkeenaFisheriesBranch2019UpperSustutRiver
# Needs a source_id

dat2_updated$source_id[dat2_updated$year < 2002 & dat2_updated$stream_name_pse == "UPPER SUSTUT RIVER"] <- "ProvBC_20190801"

# Format new data for dataset 2
dat2_sustut <- data.frame(
	year = dat_sustut$Year,
	stream_observed_count = dat_sustut$Count
) %>%
	# Add survey details (consistent among years)
	mutate(stream_survey_method = "Snorkel Survey",
				 stream_survey_quality = "Low",
				 source_id = "Miyazaki_20260505") %>%
	# Add survey location etc. 
	mutate(dat2 %>% 
							filter(stream_name_pse == "UPPER SUSTUT RIVER", year == 2005) %>%
							select(region, species_name, species_qualified, cuid, cu_name_pse, pointid, streamid,	stream_name_pse, indicator, latitude, longitude)) %>%
	# Re-order to match dataste 2
	select(region,	species_name,	species_qualified,	cuid,	cu_name_pse,	pointid,	streamid,	stream_name_pse,	indicator,	latitude,	longitude,	year,	stream_observed_count,	stream_survey_method,	stream_survey_quality,	source_id)

# Fold in
dat2_updated <- dat2_updated %>%
	filter((stream_name_pse == "UPPER SUSTUT RIVER" & year >=2002) == FALSE) %>% # Remove existing data
	bind_rows(dat2_sustut) %>% # Add in new data
	arrange(cuid, stream_name_pse) 

###############################################################################
# Write output data
###############################################################################
# Check all source_id are entered - yes
sort(unique(dat2_updated$source_id))

# Check no NAs
apply(dat2_updated, 2, function(x)sum(is.na(x)))
dat2_updated[is.na(dat2_updated$region),]

X_Drive <- get_XDrive()

write.csv(dat2_updated, file = "output/dataset2_spawner-surveys_Steelhead.csv", row.names = FALSE)
write.csv(dat2_updated,
					paste0(X_Drive,"1_PROJECTS/1_Active/Population Methods and Analysis/population-data/steelhead-data/output/archive/dataset2_spawner-surveys_Steelhead_", Sys.Date(), ".csv"),
					row.names = FALSE)
