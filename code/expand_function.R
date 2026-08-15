library(tidyr)
expand <- function(spawner_dat){
	# spawner_dat is a dataframe with stream_name_pse, year, stream_observed_count
	# spawner_dat <- dat2 %>% filter(cuid == 981) %>% select(stream_name_pse, year, stream_observed_count)

	dat_wide <- spawner_dat %>%
		arrange(year) %>%
		tidyr::pivot_wider(names_from = stream_name_pse, 
											 values_from = stream_observed_count)
	
	dat_mat <- as.matrix(dat_wide[, 2:dim(dat_wide)[2]])
	dimnames(dat_mat) <- list(dat_wide$year, names(dat_wide)[2:dim(dat_wide)[2]])
		
	avgSpawners <- spawner_dat %>% 
		group_by(stream_name_pse) %>%
		summarise(avg = mean(stream_observed_count, na.rm = TRUE))
	
	# Calculate proportional contribution of each survey to the sum		
	P <- matrix(avgSpawners$avg/sum(avgSpawners$avg), nrow = 1, dimnames = list(NULL, avgSpawners$stream_name_pse))
	
	# Create matrix to indicate when a survey is missing
	w <- !is.na(dat_mat)
	
	# Calculate expansion factor as 1 over the proportional contribution of the missing stream
	E <- apply(matrix(rep(P, dim(w)[1]), nrow = dim(w)[1], ncol=dim(w)[2], byrow = TRUE) * w, 1, sum, na.rm=TRUE) ^ (-1)
	
	
	# Expansions cannot be done when all surveys are missing
	E[which(apply(w, 1, sum) == 0)] <- NA
	
	# Calculate simple sum of observed counts
	dat_wide <- dat_wide %>%
		mutate(simple_sum = case_when(
			apply(dat_mat, 1, function(x)sum(!is.na(x))) == 0 ~ NA,
			.default = apply(dat_mat, 1, sum, na.rm = TRUE))) %>%
		mutate(expansion_factor = E) %>%
		mutate(expanded_index = round(simple_sum * E))
	
	return(dat_wide)
}