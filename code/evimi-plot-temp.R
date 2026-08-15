# Compare
y <- expanded_dat_981 %>%
	left_join(dat1 %>% filter(cuid == 981)) %>% 
	select(year, observed_count, simple_sum,estimated_count,  expanded_index, expansion_factor)
plot(y$year, y$expanded_index, "n", las = 1, xlab = "", ylab = "Abundance", bty = "l", yaxs = "i", ylim = c(0, 2000))
segments(x0 = expanded_dat_981$year,
				 x1 = expanded_dat_981$year,
				 y0 = rep(0, length(expanded_dat_981$year)),
				 y1 = expanded_dat_981$simple_sum,
				 col = grey(0.3),
				 lend = 1, lwd = 5
)
segments(x0 = expanded_dat_981$year,
				 x1 = expanded_dat_981$year,
				 y0 = rep(0, length(expanded_dat_981$year)),
				 y1 = apply(cbind(expanded_dat_981$`KEOGH RIVER`, expanded_dat_981$`ENGLISHMAN RIVER`), 1, sum, na.rm = TRUE),
				 col = grey(0.8),
				 lend = 1, lwd = 5
)
segments(x0 = expanded_dat_981$year,
				 x1 = expanded_dat_981$year,
				 y0 = rep(0, length(expanded_dat_981$year)),
				 y1 = expanded_dat_981$`KEOGH RIVER`,
				 col = grey(0.5),
				 lend = 1, lwd = 5
)
legend("topright", fill = c(grey(c(0.1, 0.8, 0.5)), NA, NA, NA), pch = c(NA, NA, NA, 19, 19, 19), border = NA, legend = c("SALMON RIVER", "ENGLISHMAN RIVER", "KEOGH RIVER", "PSE observed count", "PSE estimated count", "Steph's expansion"), col =c(NA, NA, NA, "#c17d44", "#6079b1", 1))

points(y$year, y$observed_count, col = "#c17d44", pch = 19, cex = 0.8)
points(y$year, y$estimated_count, "o", col = "#6079b1", pch = 19, cex = 0.8)
points(y$year, y$expanded_index, "o")

#########################################################################################
##################################
# Compare
y <- expanded_dat_984 %>%
	left_join(dat1 %>% filter(cuid == 984)) %>% 
	select(year, observed_count, simple_sum,estimated_count,  expanded_index, expansion_factor)
plot(y$year, y$expanded_index, "n", las = 1, xlab = "", ylab = "Abundance", bty = "l", yaxs = "i", ylim = c(0, 1600))
segments(x0 = expanded_dat_984$year,
				 x1 = expanded_dat_984$year,
				 y0 = rep(0, length(expanded_dat_984$year)),
				 y1 = expanded_dat_984$simple_sum,
				 col = grey(0.3),
				 lend = 1, lwd = 5
)
segments(x0 = expanded_dat_984$year,
				 x1 = expanded_dat_984$year,
				 y0 = rep(0, length(expanded_dat_984$year)),
				 y1 = apply(cbind(expanded_dat_984$`GORDON RIVER`, expanded_dat_984$`HEBER RIVER`), 1, sum, na.rm = TRUE),
				 col = grey(0.8),
				 lend = 1, lwd = 5
)
segments(x0 = expanded_dat_984$year,
				 x1 = expanded_dat_984$year,
				 y0 = rep(0, length(expanded_dat_984$year)),
				 y1 = expanded_dat_984$`GORDON RIVER`,
				 col = grey(0.5),
				 lend = 1, lwd = 5
)
legend("topright", fill = c(grey(c(0.1, 0.8, 0.5)), NA, NA, NA), pch = c(NA, NA, NA, 19, 19, 19), border = NA, legend = c("GOLD RIVER", "GORDON RIVER", "HEBER RIVER", "PSE observed count", "PSE estimated count", "Steph's expansion"), col =c(NA, NA, NA, "#c17d44", "#6079b1", 1))

points(y$year, y$observed_count, col = "#c17d44", pch = 19, cex = 0.8)
points(y$year, y$estimated_count, "o", col = "#6079b1", pch = 19, cex = 0.8)
points(y$year, y$expanded_index, "o", xpd = NA)

