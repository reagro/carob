
N2A_monitoring_1 <- function(ff) {	

	fix_crop <- function(p) {
		p[grep("^grou", p, ignore.case=TRUE)] <- "groundnut"	
		p[grep("soy", p, ignore.case=TRUE)] <- "soybean"	
		p[grep("sweet pot", p, ignore.case=TRUE)] <- "sweetpotato"	
		p <- gsub("tobaco", "tobacco", p)
		p <- gsub("beans", "common bean", p)
		p <- gsub("irish potatoes", "potato", p)
		p <- gsub(" ma$", " maize", p)
		p <- gsub(", ", "; ", p)
		p
	}

	# read the data
	bn <- basename(ff)
	f0 <- ff[bn == "a_general.csv"]
	d0 <- read.csv(f0)
	
	f1 <- ff[bn == "c_use_of_package_1.csv"]
	d1 <- read.csv(f1)
	
	f2 <- ff[bn == "e_harvest.csv"]
	d2 <- read.csv(f2)
	
	f3 <- ff[bn == "c_use_of_package_3.csv"]
	d3 <- read.csv(f3) 
	
	f4 <- ff[bn == "d_cropping_calendar.csv"]
	d4 <- read.csv(f4)
	
	f5 <- ff[bn == "b_info_site_2.csv"]
	d5 <- read.csv(f5)
	
	#start processing the 1st data
	d <- data.frame(
		country = d0$country,
		latitude = d0$gps_latitude,
		longitude = d0$gps_latitude,
		season = d0$season,
		trial_id= d0$farm_id
	)

	if (!is.null(d0$action_site)) {
		d$adm2 <- carobiner::fix_name(d0$action_site, "Title")
	} else {
		d$adm2 <- carobiner::fix_name(d0$district, "Title")	
	}
	d$adm3 <- carobiner::fix_name(d0$sector_ward, "Title")
	d$location <- carobiner::fix_name(d0$vilage, "Title")
	
## ALSO USE organic_fert_type"  "organic_fert_amount
	
	#subset the variables of interest in d1 and d2	
	d1 <- d1[, c("farm_id","plot_no","crop", "variety", "inoculant_used","min_fertilizer_type","min_fertiliser_amount_kg")]
	d2 <- d2[, c("farm_id","plot_no","area_harvested_m2","weight_kg")]
	dd <- merge(d1, d2, by = c("farm_id", "plot_no"), all.x = TRUE )
	
	# working on fertilizer types
	dd$min_fertilizer_type[dd$min_fertilizer_type %in% c("SSP/Urea", "SSP+Ureia", "SSP+Urea", "Urea+SSP")] <- "SSP; urea"	
	dd$min_fertilizer_type[dd$min_fertilizer_type %in% c("ssp", "Phosphor(SSP)", "SSP+Inoc","Y","12")] <- "SSP"
	dd$min_fertilizer_type[dd$min_fertilizer_type %in% c("Ureia", "Urea")] <- "urea"
	dd$min_fertilizer_type[dd$min_fertilizer_type %in% c( "None", "", "N")] <- "none"
	
	#working on fertilizer amounts
	dd$min_fertiliser_amount_kg[dd$min_fertiliser_amount_kg %in% c("2/0.4","2 /0.4","2/0,4","2/ 0,4")] <- "2/0.4"
	dd$min_fertiliser_amount_kg <- carobiner::replace_values(dd$min_fertiliser_amount_kg, c("2kg/ha", "", "N"), c("2", "0", "0"))

	
	#split fertilizer amount column to separate urea amounts and SSP amounts
	dd$ssp_amt <- ifelse(dd$min_fertilizer_type != "urea", as.numeric(sub("/.*", "", dd$min_fertiliser_amount_kg)), 0)
	dd$urea_amt <- ifelse(dd$min_fertilizer_type == "urea", as.numeric(sub(".*?/", "", dd$min_fertiliser_amount_kg)), 0)
	dd$fertilizer_type <- dd$min_fertilizer_type
	#to get rates of N and P
	dd$P_rate_plot <-dd$ssp_amt*0.16
	
	# for 10.25502/hwdb-p578
	#v <- carobiner::replace_values(dd$area_harvested_m2, c(101,102,103,104),							c(100,100,100,100))

	## how so ???
	#v[is.na(v)] <- 100
	#dd$area_harvested_m2 <-v
	
	dd$P_fertilizer <- (10000/dd$area_harvested_m2) * dd$P_rate_plot
	dd$N_rate_plot <- dd$urea_amt*0.467
	dd$N_fertilizer <-(10000/dd$area_harvested_m2) * dd$N_rate_plot
	dd$K_fertilizer <- 0
	#getting the yield
	dd$yield <- (10000/dd$area_harvested_m2) * dd$weight_kg
	
	#correcting mismatched rows in inoculated
	dd$inoculated <- dd$inoculant_used
	
	#standardizing the crops and variety
	dd$crop <- fix_crop(dd$crop)
	dd$variety <- carobiner::fix_name(dd$variety, "title")
	dd$trial_id <- dd$farm_id
	dd$rep <- dd$plot_no
 	
	dd <- dd[, c("trial_id","rep","crop","variety","inoculated","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","yield")]
	
	#get the spacing information
	d3$trial_id <- d3$farm_id
	d3$row_spacing <- as.numeric(d3$crop_1_spacing_row_to_row)
	d3$plant_spacing <- as.numeric(d3$crop_1_spacing_plant_to_plant)

	d3 <- d3[, c("trial_id","row_spacing","plant_spacing")]
	
	#standardizing the previous crop variable
	d5$trial_id <- d5$farm_id
	d5$previous_crop <- fix_crop(carobiner::fix_name(d5$main_crop_last_season, "lower"))

	d5 <- d5[, c("trial_id","previous_crop")]
	
	#get the dates information
	d4$trial_id <- d4$farm_id

	#h <-	subset(d4, grepl("harvest", activity))
	
	p <- d4[grepl("planting", d4$activity), ]
	h <- d4[grepl("harvest", d4$activity), ]
	p$planting_date <- with(p, paste(date_planting_yyyy, date_planting_mm, date_planting_dd, sep = "-"))
	h$harvest_date <- with(h, paste(date_planting_yyyy, date_planting_mm, date_planting_dd, sep = "-"))
	p <- p[, c("trial_id", "planting_date")]
	h <- h[, c("trial_id", "harvest_date")]
	d4 <- merge(p,h, by = "trial_id")
	
	d4$planting_date[d4$planting_date == "0-0-0"] <- NA
	d4$planting_date[d4$harvest_date == "0-0-0"] <- NA
	d4$planting_date <- as.character(as.Date(d4$planting_date, format = "%Y-%m-%d"))
	d4$harvest_date <- as.character(as.Date(d4$harvest_date, format = "%Y-%m-%d"))
	
	
	#merge the data sets
	q1 <- merge(d, dd, by = "trial_id", all.x=TRUE)
	q2 <- merge(q1, d3, by = "trial_id", all.x=TRUE)
	q3 <- merge(q2, d4, by = "trial_id", all.x=TRUE)
	q4 <- merge(q3, d5, by = "trial_id", all.x=TRUE)
	
	q4$yield_part <- ifelse(q4$crop == "groundnut", "pod", "seed")
	
	q4
}


N2A_monitoring_2 <- function(ff) {	

	fix_crop <- function(p) {
		p[grep("^grou", p, ignore.case=TRUE)] <- "groundnut"	
		p[grep("soy", p, ignore.case=TRUE)] <- "soybean"	
		p[grep("sweet pot", p, ignore.case=TRUE)] <- "sweetpotato"	
		p <- gsub("tobaco", "tobacco", p)
		p <- gsub("beans", "common bean", p)
		p <- gsub("irish potatoes", "potato", p)
		p <- gsub(" ma$", " maize", p)
		p <- gsub(", ", "; ", p)
		p
	}

	# read the data
	bn <- basename(ff)
	f0 <- ff[bn == "a_general.csv"]
	d0 <- read.csv(f0)
	
	f1 <- ff[bn == "c_use_of_package_2.csv"]
	d1 <- read.csv(f1)
	
	f2 <- ff[bn == "e_harvest.csv"]
	d2 <- read.csv(f2)
	
	f3 <- ff[bn == "c_use_of_package_3.csv"]
	d3 <- read.csv(f3) 
	
	f4 <- ff[bn == "d_cropping_calendar.csv"]
	d4 <- read.csv(f4)
	
	f5 <- ff[bn == "b_info_site_2.csv"]
	d5 <- read.csv(f5)
	
	#start processing the 1st data
	d <- data.frame(
		country = d0$country,
		latitude = d0$gps_latitude,
		longitude = d0$gps_latitude,
		season = d0$season,
		trial_id= d0$farm_id
	)

	if (!is.null(d0$action_site)) {
		d$adm2 <- carobiner::fix_name(d0$action_site, "Title")
	} else {
		d$adm2 <- carobiner::fix_name(d0$district, "Title")	
	}
	d$adm3 <- carobiner::fix_name(d0$sector_ward, "Title")
	d$location <- carobiner::fix_name(d0$vilage, "Title")
	
## ALSO USE organic_fert_type"  "organic_fert_amount
	
	#subset the variables of interest in d1 and d2	
	d1 <- d1[, c("farm_id","plot_no","crop", "variety", "inoculant_used","min_fertilizer_type","min_fertiliser_amount_kg")]
	d2 <- d2[, c("farm_id","plot_no","area_harvested_m2","weight_kg")]
	dd <- merge(d1, d2, by = c("farm_id", "plot_no"), all.x = TRUE )
	
	# working on fertilizer types
	dd$min_fertilizer_type[dd$min_fertilizer_type %in% c("SSP/Urea", "SSP+Ureia", "SSP+Urea", "Urea+SSP")] <- "SSP; urea"	
	dd$min_fertilizer_type[dd$min_fertilizer_type %in% c("ssp", "Phosphor(SSP)", "SSP+Inoc","Y","12")] <- "SSP"
	dd$min_fertilizer_type[dd$min_fertilizer_type %in% c("Ureia", "Urea")] <- "urea"
	dd$min_fertilizer_type[dd$min_fertilizer_type %in% c( "None", "", "N")] <- "none"
	
	#working on fertilizer amounts
	dd$min_fertiliser_amount_kg[dd$min_fertiliser_amount_kg %in% c("2/0.4","2 /0.4","2/0,4","2/ 0,4")] <- "2/0.4"
	dd$min_fertiliser_amount_kg <- carobiner::replace_values(dd$min_fertiliser_amount_kg, c("2kg/ha", "", "N"), c("2", "0", "0"))

	
	#split fertilizer amount column to separate urea amounts and SSP amounts
	dd$ssp_amt <- ifelse(dd$min_fertilizer_type != "urea", as.numeric(sub("/.*", "", dd$min_fertiliser_amount_kg)), 0)
	dd$urea_amt <- ifelse(dd$min_fertilizer_type == "urea", as.numeric(sub(".*?/", "", dd$min_fertiliser_amount_kg)), 0)
	dd$fertilizer_type <- dd$min_fertilizer_type
	#to get rates of N and P
	dd$P_rate_plot <-dd$ssp_amt*0.16
	
	# for 10.25502/hwdb-p578
	#v <- carobiner::replace_values(dd$area_harvested_m2, c(101,102,103,104),							c(100,100,100,100))

	## how so ???
	#v[is.na(v)] <- 100
	#dd$area_harvested_m2 <-v
	
	dd$P_fertilizer <- (10000/dd$area_harvested_m2) * dd$P_rate_plot
	dd$N_rate_plot <- dd$urea_amt*0.467
	dd$N_fertilizer <-(10000/dd$area_harvested_m2) * dd$N_rate_plot
	dd$K_fertilizer <- 0
	#getting the yield
	dd$yield <- (10000/dd$area_harvested_m2) * dd$weight_kg
	
	#correcting mismatched rows in inoculated
	dd$inoculated <- dd$inoculant_used
	
	#standardizing the crops and variety
	dd$crop <- fix_crop(dd$crop)
	dd$variety <- carobiner::fix_name(dd$variety, "title")
	dd$trial_id <- dd$farm_id
	dd$rep <- dd$plot_no
 	
	dd <- dd[, c("trial_id","rep","crop","variety","inoculated","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","yield")]
	
	#get the spacing information
	d3$trial_id <- d3$farm_id
	d3$row_spacing <- as.numeric(d3$crop_1_spacing_row_to_row)
	d3$plant_spacing <- as.numeric(d3$crop_1_spacing_plant_to_plant)

	d3 <- d3[, c("trial_id","row_spacing","plant_spacing")]
	
	#standardizing the previous crop variable
	d5$trial_id <- d5$farm_id
	d5$previous_crop <- fix_crop(carobiner::fix_name(d5$main_crop_last_season, "lower"))

	d5 <- d5[, c("trial_id","previous_crop")]
	
	#get the dates information
	d4$trial_id <- d4$farm_id

	#h <-	subset(d4, grepl("harvest", activity))
	
	p <- d4[grepl("planting", d4$activity), ]
	h <- d4[grepl("harvest", d4$activity), ]
	p$planting_date <- with(p, paste(date_planting_yyyy, date_planting_mm, date_planting_dd, sep = "-"))
	h$harvest_date <- with(h, paste(date_planting_yyyy, date_planting_mm, date_planting_dd, sep = "-"))
	p <- p[, c("trial_id", "planting_date")]
	h <- h[, c("trial_id", "harvest_date")]
	d4 <- merge(p,h, by = "trial_id")
	
	d4$planting_date[d4$planting_date == "0-0-0"] <- NA
	d4$planting_date[d4$harvest_date == "0-0-0"] <- NA
	d4$planting_date <- as.character(as.Date(d4$planting_date, format = "%Y-%m-%d"))
	d4$harvest_date <- as.character(as.Date(d4$harvest_date, format = "%Y-%m-%d"))
	
	
	#merge the data sets
	q1 <- merge(d, dd, by = "trial_id", all.x=TRUE)
	q2 <- merge(q1, d3, by = "trial_id", all.x=TRUE)
	q3 <- merge(q2, d4, by = "trial_id", all.x=TRUE)
	q4 <- merge(q3, d5, by = "trial_id", all.x=TRUE)
	
	q4$yield_part <- ifelse(q4$crop == "groundnut", "pod", "seed")
	
	q4
}

