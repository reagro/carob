

N2A_monitoring_2 <- function(ff) {	

	fix_crop <- function(p) {
		p[grep("^grou", p, ignore.case=TRUE)] <- "groundnut"	
		p[grep("soy", p, ignore.case=TRUE)] <- "soybean"	
		p[grep("sweet p", p, ignore.case=TRUE)] <- "sweetpotato"	
		p[grep("sweetpot", p, ignore.case=TRUE)] <- "sweetpotato"	
		p <- gsub("tobaco", "tobacco", p)
		p <- gsub("beans", "common bean", p)
		p <- gsub("pumpkins", "pumpkin", p)
		p <- gsub("irish potatoes", "potato", p)
		p <- gsub(" ma$", " maize", p)
		p <- gsub(", ", "; ", p)
		p <- gsub("\\+|/| &|&|,", "; ", p)
		p <- gsub("maize; bean", "maize; common bean", p)
		p <- gsub("fallow", "no crop", p)
		p
	}
	
	# read the data
	bn <- basename(ff)
	d0 <- read.csv(ff[bn == "a_general.csv"])
	d1 <- read.csv(ff[bn == "c_use_of_package_2.csv"])
	d1$id <- d1$SN <- d1$instanceid <- NULL
	d2 <- read.csv(ff[bn == "e_harvest.csv"])
	d2$id <- d2$SN <- d2$instanceid <- NULL
	#f3 <- ff[bn == "c_use_of_package_3.csv"]
	#d3 <- read.csv(f3) 
	d4 <- read.csv(ff[bn == "d_cropping_calendar.csv"])
	d5 <- read.csv(ff[bn == "b_info_site_2.csv"])
	
	#start processing the 1st data
	d <- data.frame(
		country = d0$country, 
		adm2 = carobiner::fix_name(d0$district, "title"), 
		adm3 = carobiner::fix_name(d0$sector_ward, "title"), 
		location = carobiner::fix_name(d0$vilage, "title"), 
		latitude = d0$gps_latitude, 
		longitude = d0$gps_latitude, 
		season = d0$season, 
		farm_id = d0$farm_id
	)

	#subset the variables of interest in d1 and d2	
	dd <- merge(d1, d2, by = c("farm_id", "plot_no"), all.x = TRUE )

	# working on fertilizer types
# Super D and D Compound seem to be the same blend (https://agra.org/wp-content/uploads/2020/08/Malawi-Report_Assessment-of-Fertilizer-Distribution-Systems-and-Opportunities-for-Developing-Fertilizer-Blends.pdf)
# Sympal: https://www.researchgate.net/profile/Charlotte-Schilt/publication/283304707_N2Africa_Final_Report_of_the_first_Phase_-_2009_-_2013/links/5d77729c4585151ee4ab2639/N2Africa-Final-Report-of-the-first-Phase-2009-2013.pdf

	p <- carobiner::fix_name(dd$mineral_fert_type, "upper")
	p[grepl("UREA", p)] <- "urea"
	
	p[grepl("^SYMP", p)] <- "sympal"
	p[grepl("^S ", p)] <- "S-compound"
	p[grepl("^D ", p)] <- "D-compound"
	p[grepl("^S-COM", p)] <- "S-compound"
	p[grepl("^D-COM", p)] <- "D-compound"
	p[grepl("SUPER D", p)] <- "D-compound"
	p[grepl("SINGLE SUPER PHOSPHATE", p)] <- "SSP"
	p[p == "SUPER PHOSPHATE"] <- "SSP"
	p[p %in% c("NONE", "NOON", "NON", "NO")] <- "none"
	
	dd$fertilizer_type <- p
	
##	mineral_fert_amount uses codes 
##  what are codes 0, 1, 2?
##  perhaps one of the codebooks explains that?

	dd$P_fertilizer <- NA
	dd$N_fertilizer <- NA
	dd$K_fertilizer <- NA

## from old script that _may_ be useful but 
## needs to be rewritten to be readable
## too much use of nested ifelse. Instead make a data.frame with type and content
  
  # # Adjusting NPK amounts
  # d1$N_fertilizer <- ifelse(d1$fertilizer_type %in% c("urea"), d1$mineral_fert_amount * 0.46,  # assumption is that mineral fert amount is in kg
                     # ifelse(d1$fertilizer_type == "23:21:0+4S", d1$mineral_fert_amount * 0.23,
                     # ifelse(d1$fertilizer_type %in% c("D compound", "S compound"), d1$mineral_fert_amount * 0.08, NA)))
                     # # ifelse(d1$fertilizer_type %in% c("Super D","Super d"),
                                                 # # d1$mineral_fert_amount * 0.01,NA))))
  
  # d1$P_fertilizer <- ifelse(d1$fertilizer_type == "TSP", d1$mineral_fert_amount * 0.46*((2*31)/(2*31+5*16)),
                     # ifelse(d1$fertilizer_type == "sympal", d1$mineral_fert_amount * 0.23*((2*31)/(2*31+5*16)),
                     # ifelse(d1$fertilizer_type %in% c("urea","unknown","S compound"), d1$mineral_fert_amount * 0.21*((2*31)/(2*31+5*16)),
                     # ifelse(d1$fertilizer_type == "D compound", d1$mineral_fert_amount * 0.18*((2*31)/(2*31+5*16)),
                     # ifelse(d1$fertilizer_type == "SSP", d1$mineral_fert_amount * 0.145, NA)))))
                     # # ifelse(d1$fertilizer_type %in% c("Super D","Super d"), d1$mineral_fert_amount * 0.24*((2*31)/(2*31+5*16)),
                     # # ifelse(d1$fertilizer_type %in% c("Single super phosphate", "Single super phosphate "," Single Super phosphate","Super phosphate"), d1$mineral_fert_amount * 0.145,NA)))))) # takes into account atomic weight of both P and O
  
  # d1$K_fertilizer <- ifelse(d1$fertilizer_type == "sympal", d1$mineral_fert_amount * 0.15,
                     # ifelse(d1$mineral_fert_type == "S compound", d1$mineral_fert_amount * 0.07,
                     # ifelse(d1$fertilizer_type == "D compound", d1$mineral_fert_amount * 0.20, NA)))
  

	dd$OM_applied <- as.numeric(dd$organic_fert_amount)
	dd$OM_used <- dd$organic_fert_amount > 0
	dd$OM_type <- dd$organic_fert_type
	dd$OM_type[!dd$OM_used] <- "none"

	dd$yield <- 10000 * dd$crop_1_weight_grain / dd$crop_1_area_harvested
	
	dd$inoculant_used[dd$inoculant_used == ""] <- NA
	dd$inoculated <- dd$inoculant_used == "Y"
	
	#standardizing the crops and variety
	dd$crop <- fix_crop(dd$crop_1)
	dd$variety <- carobiner::fix_name(dd$variety_1, "title")
	dd$rep <- dd$plot_no
 	
## to do: also deal with crop_2/variety_2 
	dd <- dd[, c("farm_id", "rep", "crop", "variety", "inoculated", "fertilizer_type", "N_fertilizer", "P_fertilizer", "K_fertilizer", "yield", "OM_used", "OM_type", "OM_applied")]
	
	
	#get the dates information
	dd4 <- d4[, "farm_id", drop=FALSE]	

	p <- apply(d4[, c("date_planting_yyyy", "date_planting_mm", "date_planting_dd")], 1, paste, collapse="-")
	dd4$planting_date <- as.character(as.Date(p))

	i <- d4$date_harvest_dd == 0
	j <- d4$date_harvest_yy == 0
	d4$date_harvest_dd[i] <- 15
	h <- apply(d4[, c("date_harvest_yyyy", "date_harvest_mm", "date_harvest_dd")], 1, paste, collapse="-")
	h[j] <- NA

	dd4$harvest_date <- as.character(as.Date(h))


	#standardizing the previous crop variable

	dd5 <- d5[, "farm_id", drop=FALSE]	
	dd5$previous_crop <- fix_crop(carobiner::fix_name(d5$main_crop_last_season, "lower"))
	
	#merge the data sets
	z <- merge(d, dd, by = "farm_id", all.x=TRUE)
	#z <- merge(z, d3, by = "farm_id", all.x=TRUE)
	z <- merge(z, dd4, by = "farm_id", all.x=TRUE)
	z <- merge(z, dd5, by = "farm_id", all.x=TRUE)
	
	z$yield_part <- "seed"
	z$trial_id <- z$farm_id
	z$farm_id <- NULL
	
	z
}





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
		adm2 = carobiner::fix_name(d0$action_site, "Title"), 
		adm3 = carobiner::fix_name(d0$sector_ward, "Title"), 
		location = carobiner::fix_name(d0$vilage, "Title"), 
		latitude = d0$gps_latitude, 
		longitude = d0$gps_latitude, 
		season = d0$season, 
		farm_id= d0$farm_id
	)

	#subset the variables of interest in d1 and d2	
	d1 <- d1[, c("farm_id", "plot_no", "crop", "variety", "inoculant_used", "min_fertilizer_type", "min_fertiliser_amount_kg", "org_fertilizer_type", "org_fertilizer_amount_kg")]
	d2 <- d2[, c("farm_id", "plot_no", "area_harvested_m2", "weight_kg")]
	dd <- merge(d1, d2, by = c("farm_id", "plot_no"), all.x = TRUE )
	
	# working on fertilizer types
	dd$min_fertilizer_type[dd$min_fertilizer_type %in% c("SSP/Urea", "SSP+Ureia", "SSP+Urea", "Urea+SSP")] <- "SSP; urea"	
	dd$min_fertilizer_type[dd$min_fertilizer_type %in% c("ssp", "Phosphor(SSP)", "SSP+Inoc", "Y", "12")] <- "SSP"
	dd$min_fertilizer_type[dd$min_fertilizer_type %in% c("Ureia", "Urea")] <- "urea"
	dd$min_fertilizer_type[dd$min_fertilizer_type %in% c( "None", "", "N")] <- "none"
	
	#working on fertilizer amounts
	dd$min_fertiliser_amount_kg[dd$min_fertiliser_amount_kg %in% c("2/0.4", "2 /0.4", "2/0,4", "2/ 0,4")] <- "2/0.4"
	dd$min_fertiliser_amount_kg <- carobiner::replace_values(dd$min_fertiliser_amount_kg, c("2kg/ha", "", "N"), c("2", "0", "0"))
	
	#split fertilizer amount column to separate urea amounts and SSP amounts
	dd$ssp_amt <- ifelse(dd$min_fertilizer_type != "urea", as.numeric(sub("/.*", "", dd$min_fertiliser_amount_kg)), 0)
	dd$urea_amt <- ifelse(dd$min_fertilizer_type == "urea", as.numeric(sub(".*?/", "", dd$min_fertiliser_amount_kg)), 0)
	dd$fertilizer_type <- dd$min_fertilizer_type
	#to get rates of N and P
	dd$P_rate_plot <-dd$ssp_amt*0.16
	
	# for 10.25502/hwdb-p578
	#v <- carobiner::replace_values(dd$area_harvested_m2, c(101, 102, 103, 104), 							c(100, 100, 100, 100))

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
	dd$rep <- dd$plot_no

## FIX NEEDED: ALSO USE organic_fert_type"  "organic_fert_amount
 	
	dd <- dd[, c("farm_id", "rep", "crop", "variety", "inoculated", "fertilizer_type", "N_fertilizer", "P_fertilizer", "K_fertilizer", "yield")]
	
	#get the spacing information
	dd3 <- d3[, "farm_id", drop=FALSE]
	dd3$row_spacing <- as.numeric(d3$crop_1_spacing_row_to_row)
	dd3$plant_spacing <- as.numeric(d3$crop_1_spacing_plant_to_plant)
	
	dd4 <- d4[, "farm_id", drop=FALSE]
	p <- d4[grepl("planting", d4$activity), ]
	h <- d4[grepl("harvest", d4$activity), ]
	p$planting_date <- with(p, paste(date_planting_yyyy, date_planting_mm, date_planting_dd, sep = "-"))
	h$harvest_date <- with(h, paste(date_planting_yyyy, date_planting_mm, date_planting_dd, sep = "-"))
	p$planting_date[p$date_planting_yyyy == 0] <- NA
	h$harvest_date[h$date_planting_yyyy == 0] <- NA

	p <- p[, c("farm_id", "planting_date")]
	h <- h[, c("farm_id", "harvest_date")]
	ph <- merge(p, h, by = "farm_id", all=TRUE)
	dd4 <- merge(dd4, ph, by = "farm_id", all.x=TRUE)
	
	dd4$planting_date <- as.character(as.Date(dd4$planting_date, format = "%Y-%m-%d"))
	dd4$harvest_date <- as.character(as.Date(dd4$harvest_date, format = "%Y-%m-%d"))
	
	#standardizing the previous crop variable
	dd5 <- d5[, "farm_id", drop=FALSE]
	dd5$previous_crop <- fix_crop(carobiner::fix_name(d5$main_crop_last_season, "lower"))

	
	#merge the data sets
	z <- merge(d, dd, by = "farm_id", all.x=TRUE)
	z <- merge(z, d3, by = "farm_id", all.x=TRUE)
	z <- merge(z, dd4, by = "farm_id", all.x=TRUE)
	z <- merge(z, dd5, by = "farm_id", all.x=TRUE)
	
	z$yield_part <- ifelse(z$crop == "groundnut", "pod", "seed")
	z$trial_id <- z$farm_id
	z$farm_id <- NULL
	z
}
