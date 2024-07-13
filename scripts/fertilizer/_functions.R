
get_elements_from_product <- function(fertab, products) {
	used <- unique(products) |> strsplit("; ") |> unlist() |> na.omit() |> unique() 	
	stopifnot(!all(fertab$name %in% used))
	fertab <- fertab[fertab$name %in% used, c("name", "N", "P", "K", "S")]
	fmat <- as.matrix(fertab[,-1]) / 100
	out <- matrix(0, ncol=4, nrow=length(products))
	colnames(out) <- paste0(c("N", "P", "K", "S"), "_fertilizer")
	for (fertilizer in used) {
		f <- fmat[fertab$name==fertilizer, ]		
		stopifnot(!any(is.na(f)))
		i <- grep(fertilizer, products)
		out[i, ] <- out[i, ] + rep(f, each=length(i))
	}
	out[is.na(products)] <- NA
	out
}



N2A_monitoring_2 <- function(ff, path) {	

	fix_crop <- function(p) {
	
		p[p=="busbean"] <- "bush bean"	
		p[p=="bushbean"] <- "bush bean"	
		p[p=="oignon"] <- "onion"	
	
		p[grep("^grou", p, ignore.case=TRUE)] <- "groundnut"	
		p[p=="grundnut"] <- "groundnut"	
		p[p=="grungnut"] <- "groundnut"	
		p[grep("soja", p, ignore.case=TRUE)] <- "soybean"	
		p[grep("soy", p, ignore.case=TRUE)] <- "soybean"	
		p[grep("sweet p", p, ignore.case=TRUE)] <- "sweetpotato"	
		p[grep("sweetp", p, ignore.case=TRUE)] <- "sweetpotato"	
		p[grep("swetpot", p, ignore.case=TRUE)] <- "sweetpotato"	
		p <- gsub("n/a", NA, p)
		p <- gsub("tobaco", "tobacco", p)
		p <- gsub("beans", "common bean", p)
		p <- gsub("pumpkins", "pumpkin", p)
		p <- gsub("irish potatoes", "potato", p)
		p <- gsub("irish potato", "potato", p)
		p <- gsub("patatoe", "potato", p)
		p <- gsub("patato", "potato", p)
		p <- gsub(" ma$", " maize", p)
		p <- gsub(", ", "; ", p)
		p <- gsub(" ;", ";", p)
		p <- gsub(" and ", "; ", p)
		p <- gsub("\\+|/| &|&|,", "; ", p)
		p <- gsub("maize; bean", "maize; common bean", p)
		p <- gsub("farrow", "no crop", p)
		p <- gsub("fallow", "no crop", p)
		p <- gsub("pegion pea", "pigeon pea", p)
		p <- gsub("groundnuts", "groundnut", p)
		p <- gsub("local maize", "groundnut", p)
		p <- gsub("fingermillet", "finger millet", p)
		p <- gsub("amaranthas", "amaranth", p)
		p <- gsub("amaranthus", "amaranth", p)
		p <- gsub("tomatoes", "tomato", p)
		p <- gsub("green amarantha", "amaranth", p)
		p <- gsub("rice upland", "rice", p)
		p <- gsub("kales", "kale", p)
		p <- gsub(" intercrop", "", p)
		p <- gsub("cowpeas", "cowpea", p)
		p <- gsub("simsim", "sesame", p)
		p <- gsub("sugar cane", "sugarcane", p)
		p <- gsub(" ;", ";", p)
		p <- gsub("  ", " ", p)
		trimws(p)

	}
	
	# read the data
	bn <- basename(ff)
	r0 <- read.csv(ff[bn == "a_general.csv"])
	r1 <- read.csv(ff[bn == "c_use_of_package_2.csv"])
	r1$SN <- r1$instanceid <- NULL
	r2 <- read.csv(ff[bn == "e_harvest.csv"])
	r2$SN <- r2$instanceid <- NULL
	#f3 <- ff[bn == "c_use_of_package_3.csv"]
	#r3 <- read.csv(f3) 
	r4 <- read.csv(ff[bn == "d_cropping_calendar.csv"])
	r5 <- read.csv(ff[bn == "b_info_site_2.csv"])
	
	#start processing the 1st data
	d <- data.frame(
		country = r0$country, 
		adm2 = carobiner::fix_name(r0$district, "title"), 
		adm3 = carobiner::fix_name(r0$sector_ward, "title"), 
		location = carobiner::fix_name(r0$vilage, "title"), 
		latitude = r0$gps_latitude, 
		longitude = r0$gps_latitude, 
		season = r0$season, 
		farm_id = r0$farm_id
	)



#farm_id plot_no crop_1_area_harvested crop_1_plants_no crop_1_weight_stover crop_1_weight_grain crop_1_grain_unshelled

	dd <- merge(r1, r2, by = c("id", "farm_id", "plot_no"), all.x = TRUE )

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
	p[p == "FERTILIZER"] <- NA
	p[p == "23:21:0+4S"] <- NA
	p[p == "0.972916667"] <- NA
	
	dd$fertilizer_type <- p

## it is not clear what the quantities refer to if there are multiple products 
## that much of each?	
	ftab <- carobiner::accepted_values("fertilizer_type")
## NPK is undefined need to check of 20-20-20 is a good guess
	ftab[ftab$name=="NPK", c("N", "P", "K", "S")] <- c(20, 20, 20, 0)	
	get_elements <- carobiner::get_function("get_elements_from_product", path, group)
	elements <- get_elements(ftab, p)
	
	dd <- cbind(dd, elements)
  
	dd$OM_amount <- as.numeric(dd$organic_fert_amount)
	dd$OM_used <- dd$organic_fert_amount > 0
	dd$OM_type <- carobiner::fix_name(dd$organic_fert_type, "tolower")
	dd$OM_type[!dd$OM_used] <- "none"

	dd$yield <- 10000 * dd$crop_1_weight_grain / dd$crop_1_area_harvested
	
	dd$inoculant_used[dd$inoculant_used == ""] <- NA
	dd$inoculated <- dd$inoculant_used == "Y"
	
	#standardizing the crops and variety
	dd$variety_type <- NA
	dd$crop <- fix_crop(dd$crop_1)
	i <- dd$crop == "bush bean"
	dd$crop[i] <- "common bean"
	dd$variety_type[i] <- "bush bean"
	i <- dd$crop == "climbing bean"
	dd$crop[i] <- "common bean"
	dd$variety_type[i] <- "climbing bean"

	dd$variety <- carobiner::fix_name(dd$variety_1, "title")
	dd$rep <- dd$plot_no
 	
## to do: also deal with crop_2/variety_2 
	dd <- dd[, c("id", "farm_id", "rep", "crop", "variety", "inoculated", "fertilizer_type", "N_fertilizer", "P_fertilizer", "K_fertilizer", "yield", "OM_used", "OM_type", "OM_amount")]
	
	#get the dates information
	if (!is.null(r4$date_planting_yyyy)) {
		dd4 <- r4[, "farm_id", drop=FALSE]	
		p <- apply(r4[, c("date_planting_yyyy", "date_planting_mm", "date_planting_dd")], 1, paste, collapse="-")
		dd4$planting_date <- as.character(as.Date(p))

		i <- r4$date_harvest_dd == 0
		j <- r4$date_harvest_yy == 0
		r4$date_harvest_dd[i] <- 15
		h <- apply(r4[, c("date_harvest_yyyy", "date_harvest_mm", "date_harvest_dd")], 1, paste, collapse="-")
		h[j] <- NA

		dd4$harvest_date <- as.character(as.Date(h))
	} else {
	## cannot merge without plot_id!!
#		pd <- r4[r4$activity == 'Date of planting', ]
#		pd$planting_date <- apply(pd[, c("yyyy", "mm", "dd")], 1, paste, collapse="_")
#		hd <- r4[r4$activity == 'Date of harvest', ]
#		hd$harvest_date <- apply(hd[, c("yyyy", "mm", "dd")], 1, paste, collapse="_")
#		v <- c("id", "farm_id")
#		m <- merge(pd[,c(v, "planting_date")], hd[,c(v, "harvest_date")], by=v)
		dd4 <- NULL
	}

	#standardizing the previous crop variable

	dd5 <- r5[, "farm_id", drop=FALSE]	
	dd5$previous_crop <- fix_crop(carobiner::fix_name(r5$main_crop_last_season, "lower"))
	
	#merge the data sets
	z <- merge(dd, d, by = "farm_id", all.x=TRUE)
	#z <- merge(z, d3, by = "farm_id", all.x=TRUE)
	z <- merge(z, dd4, by = "farm_id", all.x=TRUE)
	z <- merge(z, dd5, by = "farm_id", all.x=TRUE)
	
	z$yield_part <- "seed"
	z$trial_id <- z$farm_id
	z$farm_id <- NULL
	z$id <- NULL
	
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
	r0 <- read.csv(f0)
	
	f1 <- ff[bn == "c_use_of_package_1.csv"]
	d1 <- read.csv(f1)
	
	f2 <- ff[bn == "e_harvest.csv"]
	d2 <- read.csv(f2)
	
	f3 <- ff[bn == "c_use_of_package_3.csv"]
	d3 <- read.csv(f3) 

	f4 <- ff[bn == "d_cropping_calendar.csv"]
	r4 <- read.csv(f4)
	
	f5 <- ff[bn == "b_info_site_2.csv"]
	d5 <- read.csv(f5)
	
	#start processing the 1st data
	d <- data.frame(
		country = r0$country, 
		adm2 = carobiner::fix_name(r0$action_site, "Title"), 
		adm3 = carobiner::fix_name(r0$sector_ward, "Title"), 
		location = carobiner::fix_name(r0$vilage, "Title"), 
		latitude = r0$gps_latitude, 
		longitude = r0$gps_latitude, 
		season = r0$season, 
		farm_id= r0$farm_id
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
	dd$P_rate_plot <- dd$ssp_amt*0.16
	
	# for 10.25502/hwdb-p578
	#v <- carobiner::replace_values(dd$area_harvested_m2, c(101, 102, 103, 104), 							c(100, 100, 100, 100))

	## how so ???
	#v[is.na(v)] <- 100
	#dd$area_harvested_m2 <- v
	
	dd$P_fertilizer <- (10000/dd$area_harvested_m2) * dd$P_rate_plot
	dd$N_rate_plot <- dd$urea_amt*0.467
	dd$N_fertilizer <- (10000/dd$area_harvested_m2) * dd$N_rate_plot
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
	dd3 <- unique(dd3)
	
	dd4 <- unique(r4[, "farm_id", drop=FALSE])
	p <- r4[grepl("planting", r4$activity), ]
	h <- r4[grepl("harvest", r4$activity), ]
	p$planting_date <- with(p, paste(date_planting_yyyy, date_planting_mm, date_planting_dd, sep = "-"))
	h$harvest_date <- with(h, paste(date_planting_yyyy, date_planting_mm, date_planting_dd, sep = "-"))
	p$planting_date[p$date_planting_yyyy == 0] <- NA
	h$harvest_date[h$date_planting_yyyy == 0] <- NA

	p <- p[, c("farm_id", "planting_date")]
	h <- h[, c("farm_id", "harvest_date")]
	ph <- unique(merge(p, h, by = "farm_id", all=TRUE))
	dd4 <- merge(dd4, ph, by = "farm_id", all.x=TRUE)
	
	dd4$planting_date <- as.character(as.Date(dd4$planting_date, format = "%Y-%m-%d"))
	dd4$harvest_date <- as.character(as.Date(dd4$harvest_date, format = "%Y-%m-%d"))
	
	#standardizing the previous crop variable
	dd5 <- d5[, "farm_id", drop=FALSE]
	dd5$previous_crop <- fix_crop(carobiner::fix_name(d5$main_crop_last_season, "lower"))
	
	#merge the data sets
	z <- merge(d, dd, by = "farm_id", all.x=TRUE)
	z <- merge(z, dd3, by = "farm_id", all.x=TRUE)
	z <- merge(z, dd4, by = "farm_id", all.x=TRUE)
	z <- merge(z, dd5, by = "farm_id", all.x=TRUE)
	
	z$yield_part <- ifelse(z$crop == "groundnut", "pod", "seed")
	z$trial_id <- z$farm_id
	z$farm_id <- NULL
	z
}
