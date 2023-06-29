#################################################################################
#N2Africa was aimed at increasing biological nitrogen fixation and productivity
#of grain legumes through effective production technologies including inoculants
#and fertilizers adapted to local settings which was aimed at increasing soil
#fertility.The trails were conducted in 11 African countries
#################################################################################

carob_script <- function(path){
 
	uri <- "doi:10.25502/hwdb-p578"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	 
	#dataset level data
	
	dset <- data.frame(
		dataset_id = dataset_id,
		group = group,
		uri = uri,
        project="N2Africa",
		publication = NA,
		data_citation ="Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, P., Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira, L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, 
		F., Giller, K., Ampadu-Boakye, T., & Heerwaarden, J. van. (2020). N2Africa farm 
		monitoring - Mozambique, 2012 - 2013 [Data set]. International Institute of 
		Tropical Agriculture (IITA). doi:10.25502/HWDB-P578",

		carob_contributor = "Effie Ochieng",
		experiment_type = "variety_trials",
		has_weather =	TRUE,
		has_management = FALSE
	)
	
	#Registering the dataset
	ff <- carobiner::get_data(uri,path,group)
	js <- carobiner::get_metadata(dataset_id, path, group, major = 1, minor = 0)
	dset$license <- carobiner::get_license(js) 
	
	# read the data
	bn <- basename(ff)
	f <- ff[bn == "a_general.csv"]
	d <- read.csv(f)
	
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
	d <- d[,c("farm_id","season","country","action_site","sector_ward","vilage","gps_latitude","gps_longitude")]
	
	d$trial_id <- d$farm_id
	d$season <- d$season
	d$country <- d$country
	v <- carobiner::fix_name(d$action_site, "Title")
	v <- carobiner::replace_values(v, c("Maica","Tsangano-Fonte boa"),
	                                 c("Matica", "Tsangano"))
	d$adm2 <- v
	w <- carobiner::replace_values(d$sector_ward, 
			c( "Agonia", "Calipo/Muhua","Calipo/Mucua", "Calipo/Marasse", "Calipo/Mirasse","Calopo/Marrasse","Calipo/ Marasse","Calipo/ Muhua","Calipo/ Mirasse","Calipo/Maleliha","Uerro","Marasse"),
	        c("Angonia","Muhua","Mucua","Mirrasse","Mirrasse","Mirrasse","Mirrasse","Muhua","Mirrasse","Calipo","Uorra","Mirrasse"))
	d$adm3 <- w
	x <- carobiner::fix_name(d$vilage, "Title")
	x <- carobiner::replace_values(d$vilage, 
		c("Siwama", "Gurue-UP2","Murimo","Lioma-Namiepe","Lioma-Nintulo","Madiea","Niza","Mpupha","Namurekele","Mulosa","Namphi","Nahaco","Pwasiua","Mugunuwa","Jordan", "Vaiya","Nvine","Ruace-pissi","Nakuilo","Jordao","Mwetxo", "Mpupwa","Mohiyera","Viola 2"),
		c("Manica","Gurue","Murrimo","Namiepe","Nintulo","Madea","Zembe","Gurue","Gurue","Gurue","Gurue","Gurue","Gurue","Gurue","Gurue","Gurue","Gurue","Pissi","Gurue","Gurue","Gurue","Gurue","Gurue","Viola"))
	
	d$site <- x
	
	# selecting the areas with no coordinates get the trial_ids
#	lat_lon1 <- d[is.na(d$gps_latitude) | is.na(d$gps_longitude), ]
	
	# the given latitudes were not negative
	d$gps_latitude <- -(d$gps_latitude)
	
	
 # creating a data frame with the unique entries and the latitudes and longitudes, coordinates hard coded
	lat_lon <- data.frame(
		site = c("Barue", "Domue", "Ewarelo", "Gurue", "Lioma", "Macanga", "Magige", "Manica", "Mossurize", "Murrimo", "Mussacumbira", "Mutequelesse", "Nametil", "Nintulo", "Pissi", "Ruace", "Serra", "Tetete", "Tsangano", "Ulongue", "Zembe"), 
	
		lat = c(-17.5, -14.47, -15.16, -15.46, -15.17, -14.68, -15.32, -19, -21.21, -15.37, -13.43, -19, -14.21, -15.09, -14.9, -14.91, -16.47, -15.38, -15.16, -14.72, -19.29), 

		lon = c(33.65, 34.2, 36.94, 36.98, 36.8, 32.73, 36.7, 33.5, 33.38, 36.8, 38.6, 33.5, 40.55, 37.1, 36.6, 36.35, 35.7, 36.5, 34.5, 34.36, 33.35))	
	
  
	d <- merge(d, lat_lon, by=c("site"), all.x=TRUE)
 
	d$latitude <- d$gps_latitude
	d$longitude <- d$gps_longitude
	
	nna <- !is.na(d$lat)
	d$longitude[nna] <- d$lon[nna]
	d$latitude[nna] <- d$lat[nna]

	d$lat <- d$lon <- NULL
    
    d <- d[, c("trial_id","country","adm2","site","latitude","longitude")]
  
  
  #subset the variables of interest in d1 and d2
  
  d1 <- d1[, c("farm_id","plot_no","plot_size","crop","variety","inoculant_used","min_fertilizer_type","min_fertiliser_amount_kg")]
  d2 <- d2[, c("farm_id","plot_no","area_harvested_m2","weight_kg")]
  
  dd <- merge(d1,d2, by = c("farm_id","plot_no"), all.x = T )
  
  # working on fertilizer types
  dd$min_fertilizer_type[dd$min_fertilizer_type %in% c("SSP/Urea", "SSP+Ureia", "SSP+Urea",  "Urea+SSP")] <- "SSP; urea"	
  dd$min_fertilizer_type[dd$min_fertilizer_type %in% c("ssp", "Phosphor(SSP)", "SSP+Inoc","Y","12")] <- "SSP"
  dd$min_fertilizer_type[dd$min_fertilizer_type %in% c("Ureia", "Urea")] <- "urea"
  dd$min_fertilizer_type[dd$min_fertilizer_type %in% c( "None", "", "N")] <- "none"
  
  #working on fertilizer amounts
  dd$min_fertiliser_amount_kg[dd$min_fertiliser_amount_kg %in% c("2/0.4","2 /0.4","2/0,4","2/ 0,4")] <- "2/0.4"
  v <- carobiner::replace_values(dd$min_fertiliser_amount_kg,c("2kg/ha","","N"),                     c("2","0","0"))
  dd$min_fertiliser_amount_kg <- v
  
  #split fertilizer amount column to separate urea amounts and SSP amounts
  dd$ssp_amt <- ifelse(dd$min_fertilizer_type != "urea", as.numeric(sub("/.*", "", dd$min_fertiliser_amount_kg)), 0)
  dd$urea_amt <- ifelse(dd$min_fertilizer_type == "urea", as.numeric(sub(".*?/", "", dd$min_fertiliser_amount_kg)), 0)
  dd$fertilizer_type <- dd$min_fertilizer_type
  #to get rates of N and P
  dd$P_rate_plot <-dd$ssp_amt*0.16
  v <- carobiner::replace_values(dd$area_harvested_m2, c(101,102,103,104),              c(100,100,100,100))
  v[is.na(v)] <- 100
  dd$area_harvested_m2 <-v
  dd$P_fertilizer <- (10000/dd$area_harvested_m2) * dd$P_rate_plot
  dd$N_rate_plot <- dd$urea_amt*0.467
  dd$N_fertilizer <-(10000/dd$area_harvested_m2) * dd$N_rate_plot
  dd$K_fertilizer <- 0
  #getting the yield
  dd$yield <- (10000/dd$area_harvested_m2) * dd$weight_kg
  
  #correcting mismatched rows in inoculated
  v <- 1379:1386
  dd$inoculant_used[v] <- ifelse(dd$inoculant_used[v] == "", dd$variety[v], dd$inoculant_used[v])
  dd$inoculated <- ifelse(dd$inoculant_used == "Y",TRUE,FALSE)
  
  #standardizing the crops and variety
  v <- 570:573
  dd$crop[v] <- ifelse(dd$crop[v]=="100",dd$variety[v],dd$crop[v])
  v <- carobiner::replace_values(dd$crop, c("Groundnuit","Groudnuit","","Soybean","Soybean (Farmer variety)","Soyben","Soybean PD1","Soybean PD2"),   c("groundnut","groundnut","groundnut","soybean","soybean","soybean","soybean","soybean"))
  
  dd$crop <- v
  w <- carobiner::fix_name(dd$variety, "title")
  dd$variety <- w
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
	d5$previous_crop <- d5$main_crop_last_season
	p <- carobiner::fix_name(d5$previous_crop, "lower")
	p[grep("^grou", p)] <- "groundnut"
	
	p <- gsub("soyben", "soybean", p)
	p <- gsub("tobaco", "tobacco", p)
	p <- gsub("beans", "common bean", p)
	p <- gsub("irish potatoes", "potato", p)
	p <- gsub("sweet potatoes", "sweetpotato", p)
	p <- gsub(", ", "; ", p)
	p <- gsub(" ma$", " maize", p)

	d5$previous_crop <- p
	d5 <- d5[, c("trial_id","previous_crop")]
	
	#get the dates information
	d4$trial_id <- d4$farm_id

	#h <-  subset(d4, grepl("harvest", activity))
	
	p <- d4[grepl("planting", d4$activity), ]
	h <- d4[grepl("harvest", d4$activity), ]
	p$planting_date <- with(p, paste(date_planting_yyyy, date_planting_mm, date_planting_dd, sep = "-"))
	h$harvest_date <- with(h, paste(date_planting_yyyy, date_planting_mm, date_planting_dd, sep = "-"))
	p <- p[, c("trial_id","planting_date")]
	h <- h[, c("trial_id","harvest_date")]
    d4 <- merge(p,h, by = "trial_id")
  
  ## RH: do not remove data for that reason. But do try to fix.. 
  #remove rows with no planting and harvest info
  ##d4 <- subset(d4, !(planting_date == "0-0-0" & harvest_date == "0-0-0"))
	d4$planting_date[d4$planting_date == "0-0-0"] <- NA
	d4$planting_date[d4$harvest_date == "0-0-0"] <- NA
	d4$planting_date <- as.character(as.Date(d4$planting_date, format = "%Y-%m-%d"))
	d4$harvest_date <- as.character(as.Date(d4$harvest_date, format = "%Y-%m-%d"))
  
  
	#merge the data sets
	q1 <- merge(d, dd, by = "trial_id")
	q2 <- merge(q1, d3, by = "trial_id")
	q3 <- merge(q2, d4, by = "trial_id")
	q4 <- merge(q3, d5, by = "trial_id")
  
	q4$dataset_id <- dataset_id
	
	q4 <- q4[, c("trial_id","country","adm2","site", "latitude","longitude","rep","crop","variety","inoculated","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","planting_date","harvest_date", "row_spacing","plant_spacing","yield","previous_crop","dataset_id")]
	
	
	# all scripts should end like this
	carobiner::write_files(dset, q4, path=path)

}
