#################################################################################
#N2Africa was aimed at increasing biological nitrogen fixation and productivity
#of grain legumes through effective production technologies including inoculants
#and fertilizers adapted to local settings which was aimed at increasing soil
#fertility.The trails were conducted in 11 african countries
#################################################################################

carob_script <- function(path){
 
	uri <- "doi.org/10.25502/hwdb-p578"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	
	#dataset level data
	
	dset <- data.frame(
		dataset_id = dataset_id,
		group = group,
		uri = uri,
		publication = NA,
		data_citation ="Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, P., Baijukya, F., Sanginga, J.-M., Woomer, P., 
		Chikowo, R., Phiphira, L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, 
		F., Giller, K., Ampadu-Boakye, T., & Heerwaarden, J. van. (2020). N2Africa farm 
		monitoring - Mozambique, 2012 - 2013 [Data set]. International Institute of 
		Tropical Agriculture (IITA). https://doi.org/10.25502/HWDB-P578",

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
	f2 <- ff[bn == "d_cropping_calendar.csv"]
	d2 <- read.csv(f2)
	f3 <- ff[bn == "c_use_of_package_3.csv"]
	d3 <- read.csv(f3) 
	f4 <- ff[bn == "e_harvest.csv"]
	d4 <- read.csv(f4)
	f5 <- ff[bn == "b_info_site_2.csv"]
	d5 <- read.csv(f5)
	
	#start processing the 1st data
	d$trial_id <- d$farm_id
	d$adm1 <- d$action_site
	d$adm2 <- carobiner::fix_name(d$sector_ward)
	d$adm2 <- gsub(" ", "", d$adm2)
	d$adm2 <- gsub("Calipo/Mirasse", "Calipo/Marasse", d$adm2)
	d$adm2 <- gsub("Calopo/Marrasse", "Calipo/Marasse", d$adm2)
	d$adm3 <- d$vilage
	
	d <- d[,c("trial_id","adm1","adm2","adm3")]
	
	
	#process the 2nd data set
	d1$trial_id <- d1$farm_id
	#cleaning d1$min_fertilizer_type 
	d1$min_fertilizer_type[d1$min_fertilizer_type %in% c("SSP/Urea", "SSP+Ureia", "Urea+SSP")] <- "SSP; urea"	
	d1$min_fertilizer_type[d1$min_fertilizer_type == "Ureia"] <- "urea"	
	d1$min_fertilizer_type[d1$min_fertilizer_type %in% c("ssp", "Phosphor(SSP)")] <- "SSP"
	#adding the fertilizer inputs 
	#d1$min_fertilizer_type ==	"Y" to be determined what yes mean? which fertilizer is this?

	#found by calculating elemental P in SSP applied to the plot size and converted to kg/ha
	d1$P_fertilizer[d1$min_fertilizer_type %in% c("SSP", "SSP; urea", "SSP+Inoc")] <- 30 
	#found by calculating elemental N in Urea applied in the plot size and converted to kg/ha
	d1$N_fertilizer[d1$min_fertilizer_type %in% c("SSP; urea", "urea")]<- 3 
	
	d1 <- d1[, c("trial_id","crop","variety","P_fertilizer","N_fertilizer")]
	
	#process the 3rd data set
	d2$trial_id <- d2$farm_id
	d2$start_date <- paste(d2$date_planting_yyyy, d2$date_planting_mm, d2$date_planting_dd, sep = "-")
	d2$start_date[d2$date_planting_yyyy == 0] <- NA
	d2$start_date <- as.character(as.Date(d2$start_date))
	
	d2 <- d2[, c("start_date","trial_id")]
	
	#process the 4th dataset
	d3$trial_id <- d3$farm_id
	d3$row_spacing <- as.numeric(d3$crop_1_spacing_row_to_row)
	d3$plant_spacing <- as.numeric(d3$crop_1_spacing_plant_to_plant)

	d3 <- d3[, c("trial_id","row_spacing","plant_spacing")]
	
	#process the 5th dataset
	d4$trial_id <- d4$farm_id
	#d$grain_weight <- d4$weight_grain * is this in g/1000 seeds? * convert to numeric first
	# get the yield lets change (d4$weight_kg) to numeric
	d4$weight_kg <- as.numeric(d4$weight_kg)
	# convert the yield per ha, multiply by 10000
	d4$yield <-(10000/d4$area_harvested_m2)*d4$weight_kg 
	
	d4 <- d4[, c("trial_id","yield")]
	
	#process the 6th dataset
	d5$trial_id <- d5$farm_id
	d5$previous_crop <- d5$main_crop_last_season
	
	d5 <- d5[, c("previous_crop","trial_id")]
	
	#merge the datasets
	q <- carobiner::bindr(d,d1,d2,d3,d4,d5)
	
	q$country <- "Mozambique"
	q$latitude <- -18.66569
	q$longitude <- 35.52956
	
	#cleaning up the crop variable
	message("	 do not fill in missing data with random values. EO please fix\n")
	
## RH: you cannot randomly assign crops!!!
##	q$crop <- ifelse(q$crop %in% c("Groudnuit", "Groundnuit"), "groundnut",
##									 ifelse(q$crop %in% c("Soybean","Soybean PD1","Soybean PD2","Soybean (Farmer variety)","Soyben","100"),"soybean","groundnut")) 
##	# NA and 100 were randomly filled with groundnut and soybean respectively 

	q$crop <- carobiner::fix_name(q$crop, "lower")
	q$crop[grep("soy", q$crop, TRUE)] <- "soybean"
	q$crop[grep("grou", q$crop, TRUE)] <- "groundnut"
	
	p <- carobiner::fix_name(q$previous_crop, "lower")
	p[grep("^grou", p)] <- "groundnut"
	
	p <- gsub("soyben", "soybean", p)
	p <- gsub("tobaco", "tobacco", p)
	p <- gsub("beans", "common bean", p)
	p <- gsub("irish potatoes", "potato", p)
	p <- gsub("sweet potatoes", "sweetpotato", p)
	p <- gsub(", ", "; ", p)
	p <- gsub(" ma$", " maize", p)

	q$previous_crop <- p
	
	q <- q[, c("trial_id","country","adm1","adm2","adm3","crop","variety","P_fertilizer","N_fertilizer","start_date", "row_spacing","plant_spacing","yield","previous_crop", "latitude","longitude")]
	
	## more normalization needed of these names 
	q$variety <- carobiner::fix_name(q$variety)
	
	q$dataset_id <- dataset_id
	
	# all scripts should end like this
	carobiner::write_files(dset, q, path, dataset_id, group)

}
