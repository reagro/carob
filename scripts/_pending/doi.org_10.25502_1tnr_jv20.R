
###########################################################################################################
# N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among 
# African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition
# and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable,
# long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain
# legumes through effective production technologies including inoculants and fertilizers adapted to local settings.
# A strong national expertise in grain legume production and N2-fixation research and development will be the legacy
# of the project.
# The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other 
# countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
###########################################################################################################

# Notes
# Note1: Where trial_id is labelled as "" it is replaced with the preceding value.


carob_script <- function(path){

	uri <- "doi.org/10.25502/1tnr-jv20"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"

#dataset level data

	dset <- data.frame(
		dataset_id = dataset_id,
		group = group,
		project="N2Africa",
		uri = uri,
		publication = 'doi.org/10.1016/j.agee.2017.08.015',
		data_citation ="Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, P., Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira, L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller, K., Ampadu-Boakye, T., & Heerwaarden, J. van. (2020). N2Africa dianostic trial - Nigeria, 2014 [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/EZQV-ZZ19",
		carob_contributor = "Andrew Sila",
		data_type = "on farm experiment",
		data_institutions="IITA"
	)

p_year <- 2011 #planting year
h_year <- 2011 # harvest year

## download and read data 
	ff <- carobiner::get_data(uri,path,group)
	js <- carobiner::get_metadata(dataset_id, path, group, major = 1, minor = 0)
	dset$license <- carobiner::get_license(js) 
	
	# read the treatment data table
	f0 <- ff[basename(ff) == "f_treatments.csv"]
	d0 <- data.frame(read.csv2(f0, sep = ","))
	
	f1 <- ff[basename(ff) == "a_general.csv"]
	d1 <- data.frame(read.csv2(f1, sep = ","))
	
	#  Open this chunk below to geocode trial locations
	
	# library(tidygeocoder)
	# d1 <-  d1 %>%
	#   mutate(gcode_add <- paste0(country, ',', district, ",", sector_ward))
	# colnames(d1) <- c(colnames(d1[,-21]), 'gcode_add')
	# d1_geocode <- d1 %>%
	#   geocode_combine(queries = list(list(method = 'census'), list(method = 'osm')),
          # global_params = list(address = "gcode_add"), cascade = TRUE)
	
	# These two sector gps coords copied from the above chunk
	
  Rukara <- c(-1.795684,30.50467)
  Nyamirama <- c(-1.931835,30.50349)
  latlon <- rbind(Rukara,Nyamirama)
  latlon <- cbind(row.names(latlon), latlon)
  colnames(latlon) <-  c("sector_ward", 'lat', 'lon')
  latlon <- as.data.frame(latlon)

  d1 <- merge(d1, latlon, by = "sector_ward")
  d1$gps_longitude <- d1$lon
  d1$gps_latitude <- d1$lat
	
	f2 <- ff[basename(ff) == "b_info_site_1.csv"]
	d2 <- data.frame(read.csv2(f2, sep = ","))
	
	f3 <- ff[basename(ff) == "b_info_site_2.csv"]
	d3 <- data.frame(read.csv2(f3, sep = ","))
	
	f4 <- ff[basename(ff) == "d_cropping_calendar.csv"]
	d4 <- data.frame(read.csv2(f4, sep = ","))
	
	f5 <- ff[basename(ff) == "e_harvest.csv"]
	d5 <- data.frame(read.csv2(f5, sep = ","))

	# Get averages yield for crop1 and crop2 then separate
	d5$crop_1_weight_stover <- as.numeric(d5$crop_1_weight_stover)
	d5$crop_1_weight_grain <- as.numeric(d5$crop_1_weight_grain)
	
	# ylds <- aggregate(x = d5[c("crop_1_area_harvested", 
	#                                     "crop_1_plants_no",
	#                                     "crop_1_weight_stover",
	#                            "crop_1_weight_grain")],
	#                        by = d5[c("farm_id")],
	#                        FUN = mean)
	# ylds
	# No need to average d5 since we need to link each yield by plot number
	
	f6 <- ff[basename(ff) == "variable_definitions.csv"]
	d6 <- data.frame(read.csv2(f6, sep = ","))
	
	f7 <- ff[basename(ff) == "c_use_of_package_1.csv"]
	d7 <- data.frame(read.csv(f7, sep = ","))
	
	f8 <- ff[basename(ff) == "c_use_of_package_2.csv"]
	d8 <- data.frame(read.csv(f8, sep = ","))
	
	f9 <- ff[basename(ff) == "c_use_of_package_3.csv"]
	d9 <- data.frame(read.csv(f8, sep = ","))
	
	f10 <- ff[basename(ff) == "c_use_of_package_4.csv"]
	d10 <- data.frame(read.csv(f8, sep = ","))
	
	f11 <- ff[basename(ff) == "c_use_of_package_5.csv"]
	d11 <- data.frame(read.csv(f8, sep = ","))
	
	f12 <- ff[basename(ff) == "a_livestock.csv"]
	d12 <- data.frame(read.csv(f12, sep = ","))
	
	# Data in tables d8 - d11 data is similar. So compile data from:
	# d <- c(d1, d5, and d8)
	
	#Also ensure to pick crop_1 data yield data only
	d <- merge(d1, d5[,c('farm_id', 'plot_no','crop_1_area_harvested','crop_1_plants_no','crop_1_weight_stover', 'crop_1_weight_grain')])
	
	d0$trial_id <- d0$experiment_id
	
	# fill in the missing trial_ids
	d0$trialid <- NA
	
	# Get experiment treatments from d3 table
	d3$type_synth_fert_last_season
	d3$type_synth_fert_before_last_season
	d3$type_organic_fert_last_season
	d3$type_inoculant_last_season
	d3$type_inoculant_before_last_season
	
	# reporting table1 : d
	
  # d <- merge tables d0,d3 and d5
	#Remove white spaces in the treatment field
	d$treatment <- carobiner::fix_name(d$treatment)
	
	# remove whitespace in trial_id
	#d$trial_id <- gsub("[[:space:]]", "", d$trial_id)
	d$trial_id <- ifelse (d$trial_id == "",NA, d$trial_id)
	d$crop = "soybean"
	d$yield_part <- "seed"
	d$dataset_id <- dataset_id
	# all scripts should end like this
	
	# yield is in kg/ha
	carobiner::write_files(dset, d, path=path)
}



