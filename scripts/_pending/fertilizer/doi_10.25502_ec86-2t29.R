# R script for "carob"

## ISSUES

# multiple treatments given in separate columns
# multiple grain yields 

## RH: Indeed. There are multiple plots / treatment per farm that all need to be processed
## Also need to clarify the intercropping with banana situation. 


carob_script <- function(path) {
  
"N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project."

  uri <- "doi:10.25502/ec86-2t29"
  group <- "fertilizer"
  
  ff <- carobiner::get_data(uri, path, group)
  
  dset <- data.frame(
  	carobiner::read_metadata(uri, path, group, major=3, minor=1),
    data_institute = "IITA",
    publication= NA,
    project="N2Africa",
    data_type= "experiment",
    carob_contributor= "Mitchelle Njukuya",
    carob_date="2024-03-19"
  )
  
  ##### PROCESS data records
  
  f <- ff[basename(ff) == "data_table.csv"]
  r <- read.csv(f)
  
  d <- data.frame(record_id=r$id, country=r$country, 
		location=r$lga_district_woreda, 
        site=r$sector_ward, 
		longitude=r$gps_field_device_longitude.decimal_degrees, 
        latitude=r$gps_field_device_latitude.decimal_degrees, 
		elevation=r$gps_field_device_altitude.m,
        variety=r$improved_variety_name, variety_type=r$legume_planted_in_the_n2africa_trial,
        fertilizer_type=r$type_mineral_fertilizer_experiment, 
		OM_type=r$type_organic_fertilizer_experiment,
        pest_species=r$type_of_pest, 
		weed_species=r$type_of_weeds, 
		weed_severity=r$severity_weeds, 
		disease=r$type_of_disease,
        disease_severity=r$severity_disease, 
		trial_id=r$SN)

	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
  
	d$country <- carobiner::fix_name(d$country, "title")
	d$OM_type <- carobiner::replace_values(d$OM_type, "farmyard_manure", "farmyard manure")
	## for all?
	d$OM_used <- TRUE

	d$previous_crop <- r$crop_1_previous_season  
#	d$season_constraint <- "drought"
	d$weeding_times <- as.integer(d$weeding_times)
  #adding dates
	d$planting_date <- r$date_of_planting_whole_n2africa_field.date
	d$harvest_date <- r$date_of_final_harvest_whole_n2a_field.date
	d$fertilizer_date <- r$date_of_mineral_fertiliser_application_whole_n2africa_field.date
	d$weeding_date <- r$date_of_1st_weeding_whole_n2africa_field_.date
  
	d$yield <- mean(r$grain_weight_crop_1_plot_1.kg, r$grain_weight_crop_1_plot_2.kg, r$grain_weight_crop_1_plot_3.kg, r$grain_weight_crop_1_plot_4.kg, r$grain_weight_crop_1_plot_5.kg, r$grain_weight_crop_1_plot_6.kg) 
#	d$yield_part <- "seed"
 
    carobiner::write_files(dset, d, path=path)
}
