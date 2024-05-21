
carob_script <- function(path) {
  
"Physical topsoil properties in Murugusi, Western Kenya"
  
	uri <- "doi:10.7910/DVN/GVNJAB"
	group <- "soil_samples"

	ff <- carobiner::get_data(uri, path, group)
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=4),
		project= NA,
		publication= NA,
		data_institute = "CIAT",
		data_type = "soil properties", 
		treatment_vars = "none",
		carob_contributor="Andrew Sila", 
		carob_date="2023-09-28"
	)
  
  # No need to read the table with MIR data
	f <- ff[basename(ff) == "02 soil_samples-data.xlsx"]
	r <- data.frame(carobiner::read.excel(f))

	d <- carobiner::change_names(r[, -c(2:3)], c("Lab_id", "bd_020","TC_020", "clay_020", "sand_020"), 
			c('trial_id', 'soil_bd','soil_total_carbon', 'soil_clay', 'soil_sand'))
					
	v <- terra::vect(as.matrix(r[, c("POINT_X_utm36N", "POINT_Y_utm36N")]), 
							crs="+proj=utm +zone=36")
	v <- terra::project(v, "+proj=longlat") |> terra::crds()
	# Extract latitude and longitude values
	d$latitude <- v[,2]
	d$longitude <- v[,1]
	
	d$country <- "Kenya"
	
	d$soil_sample_top <- 0
	d$soil_sample_bottom <- 20
	
	
	carobiner::write_files(dset, d, path=path)
}



