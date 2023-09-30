
carob_script <- function(path) {
  
" Description:
Dataset for: Agronomic log of small potato farmers during the 2021-2022 campaign in the districts of Chugay, Pataz, Pías y Tayabamba in the department of La Libertad - Peru."
  
	uri <- "hdl:11529/10548646"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "soil_samples"
  
	## data set level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project="CSISA",
		uri=uri,
		publication=NA,
		data_citation = "Sherpa Sonam; Poonia Shishpal; Kumar Sunil; Sharma Sachin; Ajay Anurag; Wu William; Singh Balwinder; McDonald Andrew, 2022, 'Soil wet chemistry data and mid-infrared spectra collected from rice-wheat farmers’ fields in Bihar, India', https://hdl.handle.net/11529/10548646, CIMMYT Research Data & Software Repository Network, V1",
		data_institutions = "CIMMYT",
		carob_contributor="Andrew Sila",
		data_type = "on-farm experiment"
    )
  
  ## download and read data 
  
	ff	<- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	dset$license <- carobiner::get_license(js)
	
	# read wetchem data and sample identifier tables
	f0 <- ff[basename(ff) == "sample_identifier.csv"]
	d0 <- data.frame(read.csv(f0, sep = ","))
	# rename columns
	colnames(d0) <- tolower(colnames(d0))

	f1 <- ff[basename(ff) == "wetchem.csv"]
	d1 <- data.frame(read.csv(f1, sep = ","))
	
	# Properties
	#hd <-  c('pH_water', 'EC', 'carbon_organic', "nitrogen_total", 'phosphorus_extractable', 'potassium_extractable', 'zinc_extractable', 'copper_extractable',  'iron_extractable','manganese_extractable', "sulphur_extractable", 'boron_extractable')
	
	hd <- c('soil_pH', 'soil_EC', 'soil_SOC', 'soil_N', 'soil_P_total', 'soil_K',  'soil_Zn', 'soil_Cu', 'soil_Fe', 'soil_Mn', 'soil_S', 'soil_B')
	colnames(d1) <- c('id', hd)
	
	# No need to read table with MIR data now because we are not processing it
	f2 <- ff[basename(ff) == "drychem.csv"]
	#d2 <- data.frame(read.csv2(f2, sep = ","))
	
	# merge d0 and d1 tables
	d <- merge(d0,d1)
	
	d$country <- 'India'
	
	# Remove any -ve B values
	
	bn <- which(d$soil_B < 0)
	
	d <-  d[-bn,]
	
	# Add other common soil variables normally tested
	d$soil_total_carbon <- NULL
	d$soil_clay <- NULL
	d$soil_sand <- NULL
	d$soil_silt <- NULL
	d$soil_pH_KCl <- NULL
	d$soil_pH_CaCl2 <- NULL
	d$soil_P_available <- NULL
	d$dataset_id <- dataset_id
	d$trial_id <- NA
	d$crop <- NA
	d$yield_part <- NA
	d$yield <- NA
	
	# Drop id, original_id, year and district columns from d
	k <- which(colnames(d) %in% c("id", "original_id", "year", "district"))
	carobiner::write_files(dset, d[,-k], path=path)
}



