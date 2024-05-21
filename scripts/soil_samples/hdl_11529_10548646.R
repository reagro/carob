
carob_script <- function(path) {
  
"The Soil Intelligence System (SIS-India) mid-IR (MIR) dataset contains MIR spectra and wet chemistry measurements (pH, EC, OC, N, P, K, S, B, Zn, Cu, Mn, Fe) for 3182 soil samples collected from randomly selected rice-wheat farmer fields spread uniformly within a KVK (government extension system) domain/district in Bihar, India. Soil sampling was conducted during the time following kharif harvest and prior to land preparation for rabi during the 2018, 2019, and 2020 seasons. The dataset is used for the training and evaluation of machine learning models for spectral prediction of soil properties. (2022-03-03)"
  
	uri <- "hdl:11529/10548646"
	group <- "soil_samples"
	ff	 <- carobiner::get_data(uri, path, group)
  
	## data set level data 
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		project="CSISA",
		publication=NA,
		data_institute = "CIMMYT",
		carob_contributor="Andrew Sila",
		carob_date="2023-09-28",
		data_type = "soil sampling",
		treatment_vars = "none"
    )
  
  
	
	# read wetchem data and sample identifier tables
	f0 <- ff[basename(ff) == "sample_identifier.csv"]
	d0 <- read.csv(f0)
	# rename columns
	colnames(d0) <- tolower(colnames(d0))

	f1 <- ff[basename(ff) == "wetchem.csv"]
	d1 <- read.csv(f1)
	
	# Properties
	#hd <-  c('pH_water', 'EC', 'carbon_organic', "nitrogen_total", 'phosphorus_extractable', 'potassium_extractable', 'zinc_extractable', 'copper_extractable',  'iron_extractable','manganese_extractable', "sulphur_extractable", 'boron_extractable')
		
	hd <- c('soil_pH', 'soil_EC', 'soil_SOC', 'soil_N', 'soil_P_total', 'soil_K',  'soil_Zn', 'soil_Cu', 'soil_Fe', 'soil_Mn', 'soil_S', 'soil_B')
	colnames(d1) <- c('id', hd)
	
	# No need to read table with MIR data now because we are not processing it
	## why not?
	
	f2 <- ff[basename(ff) == "drychem.csv"]
	#d2 <- data.frame(read.csv2(f2, sep = ","))
	
	# merge d0 and d1 tables
	d <- merge(d0, d1, by="id")
	d$country <- 'India'
	
	# Remove negative B values
	d$soil_B[d$soil_B < 0] <- NA
	
	
	d$trial_id <- "1"
	
	# Drop id, original_id, year and district columns from d
	k <- which(colnames(d) %in% c("id", "original_id", "year", "district"))
	d <- d[,-k]
	
	carobiner::write_files(dset, d, path=path)
}



