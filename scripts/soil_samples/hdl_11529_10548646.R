
carob_script <- function(path) {
  
" Description:
	
The Soil Intelligence System (SIS-India) mid-IR (MIR) dataset contains MIR spectra and wet chemistry measurements (pH, EC, OC, N, P, K, S, B, Zn, Cu, Mn, Fe) for 3182 soil samples collected from randomly selected rice-wheat farmer fields spread uniformly within a KVK (government extension system) domain/district in Bihar, India. Soil sampling was conducted during the time following kharif harvest and prior to land preparation for rabi during the 2018, 2019, and 2020 seasons. The dataset is used for the training and evaluation of machine learning models for spectral prediction of soil properties. (2022-03-03)
"
  
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
		data_type = "soil sampling"
    )
  
  ## download and read data 
  
	ff	<- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	dset$license <- "unknown" #carobiner::get_license(js)
	
	# read wetchem data and sample identifier tables
	f0 <- ff[basename(ff) == "sample_identifier.csv"]
	d0 <- read.csv(f)
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
	
	d$dataset_id <- dataset_id
	d$trial_id <- "1"
	
	# Drop id, original_id, year and district columns from d
	k <- which(colnames(d) %in% c("id", "original_id", "year", "district"))
	d <- d[,-k]
	
	carobiner::write_files(dset, d, path=path)
}


