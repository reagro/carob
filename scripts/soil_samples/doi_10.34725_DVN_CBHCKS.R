# R script for "carob"


carob_script <- function(path) {
  "These data were collected within the Kenya Cereal Enhancement Programme-Climate Resilient Agricultural Livelihoods (KCEP-CRAL) Window. This study was conducted within the action sites of the Kenya Cereal Enhancement Programme-Climate Resilient Agricultural Livelihoods (KCEP-CRAL) Window, which is a programme supported by the Government of Kenya and International Fund for Agricultural Development (IFAD) within the Adaptation for Smallholder Agriculture Programme (ASAP). The project assessed key indicators of land and soil health in order to understand drivers of degradation, and monitor changes over time using the Land Degradation Surveillance Framework (LDSF) methodology (http://landscapeportal.org/blog/2015/03/25/the-land-degradation-surveillance-framework-ldsf/). The LDSF provides a field protocol for measuring indicators of the 'health' of an ecosystem. The LDSF was developed by the World Agroforestry (ICRAF) in response to the need for consistent field methods and indicator frameworks to assess land health in landscapes. The framework has been applied in projects across the global tropics, and is currently one of the largest land health databases globally with more than 30,000 observations, shared at http://landscapeportal.org. This project will benefit from existing data in the LDSF database, while at the same time contributing to these critically important global datasets through on-going data collection. Earth Observation (EO) data will be combined with the LDSF framework to develop the outputs for the project, including land degradation and soil health. Specific Activities on the ICRAF Component as Stated in the Agreement 1. Develop survey methodology detailing study design, methodology, tools, work plan and timelines documented 2. Procure LDSF field survey equipment 3. Conduct five LDSF surveys across the KCEP-CRAL action areas 4. Process, analyse and document the soil samples 5. Conduct Earth Observation-based assessment of biophysical indicators over time 6. Conduct capacity development opportunities with members of the PCU M&E staff and Government counterparts on LDSF field methodology This dataset contains LDSF field data from five LDSF sites in Kenya totalling 787 LDSF plots . It also contains the predictions using MIR Spectroscopy for 1500 Topsoil (0-20cm) and Subsoil (20-50cm) samples for six soil variables, including soil organic carbon, total nitrogen and pH."
	
  uri <- "doi:10.34725/DVN/CBHCKS"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)
  
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=4, minor=0),
		data_institute = "CIFOR-ICRAF",
		publication = NA,
		project="Kenya Cereal Enhancement Programme-Climate Resilient Agricultural Livelihoods (KCEP-CRAL) Window",
		data_type= "survey",
		treatment_vars = "none",
		carob_contributor= "Andrew Sila",
		carob_date="2024-05-29"
	)
  
	f1 <- ff[basename(ff) == "stdfine_ICRAF_mir_soil_predictions_KCEP_25Jan2021.csv"]
	r1 <- read.csv(f1)
	f2 <- ff[basename(ff) == "KCEP CRAL_ldsf_plotpropertiesICRAF 2021.csv"]
	r2 <- read.csv(f2)

	# Merge r1 and r2 to get lat and lon columns to be appended into the soils data
	d <- merge(r1,r2[, c('Cluster', "Plot", "Latitude", "Longitude", "Site")], by = c("Site", "Cluster", "Plot"), all = TRUE)
	#d$Depthcode[d$Depthcode=="Topsoil"] <- "0"
	#d$Depthcode[d$Depthcode=="Subsoil"] <- "20"
	d$soil_sample_top <- d$DepthTop
	#d$Depthcode[d$Depthcode=="20"] <- "50"
	#d$Depthcode[d$Depthcode=="0"] <- "20"	
	d$soil_sample_bottom <- d$DepthBottom
	#d$Cu.ppm.[d$Cu.ppm.=="< 0.2"] <- 0.001
	#d$B.ppm.[d$B.ppm.=="< 0.02"] <- 0.001
	
	unique(r$Site[is.na(r$Longitude) == TRUE])
	#carobiner::geocode("Chasimba", "Kenya")
	r$Latitude[is.na(r$Longitude) == TRUE][r$Site[is.na(r$Longitude) == TRUE] == "Chasimba"] <- -3.7268
	r$Longitude[is.na(r$Longitude) == TRUE][r$Site[is.na(r$Longitude) == TRUE] == "Chasimba"] <- 39.735
	
	#carobiner::geocode("Gatunga", "Kenya")
	r$Latitude[is.na(r$Longitude) == TRUE][r$Site[is.na(r$Longitude) == TRUE] == "Gatunga"] <- 0.1
	r$Longitude[is.na(r$Longitude) == TRUE][r$Site[is.na(r$Longitude) == TRUE] == "Gatunga"] <- 38.0166
	
	#carobiner::geocode("Kubo", "Kenya")
	r$Latitude[is.na(r$Longitude) == TRUE][r$Site[is.na(r$Longitude) == TRUE] == "KuboSouth"] <- -4.2806
	r$Longitude[is.na(r$Longitude) == TRUE][r$Site[is.na(r$Longitude) == TRUE] == "KuboSouth"] <- 39.3976
	
	#carobiner::geocode("Mbeere", "Kenya")
	r$Latitude[is.na(r$Longitude) == TRUE][r$Site[is.na(r$Longitude) == TRUE] == "Muminji"] <- -0.5065
	r$Longitude[is.na(r$Longitude) == TRUE][r$Site[is.na(r$Longitude) == TRUE] == "Muminji"] <- 37.791
	
	#carobiner::geocode("Thange", "Kenya")
	r$Latitude[is.na(r$Longitude) == TRUE][r$Site[is.na(r$Longitude) == TRUE] == "Thange"] <- -2.4268
	r$Longitude[is.na(r$Longitude) == TRUE][r$Site[is.na(r$Longitude) == TRUE] == "Thange"] <- 38.1205
	
	d <- data.frame(
		trial_id = r$SSN,
		soil_pH = as.numeric(r$predpH),
		soil_SOC = as.numeric(r$predSOC),
		soil_ex_bases = as.numeric(r$predExBas),
		soil_clay = as.numeric(r$predClay),
		soil_sand = as.numeric(r$predSand),
		soil_sample_top = as.numeric(r$soil_sample_top),
		soil_sample_bottom = as.numeric(r$soil_sample_bottom),
		longitude = r$Longitude,
		latitude = r$Latitude,
		country =  'Kenya'
	)
	
	carobiner::write_files(path, dset, d)
}
