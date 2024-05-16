# R script for "carob"

## only processing the soil samples. The omission trials are extracted from different datasets 

carob_script <- function(path) {
  
"Landscape-scale variability of soil health indicators: Effects of cultivation on soil organic carbon in the Usambara Mountains of Tanzania"
	uri <- "doi:10.7910/DVN/1YZAQZ"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)
  
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=3, minor=7),
		data_institutions = "CIAT",
		#publication= "doi: 10.1007/s10705-015-9750-1",
		publication = NA,
		project="CCAFS",
		data_type= "survey",
		carob_contributor= "Andrew Sila",
		carob_date="2024-05-16"
	)
  
	f1 <- ff[basename(ff) == "2. Crop Nutrition Results-Lushoto_30 Reference Samples-June 2013.xlsx"]
	r1 <- carobiner::read.excel(f1)
	f2 <- ff[basename(ff) == "4. Lushoto organic carbon and nitrogen data_30 Reference Samples.xlsx"]
	r2 <- carobiner::read.excel(f2)
	f3 <- ff[basename(ff) == "6. LushotoLDSFRaw.csv"]
	r3 <- read.csv(f3)
	
	r1$SSN <- gsub('icraf ', 'icr', r1$Field)
	r <- merge(r1, r2, by = "SSN")
	ldsf <- r3[,c('Site','Cluster', 'Plot', 'Latitude', 'Longitude', 'Country')]
	ldsf$trial_id <- paste0(ldsf$Site, '.', ldsf$Cluster, '.', ldsf$Plot)
	r$trial_id <- paste0(r$Site, '.', r$Cluster, '.', r$Plot)
	r <- merge(r, ldsf[,c('Latitude', 'Longitude', 'Country',"trial_id")], by = "trial_id")
	r <- data.frame(r)
	
	d <- data.frame(
		trial_id = r$trial_id,
		soil_pH = as.numeric(r$pH),
		soil_EC = as.numeric(r$EC.S...uS.cm.)/1000,
		soil_Al = as.numeric(r$Al..ppm.),
		soil_B = as.numeric(r$B..ppm.),
		soil_Ca = as.numeric(r$Ca..ppm.),
		soil_Cu = as.numeric(r$Cu..ppm.),
		soil_Fe = as.numeric(r$Fe..ppm.),
		soil_K = as.numeric(r$K..ppm.),
		soil_Mg = as.numeric(r$Mg..ppm.),
		soil_Mn = as.numeric(r$Mn..ppm.),
		soil_Na = as.numeric(r$Na..ppm.),
		soil_P_total = as.numeric(r$P..ppm.),
		soil_S = as.numeric(r$S..ppm.),
		soil_Zn = as.numeric(r$Zn..ppm.),
		soil_Ex_acidity = as.numeric(r$Hp..meq.100g.),
		soil_PSI = as.numeric(r$PSI..meq.100g.),
		soil_C = r$Carbon.Content....,
		soil_N = r$Nitrogen.Content....,
		country = r$Country,
		location = r$Site,
		longitude = r$Longitude,
		latitude = r$Latitude)
		
	# d$soil_PSI <- ifelse(d$soil_PSI < 0, 0,d$soil_PSI)
	#  
	# d$latitude[d$latitude == 0] <- NA
	# d$longitude[d$longitude == 0] <- NA
	# d$longitude[d$location == "Finkolo"] <- -5.5113
	# d$latitude[d$location == "Finkolo"] <- 11.2692
	# 

	##miss <- unique(d[is.na(d$latitude), c("country", "location")])
	##geo <- carobiner::geocode(miss$country, miss$location)
	##geo$put

  carobiner::write_files(path, dset, d)
}
