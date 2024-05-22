# R script for "carob"


carob_script <- function(path) {
  
"The ICRAF-ISRIC Soil VNIR Spectral Library contains visible near infrared spectra of 785 soil profiles (4,438 samples) soils selected from the Soil Information System of the International Soil Reference and Information Centre (ISRIC). The samples consist of all physically archived samples at ISRIC in 2004 for which soil attribute data was available. The spectra were measured at the World Agroforestry Center (ICRAF) Soil and Plant Spectral Diagnostic Laboratory. The samples are from 58 countries spanning Africa, Asia, Europe, North America, and South America. Associated attribute data, such as geographical coordinates, horizon (depth), and physical and chemical properties, are provided as separate tables. Using different methods for linking related files as explained in the pdf document accompanying this dataset (0ICRAF-ISRICSoilVNIRSpectralLibrary.pdf), the visNIR spectra can be linked with all tables holding both physical and chemical properties."
  
	uri <- "doi:10.34725/DVN/MFHA9C"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)
  
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institutions = "ICRAF-ISRIC",
		publication= " http://doi.org/10.1016/j.earscirev.2016.01.012.",
		project="FAO",
		data_type= "survey",
		exp_treatments = "none",
		carob_contributor= "Andrew Sila",
		carob_date="2024-05-20"
	)
  
	f1 <- ff[basename(ff) == "Chemical_properties.csv"]
	r1 <- read.csv(f1)
	f2 <- ff[basename(ff) == "Physical_properties.csv"]
	r2 <- read.csv(f2)
	f3 <- ff[basename(ff) == "Country.csv"]
	r3 <- read.csv(f3)
	f4 <- ff[basename(ff) == "Region.csv"]
	r4 <- read.csv(f4)
	f5 <- ff[basename(ff) == "Classification.csv"]
	r5 <- read.csv(f5)
	f6 <- ff[basename(ff) == "Site_description.csv"]
	r6 <- read.csv(f6)
	
	# Merge country and region
	r34 <- merge(r3,r4)
	
	#Merge country with r56
	r346 <- merge(r34, r6)
	
	#Merge r3 with r5
	r35 <- merge(r3, r5)
	
	# Merge r4 with r36
	r1$xxx <- paste0(r1$ISO, r1$BTOP, r1$BBOT, r1$SAMPLENO)
	r2$xxx <- paste0(r2$ISO, r2$BTOP, r2$BBOT, r2$SAMPLENO)
	
	r12 <- merge(r1, r2, by = "xxx")
	r125 <- merge(r12, r35, by.x = 'ID.x', by.y = "ID")
	
	r5$id <- paste0(r5$ISO,".", r5$ID)
	r6$id <- paste0(r6$ISO,".", r6$ID)
	
	r56 <- merge(r5, r6, by = "id")
	r12$id <- paste0(r12$ISO.x,".", r12$ID.x)
	
	
	r125 <- merge(r12, r56, by = 'id')
	
	# Get lat and lon from r125 and convert into degrees
	r125$LONM[which(is.na(as.numeric(r125$LONM)) == TRUE)] <- 0
	r125$LONS[which(is.na(as.numeric(r125$LONS)) == TRUE)] <- 0
	r125$Longitude <-   r125$LOND +  as.numeric(r125$LONM)/60 +  as.numeric(r125$LONS)/3600
	
	r125$LATM[which(is.na(as.numeric(r125$LATM)) == TRUE)] <- 0
	r125$LATS[which(is.na(as.numeric(r125$LATS)) == TRUE)] <- 0
	r125$Latitude <-   r125$LATD +  as.numeric(r125$LATM)/60 +  as.numeric(r125$LATS)/3600
	
	r125$Longitude[which(r125$LONEW == "W")] <- -r125$Longitude[which(r125$LONEW == "W")]
	r125$Latitude[which(r125$LATNS == "S")] <- -r125$Latitude[which(r125$LATNS == "S")]
	
	r125 <- merge(r125,r3, by.x = "ISO.x.x", by.y = "ISO")
	
	r <- r125
	
	# Clean country names
	r$COUNTRY[which(r$COUNTRY=="Cote d'Ivoire")] <- "Côte d'Ivoire"
	r$COUNTRY[which(r$COUNTRY=="Russian Federation")] <- "Russia"
	r$COUNTRY[which(r$COUNTRY=="Syrian Arab Republic")] <- "Syria"
	r$COUNTRY[which(r$COUNTRY=="Slovakia (Slovak Republic)")] <- "Slovakia"
	r$COUNTRY[which(r$COUNTRY=="Congo, the Democratic Republic of")] <- "Congo"
	

	latlon_missing_c <- r$COUNTRY[which(is.na(r$Longitude == TRUE))]
	latlon_missing_l <- r$LOC[which(is.na(r$Longitude == TRUE))]
	
	# locs <-  NULL
	# loc <- strsplit(latlon_missing_l, ",", fixed = TRUE)
	# for (l in 1:length(loc)){
	#   locs <- c(locs,loc[[l]][1])
	# }
	
	#carobiner::geocode(latlon_miss[1,1], latlon_miss[1,2])
	
	# There are about 608 records missing latlon values out of the total 4830
	# Isolate those missing latlon to be processed later
	
	r <- r[-which(is.na(r$Longitude == TRUE)),]
	d <- data.frame(
		trial_id = r$id,
		soil_pH = as.numeric(r$PHH2O),
		soil_pH_KCl = as.numeric(r$PHKCL),
		soil_pH_CaCl2 = as.numeric(r$PHCACL2),
		soil_SOC = as.numeric(r$ORGC),
		soil_Ca = as.numeric(r$CA),
		soil_K = as.numeric(r$K),
		soil_Mg = as.numeric(r$MG),
		soil_Na = as.numeric(r$NA.),
		soil_EC = as.numeric(r$EC),
		soil_Ex_Al = as.numeric(r$EXAL),
		soil_Ex_acidity = as.numeric(r$EXACID),
		soil_CEC = as.numeric(r$CECSOIL),
		soil_clay = as.numeric(r$CLAY),
		soil_silt = as.numeric(r$TSI),
		soil_sand = as.numeric(r$TSA),
		country = r$COUNTRY,
		longitude = r$Longitude,
		latitude = r$Latitude
	)

    carobiner::write_files(path, dset, d)
}
