# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
"
Description:
The objectives of this study were to examine yield gains
in the cultivars and to investigate inter-trait relationships
and yield stability under six drought and 17 rainfed conditions
in West Africa from 2013 to 2016.

"
	uri <- "doi:10.25502/20181101/1228/BB"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation ="Baffour Badu-Apraku. (2018). Gains in Grain Yield of Extra-early Maize during Three Breeding Periods under and Rain-fed Conditions [Data set]. International Institute
		of Tropical Agriculture (IITA).	https://doi.org/10.25502/20181101/1228/BB",
		data_institutions = "IITA",
		publication= NA,
		carob_contributor="Cedric Ngakou",
		experiment_type=NA, # please have a look on experiment type 
		has_weather=FALSE,
		has_soil=FALSE,
		has_management=FALSE
	)
	
	## download and read data 
	
	ff	<- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)
	
	
	f <- ff[basename(ff) == "20181025aao_Combd_BA15150_Final_DS_data.csv"]
	f1 <- ff[basename(ff) == "20181029aao_Combd_BA15150_Final_WW_data.csv"]
	 
	r <- read.csv(f)
	r1 <- read.csv(f1)
	
	## process file(s)
	rr <- r[,c("Country","Location","Rep","YR","YIELD","POLLEN","DYSK","PLHT","EHT","PASP","EROT","ASI","EASP")]	
	colnames(rr)<-c("country","location","rep","planting_date","yield","dy_tass","dy_sk","pl_ht","e_ht","p_asp","erot","asi","e_asp")
	
	rr$trial_id <- paste0(r$Pedigree, '-', r$ENV)
	rr$season<-r$Study
	
	r11 <- r1[,c("Country","LOC","Rep","YEAR","YIELD","POLLEN","DYSK","PLHT","EHT","PASP","EROT","ASI","EASP")]	
	colnames(r11)<-c("country","location","rep","planting_date","yield","dy_tass","dy_sk","pl_ht","e_ht","p_asp","erot","asi","e_asp")
	
	r11$trial_id <- paste0(r1$Pedigree, '-', r1$ENV)
	r11$season<-r1$Study
	
	d	<-rbind(rr,r11)
	d$dataset_id <- dataset_id
	d$country<- "Nigeria"
	# Fix country name base on location 
	p <- carobiner::fix_name(gsub("/", "; ", d$location))
	p <- gsub("IKENNE", "Ikenne", p)
	p <- gsub("INA", "Baba Ina", p)
	p <- gsub("MABaba Ina-HARI", "Baba Ina", p)
	d$location<- p
	
	d$country[d$location %in% c("KPEVE", "NYANKPALA", "FUMESUA")] <- "Ghana"
	d$country[d$location=="ANGARADEBOU"] <- "Benin"
	d$country[d$location=="MANGA"] <- "Burkina Faso"
	## each site must have corresponding longitude and latitude
	loc <- data.frame(location = c("Ikenne", "KPEVE", "NYANKPALA", "FUMESUA", "ANGARADEBOU", "ZARIA", "MOKWA", "DUSU", "BAGAUDA", "IFE", "MAINA-HARI", "MANGA","Baba Ina"), 
	longitude = c(3.6977469, 0.3326709, -0.981456, -1.5119402, 3.0412812, 7.6518533, 5.0544281, 12.366667, 8.38546, 4.5604451, 12.1577, -1.0723972,12.40664), 
	latitude = c(6.9010051, 6.6851678, 9.400463, 6.7143898, 11.3228338, 11.0248119, 9.2957202, 8.816667, 11.5696, 7.482824, 10.6788, 11.6673837,8.95185))
	d <- merge(d, loc, by="location", all.x=TRUE)
		 
	d$striga_trial <- FALSE
	d$striga_infected <- FALSE
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$planting_date <- as.character(d$planting_date)

	# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

