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
		publication= NA,
		carob_contributor="Cedric Ngakou",
		carob_date="2023-03-19",
		data_type="experiment",
		data_institutions="IITA"
 	)
	
	## download and read data 
	
	ff	 <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)
	
	
	f <- ff[basename(ff) == "20181025aao_Combd_BA15150_Final_DS_data.csv"]
	f1 <- ff[basename(ff) == "20181029aao_Combd_BA15150_Final_WW_data.csv"]
	 
	r <- read.csv(f)
	r1 <- read.csv(f1)
	
	## process file(s)
	rr <- r[,c("Country","Location","Rep","YR","YIELD","POLLEN","DYSK","PLHT","EHT","PASP","EROT","ASI","EASP")]	
	colnames(rr) <- c("country","location","rep","planting_date","yield","dy_tass","dy_sk","pl_ht","e_ht","p_asp","erot","asi","e_asp")
	
	rr$trial_id <- paste0(r$Pedigree, '-', r$ENV)
	rr$season <- r$Study
	
	r11 <- r1[,c("Country","LOC","Rep","YEAR","YIELD","POLLEN","DYSK","PLHT","EHT","PASP","EROT","ASI","EASP")]	
	colnames(r11) <- c("country","location","rep","planting_date","yield","dy_tass","dy_sk","pl_ht","e_ht","p_asp","erot","asi","e_asp")
	
	r11$trial_id <- paste0(r1$Pedigree, '-', r1$ENV)
	r11$season <- r1$Study
	
	d	 <- rbind(rr,r11)
	d$dataset_id <- dataset_id
	d$country <- "Nigeria"
	# Fix country name base on location 
	p <- carobiner::fix_name(gsub("/", "; ", d$location))
	p <- gsub("IKENNE", "Ikenne", p)
	p <- gsub("INA", "Baba Ina", p)
	p <- gsub("MABaba Ina-HARI", "Baba Ina", p)
	d$location <- p
	
	d$country[d$location %in% c("KPEVE", "NYANKPALA", "FUMESUA")] <- "Ghana"
	d$country[d$location=="ANGARADEBOU"] <- "Benin"
	d$country[d$location=="MANGA"] <- "Burkina Faso"
	## each site must have corresponding longitude and latitude
	loc <- data.frame(location = c("Ikenne", "KPEVE", "NYANKPALA", "FUMESUA", "ANGARADEBOU", "ZARIA", "MOKWA", "DUSU", "BAGAUDA", "IFE", "MAINA-HARI", "MANGA","Baba Ina"), 
	longitude = c(3.698, 0.333, -0.981, -1.512, 3.041, 7.652, 5.054, 12.367, 8.385, 4.560, 12.158, -1.072, 12.407), 
	latitude = c(6.901, 6.685, 9.400, 6.714, 11.323, 11.025, 9.296, 8.817, 11.570, 7.483, 10.679, 11.667, 8.952))
	d <- merge(d, loc, by="location", all.x=TRUE)
		 
	d$borer_trial <- FALSE
	d$striga_trial <- FALSE
	d$striga_infected <- FALSE
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$planting_date <- as.character(d$planting_date)

	# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

