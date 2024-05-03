# R script for "carob"

carob_script <- function(path) {
  
"
Drought is a key maize (Zea mays L.) production constraint in sub-Saharan Africa. Fourteen, fifteen, and twenty-five extra-early maturing maize cultivars, with varying Striga resistance and drought and low soil N tolerance, were developed from 1995 to 2000 (Period 1), 2001 to 2006 (Period 2), and 2007 to 2012 (Period 3), respectively. The objectives of this study were to examine yield gains in the cultivars and to investigate inter-trait relationships and yield stability under six drought and 17 rainfed conditions in West Africa from 2013 to 2016. Annual rate of yield increase across cultivars was 0.034 (3.28%) and 0.068 Mg ha−1 (2.25%), whereas yield gains per period were 0.17 and 0.38 Mg ha−1 under drought and rainfed environments, respectively. Yield gains under drought and rainfed environments were related to prolonged flowering period, increased plant and ear heights, improved stalk lodging, and ear and plant aspects, whereas delayed leaf senescence and increased number of ears per plant accompanied yield improvement under drought only. Ear aspect and number of ears per plant were primary contributors to yield and could be used as selection criteria for yield enhancement under drought and rainfed conditions. High-yielding and stable cultivars across all environments based on additive main effects and multiplicative interaction (AMMI) biplot included ‘2004 TZEE-Y Pop STR C4’ and ‘TZEE-W Pop STR BC2 C0’ of Period 2 and ‘2009 TZEE-W STR’, ‘TZEE-Y STR 106’, ‘TZEE-W STR 107’, and ‘TZEE-W DT C0 STR C5’ of Period 3. These cultivars could be commercialized to improve food self-sufficiency in sub-Saharan Africa.
"

	uri <- "doi:10.25502/20181101/1228/BB"
	group <- "maize_trials"

	ff	 <- carobiner::get_data(uri, path, group)
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=2, minor=1),
		project=NA, 
		publication= NA, 
		carob_contributor="Cedric Ngakou", 
		carob_date="2023-03-19", 
		data_type="experiment", 
		data_institutions="IITA"
 	)
	
	f <- ff[basename(ff) == "20181025aao_Combd_BA15150_Final_DS_data.csv"]
	f1 <- ff[basename(ff) == "20181029aao_Combd_BA15150_Final_WW_data.csv"]
	 
	r <- read.csv(f)
	r1 <- read.csv(f1)
	
	rr <- r[, c("Country", "Location", "Rep", "YR", "YIELD", "POLLEN", "DYSK", "PLHT", "EHT", "PASP", "EROT", "ASI", "EASP")]	
	colnames(rr) <- c("country", "location", "rep", "planting_date", "yield", "tassling_days", "silking_days", "plant_height", "e_ht", "p_asp", "erot", "asi", "e_asp")
	
	rr$trial_id <- paste0(r$Pedigree, '-', r$ENV)
	rr$season <- r$Study
	
	r11 <- r1[, c("Country", "LOC", "Rep", "YEAR", "YIELD", "POLLEN", "DYSK", "PLHT", "EHT", "PASP", "EROT", "ASI", "EASP")]	
	colnames(r11) <- c("country", "location", "rep", "planting_date", "yield", "tassling_days", "silking_days", "plant_height", "e_ht", "p_asp", "erot", "asi", "e_asp")
	
	r11$trial_id <- paste0(r1$Pedigree, '-', r1$ENV)
	r11$season <- r1$Study
	
	d	 <- rbind(rr, r11)
	
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
	loc <- data.frame(location = c("Ikenne", "KPEVE", "NYANKPALA", "FUMESUA", "ANGARADEBOU", "ZARIA", "MOKWA", "DUSU", "BAGAUDA", "IFE", "MAINA-HARI", "MANGA", "Baba Ina"), 
	longitude = c(3.698, 0.333, -0.981, -1.512, 3.041, 7.652, 5.054, 12.367, 8.385, 4.560, 12.158, -1.072, 12.407), 
	latitude = c(6.901, 6.685, 9.400, 6.714, 11.323, 11.025, 9.296, 8.817, 11.570, 7.483, 10.679, 11.667, 8.952))
	d <- merge(d, loc, by="location", all.x=TRUE)
		 
	d$borer_trial <- FALSE
	d$striga_trial <- FALSE
	d$striga_infected <- FALSE
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$planting_date <- as.character(d$planting_date)

	carobiner::write_files(dset, d, path=path)
}

