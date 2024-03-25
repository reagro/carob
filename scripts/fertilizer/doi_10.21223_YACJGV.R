# R script for "carob"

## ISSUES
# yield corrected in two cases after inspecting 
# other yield values in the same trial (location)


carob_script <- function(path) {
	  
"Description:
	Fertilizer response trials to compare the performance of 5 'best-bet' fertilizer recommendations with the current blanket recommendation in multiple locations and farms. The best-bets differ in N:P:K ratios and rates and are designed based on assumptions on how fertilizer responses may vary across locations and fields. The five treatments are compared in each site with only the reference treatment replicated. Tuber yield as well as secondary agronomic data were assessed. Data were collected using the ODK-based digital data collection tool 'Smart Agronomy Data Management System (SAnDMan)'."
	  
	uri <- "doi:10.21223/YACJGV"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
		project=NA,
		#data_citation = "Vandamme, Elke, 2023. Dataset for: Fertilizer response trials to calibrate and cross-validate AKILIMO for potato in Rwanda. https://doi.org/10.21223/YACJGV, International Potato Center, V1, UNF:6:dqyMNI9EnXyX0pNaGvS+hQ== [fileUNF]",
		publication= NA,
		data_institutions = "CIP",
		data_type="experiment", 
		carob_contributor="Njogu Mary, Stephen Gichuhi",
		carob_date="2023-01-22"
	)
	 
	f1 <- ff[basename(ff) == "Scaling_AKILIMO_yield_data.csv"]
	r <- read.csv(f1)

	f2 <- ff[basename(ff) == "DataDictionary_SA-VAP-1.xlsx"]  
	fert <- carobiner::read.excel(f2, sheet="Fertilizer treatments", skip=1)
	
	## use a subset
## do not use column numbers. Always use names. Numbers cannot be directly interpreted. 
## 	d <- r[,c(7,54,3,14,16,17)]
  
	d <- data.frame(country="Rwanda", dataset_id=dataset_id, 
			crop="potato", yield_part = "tubers",
			yield=r$tuberY * 1000, 
			latitude=r$Latitude, longitude=r$Longitude, 
			elevation=r$Altitude, treatment=r$treat)
 
	d$harvest_date <- as.character(as.Date(r$today, format= "%d-%b-%y"))
	d$trial_id <- as.character(as.integer(as.factor(paste(d$latitude, d$longitude))))

## but we can read this directly from the file 
##	npk <- data.frame(
##		treatment = c("NPK6", "NPK4_MOP2", "NPK4_UREA2", "NPK4_DAP2", "NPK11"), 
##		N_fertilizer = c(51, 34, 80, 52, 94), 
##		P_fertilizer = c(22, 15, 15, 35, 41),
##		K_fertilizer = c(42, 78, 28, 28, 78)) 
##		d <- merge(d, npk, by="treatment")

	colnames(fert)[1] <- "treatment"
	fert$treatment <- gsub("_rep.$", "", fert$treatment)
	fert <- unique(na.omit(fert[, c("treatment", "N", "P", "K")]))
	colnames(fert)[-1] <- paste0(colnames(fert)[-1], "_fertilizer")

	d <- merge(d, fert, by="treatment")
	
## fixing two cases 
	i <- d$yield > 100000
	d$yield[i] <- d$yield[i] / 10

	carobiner::write_files(dset, d, path=path)
}


