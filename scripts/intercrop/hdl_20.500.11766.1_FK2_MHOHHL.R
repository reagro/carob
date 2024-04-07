# R script for "carob"

# ISSUES

# both r2 and r4 have yield, but they do not match!
# no data on (relative) plant density or the intercropping method


carob_script <- function(path) {

"The dataset contains the description and results of a field experiment performed under the project 'Designing InnoVative plant teams for Ecosystem Resilience and agricultural Sustainability (DIVERSify)' in Kfardan, Lebanon in 2018. It contains sheets about plot information, plot level data, species level data, field metadata and an image of the field plan

The trial includes 40 faba bean varieties and 2  wheat varieties that are grown as sole crop or intercrop."

	uri <- "hdl:20.500.11766.1/FK2/MHOHHL"
	group <- "intercrop"
	ff <- carobiner::get_data(uri, path, group)

	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=3, minor=0),
		project="DIVERSify",
		publication=NA,
		data_institutions = "ICARDA",
		carob_contributor="Samar Attaher",
		carob_date="2023-04-05",
		data_type="experiment"
	)
	
	f1 <- ff[basename(ff) == "01_Plot_Information.csv"] 	
	r1 <- read.csv(f1, sep=";")
	
	f2 <- ff[basename(ff) == "02_Plot_Level_Data.csv"] 	
	r2 <- read.csv(f2, sep=";")
	
	f3 <- ff[basename(ff) == "03_Species_Level_Data.csv"] 	
	r3 <- read.csv(f3, sep=";")

	f5 <- ff[basename(ff) == "05_Metadata_Field.csv"] 	
	r5 <- read.csv(f5, sep=";")
	
	
	d1 <- data.frame(
		rep = r1$Rep,
		country = "Lebanon",
		adm1 = "Baalbek-Hermel Governorate",
		adm2 = "Baalbek",
		adm3 = "Kfar Dan",
		site = "Lebanese Agricultural Research Institute station",
		latitude = r5$Latitude,
		longitude = r5$Longitude,
		elevation = r5$Altitude,
		record_id = r1$PlotCode,
		treatment = paste(r1$CropCombination, r1$VarietyCombination, sep = "_"),
		planting_date = "2018-02",	
		rain = 194.4,
		irrigation_number = 2L, # see r5
		irrigation_amount = 60, # see r5
		irrigation_dates = "2018-04-15;2018-05-15", # see r5
		herbicide_product = tolower(r5$MajorHerbicidesMixture),
		trial_id = "1"
	) 
	
	d1$variety_wheat = NA
	d1$variety_wheat[grepl("Marguerita", r1$VarietyCombination)] <- "Marguerita"
	d1$variety_wheat[grepl("Miki", r1$VarietyCombination)] <- "Miki"

	d1$intercrops <- "none"
	d1$intercrops[r1$NumberOfPlantSpecies==2] <- "faba bean; durum wheat"

	d3 <- data.frame(
		record_id = r3$PlotCode,
		harvest_date = as.character(as.Date(paste(r3$Year, r3$MonthGrainYield, r3$DayGrainYield, sep="-")))
	)

	d13 <- merge(d1, d3, "record_id")

	dfaba <- data.frame(
		record_id = r2$PlotCode,
		crop = "faba bean",
		flowering = r2$DFLRFB, 
		maturity = r2$DMATFB , 
		plant_height= r2$FBPLHT,
		yield_part = "seed",
		yield = r2$FBGY, #kg/ha
		insecticide_product = "imidacloprid; lambdacyhalothrin",
		pest_number = r2$Pest 
	) 

	i <- is.na(r2$Faba_Bean_Pedigree)
	r2$Faba_Bean_Pedigree[i] <- r2$Faba_Bean_PN[i]
	r2$Faba_Bean_PN[i] <- NA
	variety <- paste0(r2$Faba_Bean_Pedigree, " (", r2$Faba_Bean_PN, ")")
	variety <- gsub(" \\(NA)", "", variety)
	variety <- gsub("^NA", "", variety)
	variety[variety == ""] <- NA
	dfaba$variety <- variety

	dwheat <- data.frame(
		record_id = r2$PlotCode,
		crop = "wheat",
		flowering = r2$DFLRWT, 
		maturity = r2$DMATWT, 
		plant_height = r2$WTPLHT, 
		yield_part="grain",
		yield = r2$WTGY
	) 
	
	dfaba <- merge(d13, dfaba, by="record_id", all.x=TRUE)
	dwheat <- merge(d13, dwheat,  by="record_id", all.y=TRUE)
		
	dwheat$variety <- dwheat$variety_wheat
	
	d <- carobiner::bindr(dfaba, dwheat)
	d <- d[!is.na(d$yield), ]
	d$variety_wheat <- NULL
	d$record_id <- as.integer(as.factor(d$record_id))
	
	carobiner::write_files (path, dset, d)
}
