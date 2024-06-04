# R script for "carob"


## TODO:
# specifiy intercrops 

carob_script <- function(path) {
  
"The dataset contains the description and results of a field experiment performed under the project “Designing InnoVative plant teams for Ecosystem Resilience and agricultural Sustainability (DIVERSify)” in Kfardan, Lebanon in 2018. It contains sheets about plot information, plot level data, species level data, field metadata and an image of the field plan."
  
	uri <- "hdl:20.500.11766.1/FK2/MHOHHL"
	group <- "intercrops"
	ff  <- carobiner::get_data(uri, path, group)


	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=3, minor=0),
		project="DIVERSify",		 
		publication=NA,
		data_institute = "ICARDA",
		carob_contributor="Layal Atassi",
		carob_date="2024-03-24",
		data_type="experiment",
		treatment_vars = NA
	)

	f1 <- ff[basename(ff) == "03_Species_Level_Data.csv"]
	f2 <- ff[basename(ff) == "01_Plot_Information.csv"]
	
	r1 <- read.csv(f1, sep = ";")
	r2 <- read.csv(f2, sep = ";")
	
	# Merge data frames
	r <- merge(x = r1, y = r2, by = c("PlotCode", "PlantPartner", "Rep", "Year"), all.x = TRUE)
	
	d <- data.frame(
		location = r$Site,
		yield = r$GrainYield,
		crop = r$CropSpeciesCommonName,
		variety = r$CropVariety,
		latitude = r$Lat,
		longitude = r$Long,
		emergence_date = paste(r$Year, r$MonthPlantEmergence, r$DayPlantEmergence, sep="-"),
		harvest_date = paste(r$Year, r$MonthGrainYield, r$DayGrainYield, sep="-"),
		rep = r$Rep,
		plant_density = r$PlantsEmergence_m2 * 1000
	)

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	
	##### Location #####
	d$country <- "Lebanon"
	d$adm1 <- "Baalbek-Hermel"
	d$adm2 <- "Baalbek"
	d$location <- "Kafardan"
	d$rain <- 194.4
	d$elevation <- 1080
	
	carobiner::write_files(dset, d, path=path)
}