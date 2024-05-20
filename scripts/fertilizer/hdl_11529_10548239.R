# R script for "carob"

## ISSUEs


carob_script <- function(path) {

"
Nutrient Omission Trials (NOTs) conducted in two zones (West Showa and Jimma) 
  in Ethiopia in 2015 and 2016. Trials comprise six nutrient management treatments, 
  namely Control (zero fertilizer), PK, NK, PK, NPK, NPK+Ca+Mg+Zn+B. Trials were 
  conducted on-farm with six plots per farm. Observations include soil analysis (0-20cm), 
  biomass and grain yields (2016)"

#### Identifiers
	uri <- "hdl:11529/10548239"
	group <- "maize_trials"

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)

##### dataset level metadata 
	dset <- data.frame(
		# change the major and minor versions if you see a warning
		carobiner::read_metadata(uri, path, group, major=1, minor=1),
		data_institutions = "Tamasa",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= NA,
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		project=NA,
		data_type= "experiment",
		carob_contributor= "Shumirai Manzvera",
		carob_date="2024-05-16"
	)
	
##### PROCESS data records

# read data 

	f <- ff[basename(ff) == "TAMASA_NOTs_Database_Ethiopia_2015_2016.xlsx"]
	r <- carobiner::read.excel.hdr(f, sheet = "Raw_data Harvest parameters",skip = 1, hdr = 1)
	r0 <- carobiner::read.excel(f, sheet = "Fertilizer rates & variety")
	#r <- carobiner::read.excel(f, sheet = "Raw_data Harvest parameters")
	
	# or  r <- carobiner::read.excel(f)

## process file(s)

## select the variables of interest and assign them to the correct name
	d <- data.frame(
		#crop=r$crop, 
	  #latitude=r$Latitude.Decimal.Degrees,
	  #longitude=r$Longitude.Decimal.Degrees
	  #error in coordinates, coordinates not on land for: Ethiopia
		latitude= 9.145,
		longitude=40.48967,
		elevation=r$Altitude.m.a.s.l,
		treatment=r$Experimental.design_Treatment,
		yield = r$Grain.Yield.kg.ha,
		variety=r$Maize.variety.name.MVnam,
		moist=r$Grain.moisture.content.pct,
		country=r$Site.location.information_Country,
		site=r$Region,
		adm1=r$District,
		N_fertilizer=120,
		P_fertilizer=40/2.29,
		K_fertilizer=40/1.2051,
		S_fertilizer=20,
		trial_id = "1"

	)

	
	d$country <- "Ethiopia"
	

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "maize"


##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	#fixing bad data
  r$Agronomic.management_Planting.date.PLNdat[r$Agronomic.management_Planting.date.PLNdat == '2/6//2016'] <- '2/6/2016'
  r$Agronomic.management_Planting.date.PLNdat[r$Agronomic.management_Planting.date.PLNdat == '30/52016'] <- '30/5/2016'
  dd1 <- as.numeric(r$Agronomic.management_Planting.date.PLNdat)
  dd1 <- as.character(as.Date(dd1, origin = "1900-01-01"))
  
  dd2 <- r$Agronomic.management_Planting.date.PLNdat
  dd2[nchar(dd2) <6] <- NA
  dd2 <- as.character(as.Date(dd2, format = '%d/%m/%Y'))
  dd1 <- ifelse(is.na(dd1), dd2, dd1)
  
	d$planting_date <- dd1
	dd3<- r$Harvest.date.HDATE
	dd3<-as.character(as.Date(dd3, format = '%d/%m/%Y'))
	
	
	d$harvest_date  <- dd3
   
	#what plant part does yield refer to?
	d$yield_part <- "grain"
	d$on_farm = TRUE
	d$striga_trial = FALSE 
	d$striga_infected = FALSE
	d$borer_trial = FALSE
	d$exp_treatments="experiments"
	
# all scripts must end like this
	carobiner::write_files(path, dset, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

