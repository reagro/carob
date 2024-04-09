# R script for "carob"

## ISSUES
#need to find information on planting and harvesting dates from author


carob_script <- function(path) {

"This dataset was generated from the research conducted to evaluate 
the impact of intercropping on crop productivity and drought resistance in 
terms of crop yield, whole-system yield based on nutritional yields and 
Land Equivalent Ratio, and yield loss due to drought. 
The experiment was conducted at a previously established ICRAF field trial in Manyusi village, 
Kongwa District, Dodoma, Tanzania. 
The drought was imposed in the field using rainout shelters on five cropping systems with and
without fertilizer application in the 2019 cropping season. 
The treatment factors and levels were as follows: 
cropping system (sole maize, sole pigeonpea, maize-pigeonpea, maize-gliricidia, and maize-pigeonpea-gliricidia, fertilization (fertilized, unfertilized), 
and water (ambient rainfall, drought). The experiment layout was a randomized split-split-plot with the main plot in a randomized complete block design with three replications.
The drought was imposed beginning at maize anthesis and continued past harvests of maize and pigeonpea. 
Data were collected in the field on-site rainfall, air temperature and relative humidity, and photosynthetically active radiation, 
and in the laboratory on crop grain biomass, gravimetric water content, and soil properties using crop and soil samples from the field.
Crop grain yields were calculated from laboratory measurements and used to calculate nutritional yield, land equivalent ratio, and 
drought resistance, vapor pressure deficit was calculated from air temperature and relative humidity, and 
the fraction of transmitted photosynthetically active radiation was calculated from field-measured data.
"

#### Identifiers
	uri <- "doi:10.7910/DVN/G4QLNP"
	group <- "conservation_agriculture"
	dataset_id <- carobiner::simple_uri(uri)
	

	ff  <- carobiner::get_data(uri, path, group)


	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=2, minor=3),
		dataset_id = dataset_id,
		data_institutions = "IFPRI,ICRAF",
		publication= "doi.org/10.3389/fsufs.2020.562663",
		project=NA,
		data_type= "experiment",
		carob_contributor= "Mitchelle Njukuya",
		carob_date="2024-04-09"
	)
	
	f <- ff[basename(ff) == " 006_siDom_droughtResistance.csv"]
	#f1 <- ff[basename(ff) == "007_siteCharacterization_droughtResistance.csv"]
	r <- read.csv(f)
	#r1 <- read.csv(f1)
	
## process file(s)
	d <- data.frame(trial_id=r$Plots30,rep=r$Blk,treatment=r$Trt.Code,year=r$Year,intercrop=r$CrpSystm,yield=r$Maize.Grain.Production..t.ha.*1000)
  
	#excel sheet for abbreviations indicated the following system names:
	treatcode= c("T1","T2","T3","T4","T5")
	treatnames= c("maize","maize+pigeon pea","pigeon pea","maize+gliricidia","maize+pigeon pea+gliricidia")
	
	d$treatment <- treatnames[match(r$Trt.Code,treatcode)]
	
	#fixing intercrop names 
	cropcodes = c("M","MP","P","MG","MGP")
	cropnames = c("sole maize","maize+pigeon pea","sole pigeon pea","maize+gliricidia","maize+pigeon pea+gliricidia")
	
	d$intercrop <- cropnames[match(r$CrpSystm,cropcodes)]
	d$on_farm <- TRUE
	d$is_survey <- FALSE

  # Location information was accessed from published paper
	d$country <- "Tanzania"
	d$location <- "Dodoma Region"
	d$site <- "Kogwa District"
	d$adm1 <- "Manyusi Village"
	d$longitude <- 36.296
	d$latitude <- -5.5656
	d$elevation <- 1206.6

  # Crop variety names are from published paper
	d$previous_crop <- "maize"
	d$variety[d$intercrop=="sole maize"] <- "Staha"
	d$variety[d$intercrop=="sole pigeon pea"] <- "ICEAP 0040"
	d$variety[d$intercrop=="maize+pigeon pea"] <- NA
	d$variety[d$intercrop=="maize+gliricidia"] <- NA
	d$variety[d$intercrop=="maize+pigeon pea+gliricidia"] <- NA
	#crop densities
	d$plant_density[d$intercrop=="sole maize"] <- 44.444
	d$plant_density[d$intercrop=="maize+pigeon pea"] <- 88.889
	d$plant_density[d$intercrop=="sole pigeon pea"] <- 44.444
	d$plant_density[d$intercrop=="maize+gliricidia"] <- 88.889
	d$plant_density[d$intercrop=="maize+pigeon pea+gliricidia"] <- 88.889
	
	
   #dataset and publication has no information on planting and harvesting dates
  #d$planting_date <- as.character(as.Date(   ))
  #d$harvest_date  <- as.character(as.Date(    ))
	
   d$P_fertilizer <- 15
   d$N_fertilizer <- 43
   d$fertlizer_type <- "diammonium phosphate+urea"
   d$yield_part <- "grain"
	
# all scripts must end like this
	carobiner::write_files(path, dset, d)
}



