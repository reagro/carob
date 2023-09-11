# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:
TAMASA Agronomy Panel Survey in Nigeria (2016) (2016)
"

	uri <- "hdl:11529/10548307"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "crop_cuts"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project="TAMASA",
		uri=uri,
		data_citation="Masuki, Kenneth; Chamberlin, Jordan, 2019, Tamasa APS Tanzania 2016, https://hdl.handle.net/11529/10548307, CIMMYT Research Data & Software Repository Network, V1, UNF:6:ROFhHRpFl3nj0rn+rxjaIA== [fileUNF]",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= NA,
		data_institutions = "CIMMYT",
   	data_type="survey",
		carob_contributor="Shumirai Manzvera" 
	)

## download and read data 
	#path<-("C:/Users/USER/OneDrive/Documents/datasets")
  path<-("C:/carob/scripts/crop_cuts")
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)


	f <- ff[basename(ff) == "TAMASA_TZ_APS_CC_2016.xlsx"]

	r <- TAMASA_TZ_APS_CC_2016
	d<-r


	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$is_experiment <- FALSE
	d$irrigated <- FALSE


##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- d$Country
	d$site <- d$Zone
	d$adm1 <- d$Region
	d$adm2 <- d$District
	d$adm3 <- d$Ward
	d$adm4<-d$Village
	d$elevation <- d$Altitude
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- d$Longitude
	d$latitude <- d$Altitude

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "maize"

   
##### in general, add comments to your script if computations are
##### based on information gleaned from metadata, a publication, 
##### or when they are not immediately obvious for other reasons

##### Yield #####
	d$biomass_total <- 

	d$yield <- d$`Grain yield (kg/ha@12.5%)`
	#what plant part does yield refer to?
	d$yield_part <- 
	  
	###SOIL PROPERTIES
	  r2<-TAMASA_TZ_APS_Soil_2016
	d2<-r2
	d2$country <- d2$Country
	d2$site <- d2$Zone
	d2$adm1 <- d2$Region
	d2$adm2 <- d2$District
	d2$adm3 <- d2$Ward
	d2$adm4<-d2$Village
	d2$elevation <- d2$Altitude
	d2$longitude <- d2$Longitude
	d2$latitude <- d2$Altitude
	
	d2$soil_pH<- d2$pH
	d2$soil_N<-d2$N
	d2$soil_K<-d2$K
	d2$soil_Ca<-d2$Ca
	d2$soil_Mg<-d2$Mg
	
	###TZ_TAMASA_BYS_Yield_2015_22June17
	 
	r3<-TZ_TAMASA_BYS_Yield_2015_22June17
	r3$country <- r3$Country
	r3$site <- r3$Zone
	r3$adm1 <- r3$Region
	r3$adm2 <- r3$District
	r3$adm3 <- r3$Ward
	r3$adm4<-r3$Village
	r3$elevation <- r3$Altitude
	r3$longitude <- r3$Longitude
	r3$latitude <- r3$Latitude
	
	r3$crop <- "maize"
	r3$yield <- r3$`Grain yield (kg/ha)`
	
#soil data for 2015
	r4<-TZ_TAMASA_BYS_Yield_2015_22June17
	r4$country <- r4$Country
	r4$adm1 <- r4$Zone
	r4$adm2 <- r4$Region
	r4$adm3 <- r4$District
	r4$adm4<-r4$Ward
	r4$elevation <- r4$Altitude
	r4$longitude <- r4$Longitude
	r4$latitude <- r4$Latitude
	
	r4$soil_pH<-r4$pH 
	r4$soil_N<-r4$N
	r4$soil_K<-r4$K
	r4$soil_Ca<-r4$Ca
	r4$soil_Mg<-r4$Mg
	
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)

