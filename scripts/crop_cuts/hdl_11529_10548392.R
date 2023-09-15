# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:Agronomy of 100 fields is part of APS(AgronomyPanelSurvey) Study (2016)"



	uri <- "hdl:11529\10548392"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "crop_cuts"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project= "TAMASA",
		uri=uri,
		data_citation="Kebede Mesfin, 2020, TAMASA Agronomy Of 100 Fields Ethiopia, https://hdl.handle.net/11529/10548392, CIMMYT Research Data & Software Repository Network, V1, UNF:6:7dCgjyXCytcBgdc+1s0T5A== [fileUNF]",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= NA,
		data_institutions = "CIMMYT",
   		data_type="survey",
		carob_contributor="Shumirai Manzvera"  
	)

## download and read data 
path<-("C:/carob/scripts/crop_cuts")
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	dset$license <- carobiner::get_license(js)


	f <- ("C:/carob/scripts/crop_cuts/data/raw/crop_cuts/hdl_11529_10548392")
	##TAMASA_ET_CC_2015_BakoF <- read_excel("scripts/crop_cuts/data/raw/crop_cuts/hdl_11529_10548392/TAMASA_ET_CC_2015_BakoF.xlsx", 
	   #                                   +     sheet = "Raw_Data")
r<- TAMASA_ET_CC_2015_BakoF
#	r <- readxl::read_excel(f)
#	r <- readxl::read_excel(f) |> as.data.frame()

	
## process file(s)

## use a subset
	d <- r

	
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
	d$country <-d$"Ethiopia"
	d$adm1 <- "Oromia"
	d$adm2 <- "West Showa"
	d$adm3 <- "Bako"
	d$adm4 <- d$`Name of the Village`
	d$elevation <- NA
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- 35.52-42.12

	d$latitude <-  6.58-11.77

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "maize"
#variety
	d$variety<-d$`Type of Variety`
 #farming type 
	d$intercrops <- d$`Cropping System`
	d$intercrops<-d$`Intercropping with legume`
	d$croprotation<-d$`Crop Rotation with`
	d$crop_rotation<-d$croprotation
	d$previous_crop<-d$`Previous/precursor crop`

##### Fertilizers #####

## normalize names 
   d$fertlizer_type <- d$`Type of Inorganic Fertilizer`
   d$inoculated <- FALSE
   d$OM_used<-d$`Apply Organic Fertilizer ?`
   d$OM_type<-d$`Type of Organic Fertilizer applied`
   d$soil_type<-d$`Soil type`
   d$soil_pH<-d$pH
   d$soil_K<-d$`K (mg kg-1)`
   d$soil_Mg<-d$`Mg (mg kg-1)`
   d$soil_Ca<-d$`Ca (mg kg-1)`
   
  
##### Yield #####
	d$biomass_total <- 

	d$yield <- d$`Average yield kg/ha or (Q1+Q2)/2`
	#what plant part does yield refer to?
	d$yield_part <- d$`Dry wt of cobs in (4mX4m) Q1`+d$`Dry wt of cobs /Q2`
	
	#TAMASA_ET_CC_2015_CSAF
	r2<- TAMASA_ET_CC_2015_CSAF
	d2<-r2
	#### about the data #####
	## (TRUE/FALSE)
	
	d$dataset_id <- dataset_id
	d2$on_farm <- FALSE
	d2$is_survey <- TRUE
	d2$is_experiment <- FALSE
	d2$irrigated <- FALSE
	
	
	
	##### Location #####
	## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
	## you can use carobiner::fix_name()
	
	d2$adm1 <- "Oromia,Amhara"
	d2$adm2 <- "Jimma, Wollega, West Showa, East Showa, Gojjam"

	d2$Latitude<- 7.10-11.59 
	d2$Longitude<-35.32-40.59
	#crop 
	d2$crop <- "maize"
	#variety
	d2$variety<-d2$`Name of variety`
	d2$inoculated <- FALSE
	#fertiliser
	d2$OM_used<-d2$`Fertilizer type/organic`
	#soil characteristics
	d2$soil_pH<-d2$pH
	d2$soil_K<-d2$`K  (mg/kg)`
	d2$soil_Mg<-d2$`Mg  (mg/kg)`
	d2$soil_Ca<-d2$`Ca  (mg/kg)`
	d2$soil_P_total<-d2$`P  (mg/kg)`
	
	#yield
	d2$yield <- d2$`Moisture adjusted grain  yield (kg /ha)`
	
	#TAMASA_ET_CC_2015F
	r3<- TAMASA_ET_CC_2015F
	d3 <- r3
	d3$on_farm <- FALSE
	d3$is_survey <- TRUE
	d3$is_experiment <- FALSE
	d3$irrigated <- FALSE
	#location of the village
	d3$adm1 <- "Oromia, Amhara and Southern regional state of Ethiopia" 
	d3$adm2 <- "Jimma, Ili Ababura, Wollega, East Showa, Gojjam, Woliyta"

	#each site must have corresponding longitude and latitude
	## see carobiner::geocode
	d3$longitude <- 35.52-42.12
	
	d3$latitude <-  6.58-11.77
	##### Crop #####
	## normalize variety names
	## see carobiner::fix_name
	d3$crop <- "maize"
	#variety
	d3$variety<-d3$`Name of variety`
	##Fertiliser used
	d3$OM_used<-d3$`Fertilizer type/inorganic`
	d3$soil_pH<-d3$pH
	d3$soil_K<-d3$`K  (mg/kg)`
	d3$soil_Mg<-d3$`Mg  (mg/kg)`
	d3$soil_Ca<-d3$`Ca  (mg/kg)`
	###YIELD
	d3$yield <- d3$`Quadrant(1)-Grain yield kg/ha`+d3$`Quadrant(2)-Grain yield kg/ha`+d3$`Quadrant(3)-Grain yield kg/ha`
	
	r4<-TAMASA_ET_CC_2016F
	d4<-r4
	d4$crop <- "maize"
	d4$on_farm <- FALSE
	d4$is_survey <- TRUE
	d4$is_experiment <- FALSE
	d4$irrigated <- FALSE
	
	d4$adm1 <- d4$Zone
	d4$adm2 <- d4$Districts
	d4$adm3 <- d4$Kebele
	d4$adm4 <- d4$Community
	#plant details
	d4$row_spacing<-d4$`Row Distance (cm)`
	d4$plant_spacing<-d4$`Plant Distance (cm)`
	#location
	
	d4$longitude <- 36.02-37.53
	d4$latitude <-  7.40-10.41
	#yield
	d4$yield <- d4$`Grain yield (kg/ha)`
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)

