# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description: TAMASA Agronomy Panel Survey 2016/17 Season. This file contains the maize grain yield from approximately 578 maize fields in the Southern Highlands, Northern and Eastern Zones of Tanzania in collected May-August 2017. Maize grain yield data can be linked to associated maize yield and soil by the common HHID. (2018-04-15)


"

	uri <- "hdl:11529/10548242"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "conservation_agriculture"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Craufurd, Peter; Karwani, George; Masuki, Kenneth, 2019, TAMASA TZ APS 2017 CC MaizeYield v3, https://hdl.handle.net/11529/10548242, CIMMYT Research Data & Software Repository Network, V2, UNF:6:FARtQ7xWh1m0+YYceI+wnw== [fileUNF]",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "Masuki, K., Karwani, G., Mushongi, A., Lameck, M., Chamberlin, J., Craufurd, P., Nord, A., Mwaijandi, V., Wineman, A. 2018. TAMASA Tanzania Agronomic Panel Survey for 2017. Maize grain yield data. These data were collected as part of the BMGF supported TAMASA (Taking Maize Agronomy to Scale in Africa) project, 2014-2018.",
		data_institutions = "CIMMYT",
   		data_type="survey" ,
		carob_contributor="Mitchelle Njukuya",
		carob_date="2023-08-31"
	)

## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)


	f <- "C:/Users/user/Documents/DataAnalysis/carob-TanzaniaDataset/data/raw/maize_trials/hdl_11529_10548242/TAMASA_TZ_APS_2017_CC_MaizeYield.xlsx"
	library("readxl")
	r <- read_excel(f)
	r <- readxl::read_excel(f,sheet="Raw data") |> as.data.frame()

	
## process file(s)

	d <- r

	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$is_experiment <- FALSE
	d$irrigated <- FALSE
## the treatment code	
	d$treatment <- d$QID

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- d$Country
	d$site <- d$Zone
	d$adm1 <- d$Region
	d$adm2 <- d$District
	d$adm3 <- d$Ward
	d$elevation <- d$Altitude
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- d$Longitude
	d$latitude <- d$Latitude

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "maize"
	d$variety <- NA

##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- NA
	d$harvest_date  <- NA

##### Fertilizers #####
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- NA
   d$K_fertilizer <- NA
   d$N_fertilizer <- NA
   d$S_fertilizer <- NA 
   d$lime <- NA
## normalize names 
   d$fertlizer_type <- NA
   d$inoculated <- FALSE
   d$inoculant <- NA
   
##### in general, add comments to your script if computations are
##### based on information gleaned from metadata, a publication, 
##### or when they are not immediately obvious for other reasons

##### Yield #####
	d$biomass_total <- NA

	d$yield <- d$`Grain yield (kg/ha@12.5%)`
	#what plant part does yield refer to?
	d$yield_part <- "grain" 
	
	d <- d[,c( "dataset_id","is_survey","irrigated","is_experiment","on_farm","country","site","adm1","adm2","adm3","longitude", "latitude","elevation","crop","treatment","yield_part","yield")]
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)

