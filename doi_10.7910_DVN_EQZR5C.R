# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"International Maize and Wheat Improvement Center (CIMMYT); 
Zambian Agriculture Research Institute (ZARI), 2021,
Pigeonpea Ratooning Trial Under Conservation Agriculture, 2020

   
"
#### Identifiers
	uri <- "doi:10.7910/DVN/EQZR5C"
	group <- "conservation_agriculture"

# the script filename should be paste0(dataset_id, ".R")
	dataset_id <- carobiner::simple_uri(uri)

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)

##### dataset level metadata 
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group="conservation_agriculture", dataset_id=dataset_id),
		data_citation="Pigeonpea Ratooning Trial Under Conservation Agriculture, 2020, https://doi.org/10.7910/DVN/EQZR5C, Harvard Dataverse, V1, UNF:6:OAYMpK4cYPCnzPEmfE+rNQ== [fileUNF]",
		data_institutions = "CIMMYT",
		publication= NA,
		project=NA,
		data_type= "on-farm experiment",
		carob_contributor= "Shumirai Manzvera",
		carob_date="2024-02-29"
	)
	
##### PROCESS data records

# read data 

	f <- ff[basename(ff) == "AR_ZAM_CIMMYT_ratooning_onstation_2020.csv"]
	r <- read.csv(f)
	
## use a subset
	d <- data.frame(
	  crop=r$Crop, 
		treatment=r$Intercropstrategy,
		rep=r$Rep,
		dmy_residue=r$biomass,
		yield=r$grainyield,
		country = r$Country,
		site =r$District,
		adm1 =r$Location)

	d$crop<-carobiner::replace_values(d$crop,"pigeonpea","pigeon pea")
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- FALSE

## each site must have corresponding longitude and latitude
	d$longitude <- 32.6447
	d$latitude <- -13.64451


##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- as.character(as.Date(2019-01-12   ))
	d$harvest_date  <- as.character(as.Date( 2020-01-09   ))

##### Fertilizers  #####
	d$N_fertilizer <- 10/40 *100 + 46
	d$P_fertilizer <-20/40 *100
	

	#what plant part does yield refer to?
	d$yield_part <- "grain"
	
	d$trial_id<-paste0(d$dataset_id,"_",d$rep)
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

