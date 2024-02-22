# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [copy the abstract from the repo]

"
#### Identifiers
	uri <- "doi:10.7910/DVN/1T4Q3F"
	group <- "conservation_agriculture"
	dataset_id <- carobiner::simple_uri(uri)

# the script filename should be paste0(dataset_id, ".R")


#### Download data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)

##### dataset level metadata 
	dset <- extract_metadata(js, uri, group)
	dset$project=NA
	dset$data_citation=""
	## if there is a paper, include the paper's doi here
	## also add a RIS file in references folder (with matching doi)
	dset$publication= ""
	dset$data_institutions = ""
	# data_type can be e.g. "on-farm experiment", "survey", "compilation"
	dset$data_type= "experiment"
	dset$carob_contributor= "Your Name"
	dset$carob_date="2024-01-01"
	
	
##### PROCESS data records

# read data 

	f <- ff[basename(ff) == "Linthipe_Dedza_MaizeResopnsetoGroundnut_2019_2020.csv"]
	f1 <-	ff[basename(ff) == "Mtubwi_Machinga_MaizeResopnsetoGroundnut_2019_2020.csv"]
	r <- read.csv(f)
	r1 <- read.csv(f1)

## process file(s)

## use a subset
	d <- data.frame(adm2=r$District,adm3=r$EPA, rep=r$REPLICATION,variety_code=r$maize.variety.planted.in.2019.2020.season,
	                previous_crop=r$TREATMENT.NAME..groundnut.planted.in.previuos.season.,crop_rotation=r$TREATMENT.NAME..groundnut.planted.in.previuos.season.,
	                dmy_total=r$Total.biomass..kg.ha.,yield=r$Grain.yld.ha,e_ht=r$Ear.length..cm.,treatment=r$TREATMENT.No.,longitude=7,latitude=10)
					

	d1 <- data.frame(adm2=r1$District,adm3=r1$EPA, rep=r1$REPLICATION,variety_code=r1$maize.variety.2019.2020.season,
	                previous_crop=r1$PREVIOUS.TREATMENT..2018.2019.season.,crop_rotation=r1$PREVIOUS.TREATMENT..2018.2019.season.,
	                dmy_total=r1$Total.biomass..kg.ha.,yield=r1$Grain.yld..kg.ha.,e_ht=r1$Ear.length..cm.,treatment=r1$TREATMENT.NO.)
	
	#location 
	
	
	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- 
	d$is_survey <- 
	d$is_experiment <- 
	d$irrigated <- 
## the treatment code	
	d$treatment <- 

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- 
	d$site <- 
	d$adm1 <- 
	d$adm2 <- 
	d$adm3 <- 
	d$elevation <- NA
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- 
	d$latitude <- 

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- 
	d$variety <- 

##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- as.character(as.Date(   ))
	d$harvest_date  <- as.character(as.Date(    ))

##### Fertilizers #####
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- 
   d$K_fertilizer <-
   d$N_fertilizer <- 
   d$S_fertilizer <- 
   d$lime <- 
## normalize names 
   d$fertlizer_type <- 
   
   d$inoculated <- TRUE or FALSE
   d$inoculant <- 
   
##### in general, add comments to your script if computations are
##### based on information gleaned from metadata, a publication, 
##### or when they are not immediately obvious for other reasons

##### Yield #####
	d$biomass_total <- 

	d$yield <- 
	#what plant part does yield refer to?
	d$yield_part <- 
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

