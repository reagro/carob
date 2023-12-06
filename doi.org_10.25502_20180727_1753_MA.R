# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

   This is an international study that contains data on yield and other Agronomic traits of maize including borer and striga attacks on maize in Africa.

The study was carried out by the International Institute of Tropical Agriculture between 1989 and 2015 in over thirty African countries.

This dataset contains output of the research for Ethopia.

"

	uri <- "doi.org/10.25502/20180727/1753/MA"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "conservation_agriculture"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="https://doi.org/10.25502/20180727/1753/MA",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "NA",
		data_institutions = "IITA",
   		data_type="on-farm experiment", 
		carob_contributor="Shumirai Manzvera",
		# date of first submission to carob
		carob_date="2023-11-30",
		# name(s) of others who made significant improvements
		revised_by="NA"
	)

## download and read data 
	path <- "C:/carob"
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)


	
	
## process file(s)

## use a subset
	d <- international_maize_trial_regular_ethiopia_csv
	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- FALSE
## the treatment code	
	d$treatment <- d$TRL_TITL

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- d$COUNTRY
	d$site <- d$LOCATION
	d$elevation <- NA
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- d$LONGITUDE
	d$latitude <- d$LATITUDE

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "maize"
	d$variety <- d$ENTRY

   

##### Yield #####
	
	d$yield <- d$YIELD
	d$erot <- d$E_ROT
	d$pl_ht <- d$PL_HT
	d$sl <- d$SL
	d$rl <- d$RL
	d$p_asp <- d$P_ASP
	d$e_asp <- d$E_ASP
	d$e_ht <- d$E_HT
	d$dy_sk <- d$DY_SK
	d$asi <- d$ANTHESIS
	d$blight <- d$BLIGHT
	d$borer <- d$SBDAMAT
	d$borer_dam_rat <- d$BORERDMRAT
	d$cob_dam_co <- d$COBDAMCO
	d$cob_dam_rt <- d$COBDAMRT
	d$curv <- d$CURV
	d$dead_heart <- d$DEADHEART
	d$diplodia <- d$DIPLODIA
	d$dm <- d$DM
	d$gls <- d$GLS
	d$gtext <- d$GTEXT
	d$gwt <- d$GWT
	d$husk <- d$HUSK
	d$insect <- d$INSECT
	d$moist <- d$MOIST
	d$rust <- d$RUST
	d$slper <- d$SLPER
	d$rlper <- d$RLPER
	d$pl_st <- d$PLST
	
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

