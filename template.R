# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:

    [copy the abstract from the repo]

"

	uri <- "doi:______"
	dataset_id <- carobiner::simple_uri(uri)
	group <- ""
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   project=NA,
	   uri=uri,
	   data_citation="",
	   ## if there is a paper, include the paper's doi here
	   ## also add a RIS file in references folder (with matching doi)
	   publication= "",
	   data_institutions = "",
	   carob_contributor="Your name",
	   
	   has_weather=FALSE,
	   has_soil=FALSE,
	   has_management=FALSE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)


	f <- ff[basename(ff) == "_____________"]

	r <- read.csv(f)
	r <- readxl::read_excel(f) |> as.data.frame()

	
## process file(s)

## use a subset
	d <- carobiner::change_names(r, from, to)

	
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
	d$start_date <- as.character(as.Date(   ))
	d$end_date  <- as.character(as.Date(    ))

##### Fertilizers #####
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- 
   d$K_fertilizer <-
   d$N_fertilizer <- 
## normalize names 
   d$fertlizer_type <- 
   d$inoculated <- TRUE/FALSE
   d$inoculant <- 
   
##### in general, add comments to your script if computations are
##### based in information gleaned from metadata, publication, 
##### or not immediately obvious for other reasons

##### Yield #####

	d$yield <- 
	d$biomass_total <- 
	
# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)
}

## now test your function in a clean environment 
# path <- _____
# carob_script(path)

