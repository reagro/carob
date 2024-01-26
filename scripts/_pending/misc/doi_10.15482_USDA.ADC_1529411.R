# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    This dataset consists of growth and yield data for each season when sorghum [Sorghum bicolor (L.)] was grown at the USDA-ARS Conservation and Production Laboratory (CPRL), Soil and Water Management Research Unit (SWMRU) research weather station, Bushland, Texas 

"

	uri <- "doi:10.15482/USDA.ADC/1529411"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "conservation_agriculture"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="https://doi.org/10.13031/2013.21321",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "",
		data_institutions = "USDA-ARS ",
   		data_type="experiment", 
		carob_contributor="Shumirai Manzvera",
		# date of first submission to carob
		carob_date="2023-10-06" 
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)


	f <- ff[basename(ff) == "_____________"]

	r <- read.csv(f)
	r <- readxl::read_excel(f) |> as.data.frame()

	
## process file(s)

## use a subset
	d <- carobiner::change_names(r, from, to)
  d<-X2015_W_Sorghum_Growth_and_Yield_V3
	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <-FALSE 
	d$is_experiment <- TRUE
	d$irrigated <- TRUE
## the treatment code	
	d$treatment <- 

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "United States"
	d$site <-"texas" 
	d$adm1 <- 
	d$adm2 <- 
	d$adm3 <- 
	d$elevation <- 1170 
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- -102.094189
	d$latitude <- 35.186714

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "sorghum" 
	d$variety <- 



	d$yield <- d$`Dry grain yield in kg/ha`
	#what plant part does yield refer to?
	 
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

