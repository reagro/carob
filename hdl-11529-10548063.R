# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Temperature Wheat Yield Trial (HTWYT) is a replicated yield trial that contains spring bread wheat (Triticum aestivum) germplasm adapted to Mega-environment 1 (ME1) which represents high temperature areas. (2016)

"

	uri <- "hdl:11529/10548063"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "conservation_agriculture"
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
		data_institutions = "CIMMYT",
		
   		data_type="experiment", 
		carob_contributor="Shumirai Manzvera",
		
		carob_date="2023-08-02" 
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
  d<- X15TH_THWYT_RawData_xls
	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- 
## the treatment code	
	d$treatment <- d$

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- d$Country
	d$site <- d$Loc_desc
	d$adm1 <- 
	d$adm2 <- 
	d$adm3 <- 
	d$elevation <- NA
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	#data location
	d1<-X15TH_THWYT_Loc_data_xls
	d1$longitude <- d1$Long_degress
	d1$latitude <-d1$Lat_degress

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "wheat"
	d$variety <- d$Gen_name



##### Yield #####
	 

	d$yield <- d$Value


	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

