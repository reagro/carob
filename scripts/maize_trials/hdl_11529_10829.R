# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [copy the abstract from the repo]

"

	uri <- "hdl:11529/10829"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation='Thierfelder, Christian, 2016, "Options available for the management of drought-tolerant maize varieties and conservation agriculture practices in Malawi", https://hdl.handle.net/11529/10829, CIMMYT Research Data & Software Repository Network, V1',
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication=NA,
		data_institutions = "CIMMYT",
   		data_type="on-farm experiment",
		carob_contributor="Mitchelle Njukuya" 
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)
	dset$license <- carobiner::get_license(js)


	f <- ff[basename(ff) == "Summary files Malawi 2005-15..xlsx"]

	
	r <- readxl::read_excel(f,sheet="Maize working data") |> as.data.frame()
	r1 <- readxl::read_excel(f,sheet="Legume yields") |> as.data.frame()
	r2 <- readxl::read_excel(f,sheet="Intercropped legume yields") |> as.data.frame()

	
## process file(s)

## use a subset
	d <- r
	d$country <- "Malawi"
	d$adm1 <- d$District
	d$adm2 <- d$Village
	d$intercrop <- NA
	d$intercrop[d$`sole/intercrop`==1 & d$treatment %in% c("CA +Maize/Pp", "CA+maize/Pp", "Maizepp")] <- "pigeon pea"
	d$intercrop[d$`sole/intercrop`==1 & d$treatment %in% c("CA +Maize/mucuna")] <- "velvet bean"
	d$intercrop[d$`sole/intercrop`==1 & d$treatment %in% c("CA+Maize/Cp")] <- "cowpea"
	d$yield <- d$`Grain yield (kg/ha)`
	d$crop <-d$`crop grown`
	d<-d[,c("dataset_id","country","adm1","adm2","treatment","Variety","crop","intercrop","on_farm","irrigated","is_survey","yield")]
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE 
	d$is_survey <- FALSE
	d$is_experiment <- TRUE 
	d$irrigated <- FALSE
## the treatment code	
	d$treatment <- d$Tmnt
	

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
   d$inoculated <- TRUE/FALSE
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

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)

