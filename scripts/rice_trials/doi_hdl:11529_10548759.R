# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [copy the abstract from the repo]

"

	uri <- "hdl:11529/10548759"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "rice_trials"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Jat, Hanuman S.; Choudhary, Madhu; Datta, Ashim; Kakraliya, Suresh K.; McDonald, Andrew J.; Jat, ML; Sharma, Parbodh C., 2022, Long-term conservation agriculture helps in the reclamation of sodic soils in major agri-food systems, https://hdl.handle.net/11529/10548759, CIMMYT Research Data & Software Repository Network, V1",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "DOI: 10.1002/ldr.4321",
		data_institutions = "CIMMYT",
   		data_type="on-farm experiment", # or, e.g. "on-farm experiment", "survey", "compilation"
		carob_contributor="Hope Takudzwa Mazungunye"  
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)
	dset$license <- carobiner::get_license(js)[[1]]

## just take the name of the file and file format
	f <- ff[basename(ff) == "Jat et al 2022 Final row data for LDD_SK.xlsx"]
	r <- readxl::read_excel(f, sheet="ESP") |> as.data.frame()

	
## process file(s)

## use a subset
	d <- r[4:15, 1:8]

	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- FALSE
## the treatment code	
	d$treatment[d$...1 == "Sc1"] <- "Conventional rice - wheat system"
	d$treatment[d$...1== "Sc2"]<- "partial CSA based rice-wheat-mungbean system"
	d$treatment[d$...1== "Sc3"]<- "full CSA based rice-wheat-mungbean system"
	d$treatment[d$...1== "Sc4"]<- "full CSA based maize-wheat-mungbean system"
##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- INDIA
	d$site <- ICAR-CSSRI
	d$adm1 <- NA
	d$adm2 <- NA
	d$adm3 <- NA
	d$elevation <- 243
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- 76.95527778
	d$latitude <- 29.70555556 

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- d$crop[d$...1 == "Sc1"] <- " rice - wheat" 
	d$variety <- NA

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

