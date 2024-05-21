# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
copy and paste the abstract from the repository. Do not add line breaks
"

#### Identifiers
	uri <- "doi:10xxx/yyy"
	group <- "___"

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)

##### dataset level metadata 
	dset <- data.frame(
		# change the major and minor versions if you see a warning
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institute = "",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication = "",
		project = NA,
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		data_type = "experiment",
		# treatment_vars has semi-colon separated variable names that represent the
		# treatments if the data is from an experiment. E.g. "N_fertilizer;P_fertilizer;K_fertilizer"
		treatment_vars = NA, 
		carob_contributor = "Your Name",
		carob_date = "2024-01-01"
	)
	
##### PROCESS data records

# read data 

	f <- ff[basename(ff) == "_____________"]
	r <- read.csv(f)
	# or  r <- carobiner::read.excel(f)

## process file(s)

## select the variables of interest and assign them to the correct name
	d <- data.frame(
		crop=r$crop, 
		latitude=r$lat,
		longitude=r$lon,
		yield = r$yield_tonha * 1000
		# etc
	)

	
#### about the data #####
## (TRUE/FALSE)
	d$on_farm <- 
	d$is_survey <- 
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
	d$yield <- 
	#what plant part does yield refer to?
	d$yield_part <- 
	
# all scripts must end like this
	carobiner::write_files(path, dset, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

