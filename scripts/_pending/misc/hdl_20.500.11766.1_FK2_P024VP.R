# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

   Final dataset from agronomic experiment in Gumara Maksegnit (2016), as elaborated by GARC researcher in charge for this trial. Please contact author and contact person at ICARDA to obtain more detailed metadata or to propose collaboration.

"

	uri <- "hdl:20.500.11766.1/FK2/P024VP"
	group <- "conservation group"
	ff <- carobiner::get_data(uri, path, group)
	## dataset level data 
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=2, minor=0),
		project=NA,
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "",
		data_institutions = "ICARDA",
   		data_type="on-farm experiment", 
		carob_contributor="Shumirfai Manzvera",
		# date of first submission to carob
		carob_date="2023-10-31" 
	)



	f <- ff[basename(ff) == "Experimental_Data.xlsx"]
	r <- read.csv(f)
	r <- readxl::read_excel(f,sheet = "ExperimentalData") |> as.data.frame()
	
## process file(s)

## use a subset
	d <- Experimental_Data

	
#### about the data #####
## (TRUE/FALSE)

	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- FALSE
## the treatment code	
	d$treatment <- d$Treatment

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "Ethiopia"
	d$site <-  "Gonder"
	d$adm1 <- 
	d$adm2 <- 
	d$adm3 <- 
	d$elevation <- NA
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- 37.466667
	d$latitude <- 12.6

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "sorghum"
	

  
##### Yield #####

	d$yield <- d$Yield
	#replication
	d$rep<-d$Replicate
	#plant height
	d$plant_height<-d$Plant_Height
	#residual yield
	d$residue_yield<-d$Stover
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

