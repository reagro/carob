# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

These are the raw data of the paper: 'Mulch application as the overarching factor explaining increase in soil organic carbon stocks under conservation agriculture in two 8-year-old experiments in Zimbabwe.' authored by Armwell Shumba, Regis Chikowo, Christian Thierfelder, Marc Corbeels, Johan Six, Rémi Cardinael and submitted for publication in a peer-reviewed journal.]

"

	uri <- "doi:10.18167/DVN1/VPOCHN"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "conservation_agriculture"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Shumba, Armwell; Chikowo, Regis; Thierfelder, Christian; Corbeels, Marc; Six, Johan; Cardinael, Rémi, 2023, Data for Mulch application as the overarching factor explaining increase in soil organic carbon stocks under conservation agriculture in two 8-year-old experiments in Zimbabwe, https://doi.org/10.18167/DVN1 /VPOCHN , CIRAD Dataverse, V2",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= NA,
		data_institutions = "CIRAD; University of ZIMBABWE; CIMMYT; ETH ZURICH",
		# e.g. "on-farm experiment", "survey", "compilation"
   		data_type="experiment", 
		carob_contributor="Hope Takudzwa Mazungunye",
		# date of first submission to carob
		carob_date="2024-01-25",
		# name(s) of others who made significant improvements
		revised_by=""
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=0)
	dset$license <- carobiner::get_license(js)[1]


	f <- ff[basename(ff) == "Shumba_et_al_Raw_data_SOC_paper_vf.xlsx"]

	#r <- read.csv(f)
	r <- readxl::read_excel(f, sheet = "Soil_profile_BD_1m") |> as.data.frame()
	r1 <- readxl::read_excel(f, sheet = "SOC_conc_stocks") |> as.data.frame()
	r2 <- readxl::read_excel(f, sheet = "SOC_change_accum_rates") |> as.data.frame()
	r3 <- readxl::read_excel(f, sheet = "Seasonal_OC_inputs") |> as.data.frame()
#r4 <- carobiner::bindr(r, r1, r2, R3)
	
## process file(s)

## use a subset
	d <- carobiner::change_names(r, from, to)
d <- r
d1 <-r1
d2<- r2
d3 <- r3
	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- FALSE
	  
	  d1$dataset_id <- dataset_id
	d1$on_farm <- TRUE
	  d1$is_survey <- FALSE
	  d1$is_experiment <- TRUE
	  d1$irrigated <- FALSE
	  
	  d2$dataset_id <- dataset_id
	d2$on_farm <- TRUE
	  d2$is_survey <- FALSE
	  d2$is_experiment <- TRUE
	  d2$irrigated <- FALSE
	  
	  d3$dataset_id <- dataset_id
	d3$on_farm <- TRUE
	  d3$is_survey <- FALSE
	  d3$is_experiment <- TRUE
	  d3$irrigated <- FALSE
## the treatment code	
	d$treatment <-  d$Treatment
	  d1$treatment <-d1$Treatment
	  d2$treatment <-d2$Treatment
	  d3$treatment <- d3$Treatment

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "ZIMBABWE"
	d$site <- d$Site
	
	g<-unique(d[,c("country","site")])  
	d$adm3 <- 
	d$elevation <- NA
	  
	  d1$country <- 
	    d1$site <- 
	    d1$adm1 <- 
	    d1$adm2 <- 
	    d1$adm3 <- 
	    d1$elevation <- NA
	  d2$country <- 
	    d2$site <- 
	    d2$adm1 <- 
	    d2$adm2 <- 
	    d2$adm3 <- 
	    d2$elevation <- NA
	  d3$country <- 
	    d3$site <- 
	    d3$adm1 <- 
	    d3$adm2 <- 
	    d3$adm3 <- 
	    d3$elevation <- NA
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

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

