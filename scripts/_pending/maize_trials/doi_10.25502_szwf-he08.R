# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    Yield gains and associated changes in an early yellow bi-parental maize population following Genomic Selection for Striga resistance and drought tolerance.

"

	uri <- "doi.org/10.25502/szwf-he08"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project="CGIAR Research Program on Maize",
		uri=uri,
		data_citation="Badu-Apraku Baffour, R. Asiedu, A.O. Talabi, M.A.B. Fakorede, Y. Fasanmade, M. Gedil, & C. Magorokosho. (2018). Yield gains and associated changes in an early yellow bi-parental maize population following Genomic Selection for Striga resistance and drought tolerance [dataset]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/SZWF-HE08",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "https://doi.org/10.3390/plants8110518",
		data_institutions = "IITA",
		# e.g. "on-farm experiment", "survey", "compilation"
   	data_type="experiment", 
		carob_contributor="Eduardo Garcia Bendito",
		# date of first submission to carob
		carob_date="2023-10-25" 
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)


	f1 <- ff[basename(ff) == "EARLY Drought.csv"]
	f2 <- ff[basename(ff) == "Early Drought+Heat.csv"]
	f3 <- ff[basename(ff) == "Early Heat.csv"]
	f4 <- ff[basename(ff) == "Extra-Early Drought.csv"]
	f5 <- ff[basename(ff) == "Extra-Early Drought+Heat.csv"]
	f6 <- ff[basename(ff) == "Extra-Early Heat.csv"]
	
	# View(read.csv(ff[basename(ff) == "Metadata.csv"]))

	r1 <- read.csv(f1)
	colnames(r1) <- tolower(colnames(r1))
	r1$variety_type <- "early"
	r1$exp_type <- "drought"
	r2 <- read.csv(f2)
	colnames(r2) <- tolower(colnames(r2))
	r2$variety_type <- "early"
	colnames(r2)[c(16,17,18)] <- c("rl", "sl", "hc")
	r2$exp_type <- "drought-heat"
	r3 <- read.csv(f3)
	colnames(r3) <- tolower(colnames(r3))
	r3$variety_type <- "early"
	r3$exp_type <- "heat"
	r4 <- read.csv(f4)
	colnames(r4) <- tolower(colnames(r4))
	r4$variety_type <- "extra-early"
	r4$exp_type <- "drought"
	r5 <- read.csv(f5)
	colnames(r5) <- tolower(colnames(r5))
	r5$variety_type <- "extra-early"
	colnames(r5)[c(16,17,18)] <- c("rl", "sl", "hc")
	r5$exp_type <- "drought-heat"
	r6 <- read.csv(f6)
	colnames(r6) <- tolower(colnames(r6))
	r6$variety_type <- "extra-early"
	colnames(r6)[c(16,17)] <- c("rl", "sl")
	r6$exp_type <- "heat"
	r <- carobiner::bindr(r1,r2,r3,r4,r5,r6)
	r$lfdth.1 <- NULL

	
## process file(s)

## use a subset
	d <- carobiner::change_names(r, c("pedigree", "dysk", "plht"), c("variety", "silking", "plant_height"))

	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$trial_id <- d$loc
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- ifelse(d$exp_type == "heat", TRUE, FALSE)
## the treatment code	
	d$treatment <- d$exp_type

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "Nigeria"
	d$site <- ifelse(d$loc == "IKDS", "Ikenne", "Kadawa")
	d$adm1 <- ifelse(d$loc == "IKDS", "Ogun State", "Kano State")
	d$elevation <- ifelse(d$loc == "IKDS", 60, 500)
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- ifelse(d$loc == "IKDS", 3.700, 8.450)
	d$latitude <- ifelse(d$loc == "IKDS", 6.883, 11.650)

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "maize"
	d$variety <- d$variety

##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- as.character(as.Date(paste0(d$year, "-02-15"), format = "%Y-%m-%d"))

##### Fertilizers #####
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
## RH how sure are we about these zero applications?
#   d$P_fertilizer <- 0
#   d$K_fertilizer <- 0
#   d$N_fertilizer <- 0
#   d$S_fertilizer <- 0
## normalize names 
#   d$fertilizer_type <- "none"

##### in general, add comments to your script if computations are
##### based on information gleaned from metadata, a publication, 
##### or when they are not immediately obvious for other reasons

##### Yield #####

	#what plant part does yield refer to?
	d$yield_part <- "grain"
   
  d$dataset_id <- dataset_id
   
  # Subset final
  d <- d[,c("dataset_id", "country", "trial_id", "on_farm", "is_survey", "treatment", "rep",
            "site", "adm1", "latitude", "longitude", "elevation",
            "crop", "variety", "variety_type",
            "planting_date",
 #           "fertilizer_type", "N_fertilizer", "P_fertilizer", "K_fertilizer",
            "yield", "yield_part",
            "silking", "plant_height")]
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}
