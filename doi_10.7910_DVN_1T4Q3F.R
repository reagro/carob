# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"This dataset is generated from maize response trials (legume/maize rotation system) that were conducted in Linthipe EPA, Dedza district, and Ntubwi EPA in Machinga district in Malawi, 2019/2020 cropping season. In the previous season, certifies and recycled seed of different varieties of groundnut were grown. In the current season, maize was planted as a test crop.
"
#### Identifiers
	uri <- "doi:10.7910/DVN/1T4Q3F"
	group <- "conservation_agriculture"
	dataset_id <- carobiner::simple_uri(uri)

# the script filename should be paste0(dataset_id, ".R")


#### Download data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)

##### dataset level metadata 
	dset <- data.frame(
	  carobiner::extract_metadata(js, uri, group="conservation_agriculture", dataset_id=dataset_id),
	  data_citation="Lilongwe University of Agriculture and Natural Resources, 2021, Performance of Maize Following Groundnut Varieties at Different Densities, https://doi.org/10.7910/DVN/1T4Q3F, Harvard Dataverse, V1, UNF:6:oLZEjX4cmNU+x2ElSIOljA== [fileUNF]",
	  data_institutions = "International Food Policy Research Institute (IFPRI)",
	  ## if there is a paper, include the paper's doi here
	  ## also add a RIS file in references folder (with matching doi)
	  publication= NA,
	  # data_type can be e.g. "on-farm experiment", "survey", "compilation"
	  project=NA,
	  data_type= "experiment",
	  carob_contributor= "Shumirai Manzvera",
	  carob_date="2024-02-22"
	)
	
	
##### PROCESS data records

# read data 

	f <- ff[basename(ff) == "Linthipe_Dedza_MaizeResopnsetoGroundnut_2019_2020.csv"]
	f1 <-	ff[basename(ff) == "Mtubwi_Machinga_MaizeResopnsetoGroundnut_2019_2020.csv"]
	r <- read.csv(f)
	r1 <- read.csv(f1)

## process file(s)

## use a subset
	d0 <- data.frame(adm2=r$District,adm3=r$EPA, rep=r$REPLICATION,variety_code=r$maize.variety.planted.in.2019.2020.season,
	                previous_crop="groundnut",crop_rotation="groundnut",
	                dmy_total=r$Total.biomass..kg.ha.,yield=r$Grain.yld.ha,treatment=r$TREATMENT.No.,longitude=34.1253751,latitude=-14.1832077,crop="maize")
					

	d1 <- data.frame(adm2=r1$District,adm3=r1$EPA, rep=r1$REPLICATION,variety_code=r1$maize.variety.2019.2020.season,
	                previous_crop="groundnut",crop_rotation="groundnut",
	                dmy_total=r1$Total.biomass..kg.ha.,yield=r1$Grain.yld..kg.ha.,treatment=r1$TREATMENT.NO.,longitude =35.5737,latitude = -14.9458,crop="maize")
	
	
	d <- carobiner::bindr(d0,d1)
	
#### about the data #####
## (TRUE/FALSE)
  d$country<- "Malawi"
	d$dataset_id <- dataset_id
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- FALSE




##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- as.character(as.Date(2019   ))
	d$harvest_date  <- as.character(as.Date(  2020  ))

 
	#what plant part does yield refer to?
	d$yield_part <- "grain"
	d$trial_id<-paste0(d$dataset_id,"_",d$rep)
	d$dmy_total<-as.numeric(d$dmy_total)
	d$treatment<-as.character(d$treatment)
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}



