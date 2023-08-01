# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) 

"Description:

    [The present data is based on on-farm demonstration sites set in Malawi to demonstrate the best options available at the moment for the management of drought-tolerant maize varieties and conservation agriculture practices in Balaka, Machinga and Zomba communities. Crop yields in southern Africa are generally low compared to the world average and the average of developing regions. Thus, this calls for the identification of more sustainable strategies that are capable of increasing yields. Amongst the possible strategies is conservation agriculture (CA). This data is a subset of a larger data set from southern Africa that seeks to demonstrate the effects of CA technologies as compared to the traditional farmers' practices. The CA treatments included: 1. Farmers check. Traditional land preparation (ridges) and maize management. Residues may be grazed, removed, burned or incorporated into the ridges. 2. Conservation Agriculture – sole maize. No tillage, no burning. Previous year’s ridges retained (but not reformed). Residue retained (mulch). 3. Conservation Agriculture – maize/legume intercrop. No tillage, no burning. Previous year’s ridges retained (but not reformed). Residue retained (mulch). The data set presents yields for maize and the legumes from these sites over 10 seasons (2005-2015). (2016-12-08)]

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
		data_citation="Thierfelder, Christian",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "Thierfelder, Christian, 2016, Options available for the management of drought-tolerant maize varieties and conservation agriculture practices in Malawi, https:hdl.handle.net/11529/10829, CIMMYT Research Data & Software Repository Network, V1",
		data_institutions = "CIMMYT",
   		data_type="experiment",
		carob_contributor="Blessing"
	)
	

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)
	dset$license <- carobiner::get_license(js)


	f <- ff[basename(ff) == "Summary files Malawi 2005-15..xlsx"]

	r <- read.csv(f)
	r <- readxl::read_excel(f,sheet ="Maize working data") |> as.data.frame()
	r1<- readxl::read_excel(f,sheet ="Legume yields") |> as.data.frame()
	r2<- readxl::read_excel(f,sheet ="Intercropped legume yields") |> as.data.frame()

	
## process file(s)

## use a subset
	d <- carobiner::change_names(r, from, to)

	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- FALSE
## the treatment code	
	r$treatment <- r$Tmnt.
	r1$treatment <- r1$Tmnt.
	r2$treatment <- r2$Tmnt.
	# standardize the crop names
	r$crop <- tolower(r$`crop grown`)
	r1$crop <- tolower(r1$`Crop grown`)
	r2$crop <- tolower(r2$`Crop grown`)
	#standardize the farmer names
	r$farmername <- (r$Farmer)
	r1$farmername <- (r1$`Site name`)
	r2$farmername <- (r2$Farmer)

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "Malawi"
	d$site <- 
	d$adm1 <- 
	d$adm2 <- 
	d$adm3 <- 
	d$elevation <- NA
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <-NA 


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

