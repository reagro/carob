# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) { 

Description:

     "The objective of this work set is to demonstrate the best options currently available for the management of conservation agriculture (CA) practices in different communities in Mozambique. Eleven communities were selected from the districts of Sofala, Tete and Manica (approximately 100 to 200 families in each community) to host these demonstration sites and six demo fields were installed in each community from 2006-2015 (9 seasons). The treatments in each community were as follows: 1. Farmers' practice (control)- Traditional management with removal of stubble. 2. Conservation agriculture- The stubble is kept in the ground, there is no preparation of the ground, and the sowing is done manually in covachos previamento open (see the management of the covachos) and with SULCADOR in Nhamatiquite. 3. Direct sowing (SD): The stubble is kept in the soil, the direct sowing is done with Matraca or sharp bread. (2016-12-08)"

	uri <- "hdl:11529/10830"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"
	path <- ("C:/Users/user/Documents/MY WORKING DIRECTORY/carob")
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Thierfelder, Christian, 2016, Increase in sustainable agricultural production in Mozambique through drought tolerant maize and conservation agriculture, https://hdl.handle.net/11529/10830, CIMMYT Research Data & Software Repository Network, V1",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "Increase in sustainable agricultural production in Mozambique through drought tolerant maize and conservation agriculture",
		data_institutions = "CIMMYT",
   		data_type="on-farm experiment",
		carob_contributor="Blessing Dzuda"  
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)
	dset$license <- carobiner::get_license(js)
	
	f <- "C:/Users/user/Documents/MY WORKING DIRECTORY/carob/data/raw/maize_trials/hdl_11529_10830"
 
	library(readxl)
	Summary_Mozambique_On_farm_Demonstration_2006_2015 <- read_excel("data/raw/maize_trials/hdl_11529_10830/Summary Mozambique On-farm Demonstration 2006-2015.xlsx")
	

	
## process file(s)

## use a subset
	b <- Summary_Mozambique_On_farm_Demonstration_2006_2015

	
#### about the data #####
## (TRUE/FALSE)

	b$dataset_id <- dataset_id
	b$on_farm <- TRUE
	b$is_survey <- FALSE
	b$is_experiment <- TRUE
	b$irrigated <- FALSE
## the treatment code	
	b$tilage <- b$Tmnt.
	b$harvest_date <- b$`Harvest Year`
	b$adm2 <- b$District
	b$adm3 <- b$Village
	b$adm4 <- b$`District No.`
	b$rep <- b$`Cummulative rep`
	b$crop <- b$`Crop grown`
	b$variety <- b$Variety
	b$plant_popilation <- b$`Final stand (pl/ha)`
	b$residue_yield <- b$`Stalk yield (kg/ha)`
	b$yield <- b$`Grain yield (kg/ha)`

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	b$country <- "Mozambique"

## each site must have corresponding longitude and latitude
## see carobiner::geocode
	b$longitude[b$Village=="Pumbuto"] <- 33.7492
	b$longitude[b$Village=="Nhanguo"] <- 34.483
	b$longitude[b$Village=="Puanda"] <- 40.708
	b$longitude[b$Village=="Guro"] <- 33.5690
	b$longitude[b$Village=="Malomue"] <- 35.2368
	b$longitude[b$Village=="Ruaca"] <- 36.353
	b$longitude[b$Village=="Nhamizhinga"] <- 33.929
	b$longitude[b$Village=="Nzewe"] <- 29.7251
	b$longitude[b$Village=="Nhamatiquite"] <- 33.26
	b$longitude[b$Village=="Maguai"] <- 33.1472
	b$longitude[b$Village=="Madjiga"] <- 33.1408
	b$longitude[b$Village=="Lamego"] <- 34.2582
	b$longitude[b$Village=="Belia"] <- 34.8370
	b$longitude[b$Village=="Gimu"] <- 33.2807
	b$longitude[b$Village=="Ulongue"] <- 34.3588
	b$longitude[b$Village=="Nharuchonga"] <- 34.123
	b$longitude[b$Village=="Mussianharo"] <- 33.258
	b$longitude[b$Village=="Malomwe"] <- 34.917
	b$longitude[b$Village=="Lamego John Segredo"] <- 34.3571
	b$longitude[b$Village=="Lamego Ngeja"] <- 34.5271
	
	  
	b$latitude[b$Village=="Pumbuto"] <- -19.0056
	b$latitude[b$Village=="Nhanguo"] <- -19.917
	b$latitude[b$Village=="Puanda"] <- 14.752
	b$latitude[b$Village=="Guro"] <- -16.9640
	b$latitude[b$Village=="Malomue"] <- 14.6436
	b$latitude[b$Village=="Ruaca"] <- -14.916
	b$latitude[b$Village=="Nhamizhinga"] <- -21.418
	b$latitude[b$Village=="Nzewe"] <- -3.0515
	b$latitude[b$Village=="Nhamatiquite"] <- 19.36
	b$latitude[b$Village=="Maguai"] <- -21.5263
	b$latitude[b$Village=="Madjiga"] <- -20.5535
	b$latitude[b$Village=="Lamego"] <- -19.3188
	b$latitude[b$Village=="Belia"] <- -19.8316
	b$latitude[b$Village=="Gimu"] <- -19.0595
	b$latitude[b$Village=="Ulongue"] <- 14.7080
	b$latitude[b$Village=="Nharuchonga"] <- -19.240
	b$latitude[b$Village=="Mussianharo"] <- -18.0438
	b$latitude[b$Village=="Malomwe"] <- 14.7258
	b$latitude[b$Village=="Lamego John Segredo"] <- -19.356
	b$latitude[b$Village=="Lamego Ndeja"] <- -193721

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	b$crop <- "Maize"


##### Fertilizers #####
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   b$P_fertilizer <- NA 
   b$K_fertilizer <-NA
   b$N_fertilizer <- NA
   b$S_fertilizer <- NA
   b$lime <- NA
## normalize names 
   b$fertlizer_type <-NA 
   b$inoculated <- FALSE
   b$inoculant <- FALSE
   
##### in general, add comments to your script if computations are
##### based on information gleaned from metadata, a publication, 
##### or when they are not immediately obvious for other reasons

##### Yield #####

	#what plant part does yield refer to?
	b$yield_part <- "grain"
   
  ###Extracting columns from dataset
   b <- b[, c("country","adm2","adm3","adm4","latitude","longitude","crop","tilage","harvest_date","variety","plant_popilation","yield_part","residue_yield","yield")]
	
# all scripts must end like this
	carobiner::write_files(dset, b, path=path)
}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)

