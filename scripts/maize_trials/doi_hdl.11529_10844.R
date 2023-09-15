# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [The present data is from a long-term trial set at Msekera Research Station in Zambia to monitor and evaluate the the longer term effects of conservation agriculture practices on soil quality, soil water dynamics, weeds, pests/diseases and crop yield. The treatments set to investigate this are: T1: Control plot 1 (CPM1); traditional farmers practice mouldboard plough on the flat, maize as a sole crop, no residue retention, stubble incorporated into the row for the following season. T2: Control plot 2 (CPM2); ridge and furrow system dug by hand, maize as a sole crop, no residue retention, stubble incorporated into the row for the following season T3: Basins (BAM), residue retention on the surface, maize as a sole crop T4: Dibble stick (DISM), residue retention on the surface, maize as a sole crop T5: Direct seeder (DSM), residue retention on the surface, maize as a sole crop T6: Direct seeding maize/cowpea intercropping (DS-M/C), 90cm rows, residue retention on the surface T7: Direct seeding cowpea (Cowpea-maize rotation) (DS-MC), residue retention on the surface T8:Direct seeding maize (Maize-cowpea rotation) (DS-CM), residue retention on the surface T9:Direct seeding soya (Soybean-maize r otation) (DS-MS), residue retention on the surface T10: Direct seeding maize (Maize-soybean rotation) (DS-SM), residue retention on the surface The present data set is from 2012 to 2016. (2016)]

"

	uri <- "hdl:11529/10844"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Thierfelder, Christian, 2016, Monitoring and evaluation the longer term effects of conservation agriculture practices on soil quality, soil water dynamics, weeds, pests/diseases and crop yield in Eastern Zambia, https://hdl.handle.net/11529/10844, CIMMYT Research Data & Software Repository Network, V2",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "Monitoring and evaluation the longer term effects of conservation agriculture practices on soil quality, soil water dynamics, weeds, pests/diseases and crop yield in Eastern Zambia",
		data_institutions = "CIMMYT",
   		data_type= "on-farm experiment",
		carob_contributor="Blessing Dzuda"  
	)

## download and read data 
	path <- "C:/Users/user/Documents/MY WORKING DIRECTORY/carob"
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)


  library("readxl")
	Msekera_2012_2016 <- read_excel("data/raw/maize_trials/hdl_11529_10844/Msekera 2012.2016.xlsx")
	Msekera_2012_2016 <- read_excel("data/raw/maize_trials/hdl_11529_10844/Msekera 2012.2016.xlsx", 
	                                +     sheet = "All legume yield Msekera")
	
## process file(s)

## use a subset
	c <- 	Msekera_2012_2016
	c1 <- Msekera_2012_2016

	
#### about the data #####
## (TRUE/FALSE)

	c$dataset_id <- dataset_id
	c$on_farm <- TRUE
	c$is_survey <- FALSE
	c$is_experiment <- TRUE 
	c$irrigated <- FALSE
## the treatment code	
	c$treatment <- c$Treatment
	c$harvest_year <- c$Year
	c$crop <- c$Crop
  c$rep <- c$Replicate
  c$biomass_total <- c$Biomass 
  c$yield <- c$Grain
##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	c$country <- "Zambia"
	c$site <- "Msekera Research Station"
	c$adm1 <- "Chipata"
	c$adm2 <- NA
	c$adm3 <- NA
	c$elevation <- NA
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	c$longitude <- -13.470413670095095
	c$latitude <- 32.49826506313018

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	c$crop <- "Maize" 

##### Fertilizers #####
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   c$P_fertilizer <- NA
   c$K_fertilizer <- NA
   c$N_fertilizer <- NA
   c$S_fertilizer <- NA
   c$lime <- NA
## normalize names 
   c$fertlizer_type <- NA
   c$inoculated <- FALSE
   c$inoculant <- NA
   
##### in general, add comments to your script if computations are
##### based on information gleaned from metadata, a publication, 
##### or when they are not immediately obvious for other reasons

##### Yield #####
	#what plant part does yield refer to?
	 c$yield_part <- "Grain"
   
  c <- c[, c("country","adm1","site","longitude","latitude", "harvest_year","crop","treatment","rep","yield_part","biomass_total","yield")]
	
###############################################################################################################################################
###############################LEGUMES DATA###################################################
  ## the treatment code	
  c1$treatment <- c1$Tmnt.
  c1$harvest_year <- c1$Year
  c1$crop <- c1$Crop
  c1$rep <- c1$Rep
  c1$biomass_total <- c1$`Biomass yield (kg/ha)`
  c1$yield <- c1$`Grain/cotton yield (kg/ha)`
  ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  c1$country <- "Zambia"
  c1$site <- "Msekera Research Station"
  c1$adm1 <- "Chipata"
  c$adm2 <- NA
  c$adm3 <- NA
  c$elevation <- NA
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
  c1$longitude <- -13.470413670095095
  c1$latitude <- 32.49826506313018
  
  ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  c1$crop <- c1$Crop
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  c1$P_fertilizer <- NA
  c1$K_fertilizer <- NA
  c1$N_fertilizer <- NA
  c1$S_fertilizer <- NA
  c1$lime <- NA
  ## normalize names 
  c1$fertlizer_type <- NA
  c1$inoculated <- FALSE
  c1$inoculant <- NA
  
  ##### in general, add comments to your script if computations are
  ##### based on information gleaned from metadata, a publication, 
  ##### or when they are not immediately obvious for other reasons
  
  ##### Yield #####
  #what plant part does yield refer to?
  c1$yield_part <- "Grain"
  
  c1 <- c1[, c("country","adm1","site","longitude","latitude", "harvest_year","crop","treatment","rep","yield_part","biomass_total","yield")]
  
  
# all scripts must end like this
	carobiner::write_files(dset, c, c1, path=path)
}



## now test your function in a clean R environment 
# path <- _____
# carob_script(path)

