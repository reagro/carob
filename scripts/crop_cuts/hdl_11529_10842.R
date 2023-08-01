# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [copy This data set is from a long-term (2010-2016) trial set in sandy soils. The study seeks to monitor and evaluate the effects over time of conservation agriculture (CA) practices on crop yield, soil quality, weeds, pests and diseases. The trial was set as a randomised complete block design with the following treatments: T1: Check plot (CP); traditional farmers practice using the mouldboard plough, maize as a sole crop, no residue retention, stubbles incorporated T2: Direct seeding with animal drawn seeder (DSM), maize as a sole crop, residue retention (at a rate of 2.5-3 t ha-1 in the first year, thereafter all crop residues retained) T3: Basin (BAM), maize as a sole crop, residue retention T4: Jab planter (JPM), maize as a sole crop, residue retention T5: Direct seeding with animal drawn seeder (DSMB), biochar incorporated, maize as a sole crop, residue retention T6: Direct seeding with animal drawn seeder (DSMP), maize-pigeon pea (Cajanus cajan) intercropping, residue retention T7: Crop rotation A1 (A1M): direct seeding with animal drawn seeder, maize-groundnut rotation (Phase 1), residue retention; Maize- Groundnut T8: Crop rotation A2(A2G): direct seeding with animal drawn seeder, maize-groundnuts rotation (Phase 2), residue retention; Groundnuts- Maize T9: Crop rotation B1 (B1M): direct seeding with animal drawn seeder, maize-sunflower rotation (Phase 1), residue retention; Maize- Sunflower T10: Crop rotation B2 (B2S): direct seeding with animal drawn seeder, maize-sunflower rotation (Phase 2), residue retention; Sunflower- Maize. (2016)the abstract from the repo]

"

	uri <- "hdl:11529/10842"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "crop_cuts"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation='Thierfelder, Christian; Blessing Mhlanga, 2016, "Monitoring and evaluation of the effects over time of conservation agriculture practices on crop yield, soil quality, weeds, pests and diseases., https://hdl.handle.net/11529/10842, CIMMYT Research Data & Software Repository Network, V1"',
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "2016-12-20",
		data_institutions = "CIMMYT",
   		data_type="experimental data", # or, e.g. "on-farm experiment", "survey", "compilation"
		carob_contributor="Shumirai Manzvera" 
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=2)
	dset$license <- carobiner::get_license(js)


	f <- ff[basename(ff) == "Domboshawa 2010.2016.xlsx"]


	r2 <- readxl::read_excel(f,sheet = 2) |> as.data.frame()
	r1 <- readxl::read_excel(f,sheet = 1) |> as.data.frame()
	

	
## process file(s)

## use a subset
	d1 <- r1
	d2<- r2
#### about the data #####
## (TRUE/FALSE)

	d1$dataset_id <- dataset_id
	d1$on_farm <- TRUE
	d1$is_survey <- FALSE
	d1$is_experiment <- TRUE
	d1$irrigated <- FALSE
## the treatment code	
	d1$treatment <- NA
	d1$treatment[d1$Tmnt.==1]<-"CP"
	d1$treatment[d1$Tmnt.==2]<-"DSM"
	d1$treatment[d1$Tmnt.==3]<-"BAM"
	d1$treatment[d1$Tmnt.==4]<-"JPM"
	d1$treatment[d1$Tmnt.==5]<-"DSMB)"
	d1$treatment[d1$Tmnt.==6]<-"DSMP"
	d1$treatment[d1$Tmnt.==7]<-"A1M"
	d1$treatment[d1$Tmnt.==8]<-"A2G"
	d1$treatment[d1$Tmnt.==9]<-"B1M"
	d1$treatment[d1$Tmnt.==10]<-"B2S"
	
##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d1$country <- d1$Country
	d1$site <- d1$Location
	d1$adm1 <- d1$"Mashonaland East"
	d1$adm2 <- d1$"Goromonzi District"
	d1$adm3 <- d1$"Ward 4"
	d$elevation <- NA
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- 
	d$latitude <- 

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <-
	d1$crop <- NA
	d1$crop[d1$Crop=="Maize"]<-"Maize"
	d1$crop[d1$Crop=="MAIZE"]<-"Maize"
	d1$crop[d1$Crop=="Maize/Ppea"]<-"Maize+Ppea"
	d1$crop[d1$Crop=="MAIZE+Cowpea"]<-"Maize+Cowpea"

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
	d1$biomass_total <- d$`Biomass yield (kg/ha)`
	d$yield <- 
	#what plant part does yield refer to?
	d1$yield_part <- d$`Grain/cotton yield (kg/ha)`
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)

