# R script for "carob"

## ISSUES
# ....

carob_script <- function(path) {

"Description:

    This data set is from a long-term (2010-2016) trial set in sandy soils. The study seeks to monitor and evaluate the effects over time of conservation agriculture (CA) practices on crop yield, soil quality, weeds, pests and diseases. The trial was set as a randomised complete block design with the following treatments: T1: Check plot (CP); traditional farmers practice using the mouldboard plough, maize as a sole crop, no residue retention, stubbles incorporated T2: Direct seeding with animal drawn seeder (DSM), maize as a sole crop, residue retention (at a rate of 2.5-3 t ha-1 in the first year, thereafter all crop residues retained) T3: Basin (BAM), maize as a sole crop, residue retention T4: Jab planter (JPM), maize as a sole crop, residue retention T5: Direct seeding with animal drawn seeder (DSMB), biochar incorporated, maize as a sole crop, residue retention T6: Direct seeding with animal drawn seeder (DSMP), maize-pigeon pea (Cajanus cajan) intercropping, residue retention T7: Crop rotation A1 (A1M): direct seeding with animal drawn seeder, maize-groundnut rotation (Phase 1), residue retention; Maize- Groundnut T8: Crop rotation A2(A2G): direct seeding with animal drawn seeder, maize-groundnuts rotation (Phase 2), residue retention; Groundnuts- Maize T9: Crop rotation B1 (B1M): direct seeding with animal drawn seeder, maize-sunflower rotation (Phase 1), residue retention; Maize- Sunflower T10: Crop rotation B2 (B2S): direct seeding with animal drawn seeder, maize-sunflower rotation (Phase 2), residue retention; Sunflower- Maize. (2016)

"

	uri <- "hdl:11529/10842"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "conservation_agriculture"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation='Thierfelder, Christian; Blessing Mhlanga, 2016, "Monitoring and evaluation of the effects over time of conservation agriculture practices on crop yield, soil quality, weeds, pests and diseases., https://hdl.handle.net/11529/10842, CIMMYT Research Data & Software Repository Network, V1"',
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= NA,
		data_institutions = "CIMMYT",
   		data_type="experimental data", # or, e.g. "on-farm experiment", "survey", "compilation"
		carob_contributor="Shumirai Manzvera" 
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=2)
	dset$license <- carobiner::get_license(js)

	f <- ff[basename(ff) == "Domboshawa 2010.2016.xlsx"]

	r1 <- readxl::read_excel(f,sheet = 1) |> as.data.frame()
	r2 <- readxl::read_excel(f,sheet = 2) |> as.data.frame()
	
	
## process file(s)

## use a subset
#### about the data #####
## (TRUE/FALSE)

	d1 <- data.frame(country = r1$Country)
	d1$dataset_id <- dataset_id
	d1$on_farm <- TRUE
	d1$is_survey <- FALSE
	d1$irrigated <- FALSE
## the treatment code	
	tcodes <- c("CP", "DSM", "BAM", "JPM", "DSMB)", "DSMP", "A1M", "A2G", "B1M", "B2S")
	d1$treatment <- tcodes[r1$Tmnt.]
	
##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d1$location <- r1$Location
	d1$adm1 <- "Mashonaland East"
	d1$adm2 <- "Goromonzi District"
	d1$adm3 <- "Ward 4"
	d1$elevation <- as.numeric(NA)
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d1$longitude <-  31.17505
	d1$latitude <- -17.60859
	

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d1$crop <- "maize"

##### Yield #####
	d1$biomass_total <- r1$`Biomass yield (kg/ha)`
	#what plant part does yield refer to?
	d1$yield_part <- "grain"
	d1$yield <- r1$`Grain/cotton yield (kg/ha)`
	
	#farming type 
	d1$intercrops <- NA
	d1$intercrops[r1$Crop=="Maize/Ppea"] <- "pigeon pea"
	d1$intercrops[r1$Crop=="MAIZE+Cowpea"] <- "cowpea"
	
	#replication number
	d1$rep <- as.integer(r1$Rep)

	d1$trial_id <- "1"


	print("second dataset needs to be processed")
		
# all scripts must end like this
	carobiner::write_files(dset, d1, path=path)

}

