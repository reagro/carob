# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [Farmers’ participatory researchers managed long-term trails aimed to improve the productivity, profitability, and sustainability of smallholder agriculture in the EGP by activities carried out to address the objectives: 1. Understand farmer circumstances with respect to cropping systems, natural and economic resources base, livelihood strategies, and capacity to bear risk and undertake technological innovation. 2. Develop with farmers more productive and sustainable technologies that are resilient to climate risks and profitable for small holders. 3. Facilitate widespread adoption of sustainable, resilient, and more profitable farming systems. (2018-02-18)]

"

	uri <- "hdl:11529/10547970"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "?"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Gathala, Mahesh K.; Tiwari, Thakur P.; Islam, Saiful; Chowdhury, Apurba K.; Bhattacharya, Prateek M.; Das, K.K.; Dhar, Tapamay; Pradhan, K.; Sinha, A.K.; Ghosh, Arunava; Mitra, B.; Chattopadhyay, C., 2018, '2.8-Rabi (winter) crops-all nodes-Long term trial (LT)-Malda-West Bengal', https://hdl.handle.net/11529/10547970, CIMMYT Research Data & Software Repository Network, V2",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "",
		data_institutions = "CIMMYT",
   		data_type="experiment", 
		carob_contributor="Mitchelle Njukuya",
		# date of first submission to carob
		carob_date="2023-10-10" 
	)

## download and read data 
  path <- "C:/Users/user/Documents/DataAnalysis/carob-Malda"
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)


	f <- ff[basename(ff) == "Rabi Maize 2016-17-LT-all nodes-Malda.xlsx"]
 library("readxl")
	r <- read_excel(f)
	r <- readxl::read_excel(f,sheet = "4- Stand counts & Phenology") |> as.data.frame()

#################################################### 4- Stand counts & Phenology #############################################	
## use a subset
	d <- r

	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE 
	d$is_experiment <- TRUE
	d$irrigated <- TRUE
## the treatment code	
	d$treatment <- d$Tmnt
	d$trial_id <- d$`Trial Code`

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "India"
	d$site <- "West Benga"
	d$adm1 <- "Malda"
	d$adm2 <- d$Node
	d$adm3 <- NA
	d$elevation <- NA
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude [d$adm2 =="Mahadipur"] <- 88.1265
	d$latitude [d$adm2 =="Mahadipur"] <- 24.8501 
	d$longitude [d$adm2 =="Bidyanandapur"] <- 87.9903
	d$latitude [d$adm2 =="Bidyanandapur"] <- 25.9517
	d$longitude [d$adm2 =="Urgitola"] <- 88.1411
	d$latitude [d$adm2 =="Urgitola"] <- 25.0108

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- d$Crop
	d$variety <- d$Variety

##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- d$`Date of seeding (dd-mm-yy)`
	d$harvest_date  <- d$`Datw of harvest (dd-mm-yy)`

##### Fertilizers #####
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- NA
   d$K_fertilizer <-NA
   d$N_fertilizer <- NA
   d$S_fertilizer <- NA
   d$lime <- NA
## normalize names 
   d$fertlizer_type <- NA
   d$inoculated <- FALSE
   d$inoculant <- NA
   d$season <- d$Season  
   d$emergence <- d$`100% emergence (DAS)`
   d$flowering <- d$`50% anthesis (DAS)`
   d$maturity <- d$`80% physiological maturity (DAS)`
   d$harvest <- d$`Harvesting (DAS)`
   
##### in general, add comments to your script if computations are
##### based on information gleaned from metadata, a publication, 
##### or when they are not immediately obvious for other reasons

##### Yield #####
	d$biomass_total <- NA

	d$yield <- NA
	#what plant part does yield refer to?
	d$yield_part <- NA 
	
	d <- d[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","country","site","adm1","adm2","longitude","latitude","season","treatment","trial_id","crop","variety","planting_date","harvest_date","inoculated","inoculant","biomass_total","emergence","flowering","maturity","harvest")]

	######################## END OF 4- Stand counts & Phenology ##################################
	
 #############################6 - Fertilizer amounts ##########################################
	
	## process file(s)
	r1 <- readxl::read_excel(f,sheet = "6 - Fertilizer amounts ") |> as.data.frame()              
	
	d1 <- r1
	
	
	#### about the data #####
	## (TRUE/FALSE)
	
	d1$dataset_id <- dataset_id
	d1$on_farm <- TRUE
	d1$is_survey <- FALSE
	d1$is_experiment <- TRUE
	d1$irrigated <- TRUE
	## the treatment code	
	d1$treatment <- d1$Tmnt 
	d1$season <- d1$Season
	
	##### Location #####
	## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
	## you can use carobiner::fix_name()
	d1$country <- "India"
	d1$site <- "West Benga"
	d1$adm1 <- "Malda"
	d1$adm2 <- d1$Node
	d1$adm3 <- NA
	d1$elevation <- NA
	## each site must have corresponding longitude and latitude
	## see carobiner::geocode
	d1$longitude [d1$adm2 =="Mahadipur"] <- 88.1265
	d1$latitude [d1$adm2 =="Mahadipur"] <- 24.8501 
	d1$longitude [d1$adm2 =="Bidyanandapur"] <- 87.9903
	d1$latitude [d1$adm2 =="Bidyanandapur"] <- 25.9517
	d1$longitude [d1$adm2 =="Urgitola"] <- 88.1411
	d1$latitude [d1$adm2 =="Urgitola"] <- 25.0108
	
	##### Crop #####
	## normalize variety names
	## see carobiner::fix_name
	d1$crop <- d1$`Types of Trail`
	d1$variety <- NA
	d1$trial_id <- d1$`Trial Code`
	
	##### Time #####
	## time can be year (four characters), year-month (7 characters) or date (10 characters).
	## use 	as.character(as.Date()) for dates to assure the correct format.
	d1$planting_date <- NA
	d1$harvest_date  <- NA
	
	##### Fertilizers #####
	## note that we use P and K, not P2O5 and K2O
	d1$P_fertilizer <- "25"
	d1$K_fertilizer <- "25"
	d1$N_fertilizer <- "160"
	d1$S_fertilizer <- NA
	d1$lime <- NA
	d1$Zn_fertilizer <- NA
	d1$Boric_acid <- "0.79"
	
	
	d1$fertlizer_type_1 <- d1$`Product used...19`
	d1$fertlizer_type_2 <- d1$`Product used...40`
	
	d1$inoculated <- FALSE
	d1$inoculant <- NA
	
	##### in general, add comments to your script if computations are
	##### based on information gleaned from metadata, a publication, 
	##### or when they are not immediately obvious for other reasons
	
	##### Yield #####
	d1$biomass_total <- NA
	
	d1$yield <- NA
	#what plant part does yield refer to?
	d1$yield_part <- NA 
	
	d1 <- d1[,c("dataset_id","is_survey","on_farm","is_experiment","irrigated","country","site","adm1","longitude","latitude","treatment","crop","trial_id","P_fertilizer","N_fertilizer","K_fertilizer","Boric_acid","fertlizer_type_1","fertlizer_type_2",)]
################################### END OF 6 - Fertilizer amounts #######################################
	
############################## 12 - Biomass sample###########################################
		
	r2 <- readxl::read_excel(f,sheet = "12 - Biomass samples") |> as.data.frame()
	
	d2 <- r2
	
	#### about the data #####
	## (TRUE/FALSE)
	
	d2$dataset_id <- dataset_id
	d2$on_farm <- TRUE
	d2$is_survey <- FALSE 
	d2$is_experiment <- TRUE
	d2$irrigated <- TRUE
	## the treatment code	
	d2$treatment <- d2$Tmnt
	
	##### Location #####
	## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
	## you can use carobiner::fix_name()
	d2$country <- "India"
	d2$site <- "West Benga"
	d2$adm1 <- "Malda"
	d2$adm2 <- d2$Node
	d2$adm3 <- NA
	d2$elevation <- NA
	## each site must have corresponding longitude and latitude
	## see carobiner::geocode
	d2$longitude [d2$adm2 =="Mahadipur"] <- 88.1265
	d2$latitude [d2$adm2 =="Mahadipur"] <- 24.8501 
	d2$longitude [d2$adm2 =="Bidyanandapur"] <- 87.9903
	d2$latitude [d2$adm2 =="Bidyanandapur"] <- 25.9517
	d2$longitude [d2$adm2 =="Urgitola"] <- 88.1411
	d2$latitude [d2$adm2 =="Urgitola"] <- 25.0108
	d2$season <- d2$Season
	
	
	##### Crop #####
	## normalize variety names
	## see carobiner::fix_name
	d2$crop <- d2$`Types of Trial`
	d2$variety <- NA
	d2$trial_id <- d2$`Trial Code`
	
	##### Time #####
	## time can be year (four characters), year-month (7 characters) or date (10 characters).
	## use 	as.character(as.Date()) for dates to assure the correct format.
	d2$planting_date <- NA
	d2$harvest_date  <- NA
	
	##### Fertilizers #####
	## note that we use P and K, not P2O5 and K2O
	## P <- P2O5 / 2.29
	## K <- K2O / 1.2051
	d2$P_fertilizer <- NA
	d2$K_fertilizer <-NA
	d2$N_fertilizer <- NA
	d2$S_fertilizer <- NA
	d2$lime <- NA
	## normalize names 
	d2$fertlizer_type <- NA
	d2$inoculated <- FALSE
	d2$inoculant <- NA
	
	##### in general, add comments to your script if computations are
	##### based on information gleaned from metadata, a publication, 
	##### or when they are not immediately obvious for other reasons
	
	##### Yield #####
	d2$above_ground_biomass1 <- d2$`Top portion 1 (g)...16`
	d2$above_ground_biomass2 <- d2$`Top portion 2 (g)...18`
	d2$above_ground_biomass3 <- d2$`Top portion 3 (g)...20`
	d2$biomass_total <- d2$`Sun dry biomass (t/ha)`
	
	d2$yield <- NA
	#what plant part does yield refer to?
	d2$yield_part <- "grain"
	
	d2 <- d2[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","country","site","adm1","adm2","longitude","latitude","season","treatment","trial_id","crop","inoculated","inoculant","above_ground_biomass1","above_ground_biomass2","above_ground_biomass3","biomass_total","yield_part")]
########################### END OF 12 - Biomass samples ################################################
	
##############################14 - Grain Harvest ##########################################################

	r3 <- readxl::read_excel(f,sheet = "14 - Grain Harvest ") |> as.data.frame() 
	
	d3 <- r3

	#### about the data #####
	## (TRUE/FALSE)
	
	d3$dataset_id <- dataset_id
	d3$on_farm <- TRUE
	d3$is_survey <- FALSE
	d3$is_experiment <- TRUE
	d3$irrigated <- TRUE
	## the treatment code	
	d3$treatment <- d3$Tmnt
	
	##### Location #####
	## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
	## you can use carobiner::fix_name()
	d3$country <- "India"
	d3$site <- "West Benga"
	d3$adm1 <- "Malda"
	d3$adm2 <- d3$Node
	d3$adm3 <- NA
	d3$elevation <- NA
	## each site must have corresponding longitude and latitude
	## see carobiner::geocode
	d3$longitude [d3$adm2 =="Mahadipur"] <- 88.1265
	d3$latitude [d3$adm2 =="Mahadipur"] <- 24.8501 
	d3$longitude [d3$adm2 =="Bidyanandapur"] <- 87.9903
	d3$latitude [d3$adm2 =="Bidyanandapur"] <- 25.9517
	d3$longitude [d3$adm2 =="Urgitola"] <- 88.1411
	d3$latitude [d3$adm2 =="Urgitola"] <- 25.0108
	d3$season <- d3$Season
	##### Crop #####
	## normalize variety names
	## see carobiner::fix_name
	d3$crop <- d3$`Types of Trial`
	d3$trial_id <- d3$`Trial Code`
	d3$variety <- NA
	d3$season <- d3$Season
	
	##### Time #####
	## time can be year (four characters), year-month (7 characters) or date (10 characters).
	## use 	as.character(as.Date()) for dates to assure the correct format.
	d3$planting_date <- NA
	d3$harvest_date  <- NA
	
	##### Fertilizers #####
	## note that we use P and K, not P2O5 and K2O
	## P <- P2O5 / 2.29
	## K <- K2O / 1.2051
	d3$P_fertilizer <- NA
	d3$K_fertilizer <-NA
	d3$N_fertilizer <- NA
	d3$S_fertilizer <- NA
	d3$lime <- NA
	## normalize names 
	d3$fertlizer_type <- NA
	d3$inoculated <- FALSE
	d3$inoculant <- NA
	
	##### in general, add comments to your script if computations are
	##### based on information gleaned from metadata, a publication, 
	##### or when they are not immediately obvious for other reasons
	
	##### Yield #####
	d3$biomass_total <- d3$`Biomass (t/ha)`
	d3$residue_yield <- d3$`Straw yield (t/ha)`  
	d3$yield <- d3$`Grain yield (t/ha)`
	#what plant part does yield refer to?
	d3$yield_part <- "grain"
	
	d3 <- d3[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","treatment","trial_id","country","site","adm1","adm2","longitude","latitude","season","inoculated","inoculant","biomass_total","residue_yield","yield","yield_part")]       
	

# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

