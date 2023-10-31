# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "Description:

    [Farmers’ participatory researchers managed long-term trails aimed to improve the productivity, profitability, and sustainability of smallholder agriculture in the EGP by activities carried out to address the objectives: 1. Understand farmer circumstances with respect to cropping systems, natural and economic resources base, livelihood strategies, and capacity to bear risk and undertake technological innovation. 2. Develop with farmers more productive and sustainable technologies that are resilient to climate risks and profitable for small holders. 3. Facilitate widespread adoption of sustainable, resilient, and more profitable farming systems. (2018-02-18)]

"
  
  uri <- "hdl:11529/10547970"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "crop_cuts"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=EiA,
    uri=uri,
    data_citation="Gathala, Mahesh K.; Tiwari, Thakur P.; Islam, Saiful; Chowdhury, Apurba K.; Bhattacharya, Prateek M.; Das, K.K.; Dhar, Tapamay; Pradhan, K.; Sinha, A.K.; Ghosh, Arunava; Mitra, B.; Chattopadhyay, C., 2018, '2.8-Rabi (winter) crops-all nodes-Long term trial (LT)-Malda-West Bengal', https://hdl.handle.net/11529/10547970, CIMMYT Research Data & Software Repository Network, V2",
    ## if there is a paper, include the paper's doi here
    ## also add a RIS file in references folder (with matching doi)
    publication= "",
    data_institutions = "CIMMYT",
    data_type="experiment", 
    carob_contributor="Mitchelle Njukuya",
    # date of first submission to carob
    carob_date="2023-10-26" 
  )
  
  ## download and read data 
  path <- "C:/Users/user/Documents/DataAnalysis/carob-Malda"
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  
  f2 <- ff[basename(ff) == "Lentil 2016-17-LT-all nodes-Malda.xlsx"]
  library("readxl")
  r8 <- read_excel(f2)
  r8 <- readxl::read_excel(f2,sheet = "4- Stand counts & Phenology") |> as.data.frame()
  
  #################################################### 4- Stand counts & Phenology #############################################	
  ## use a subset
  d8 <- r8
  
  
  #### about the data #####
  ## (TRUE/FALSE)
  
  d8$dataset_id <- dataset_id
  d8$on_farm <- TRUE
  d8$is_survey <- FALSE 
  d8$is_experiment <- TRUE
  d8$irrigated <- TRUE
  ## the treatment code	
  d8$treatment <- d8$Tmnt
  d8$trial_id <- d8$`Trial Code`
  
  ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  d8$country <- "India"
  d8$site <- "West Benga"
  d8$adm1 <- "Malda"
  d8$adm2 <- d8$Node
  d8$adm3 <- NA
  d8$elevation <- NA
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
  d8$longitude [d8$adm2 =="Kalinagar"] <- 88.1075
  d8$latitude [d8$adm2 =="Kalinagar"] <- 22.4403
  d8$longitude [d8$adm2 =="Gaurangapur"] <- 87.8665
  d8$latitude [d8$adm2 =="Gaurangapur"] <- 22.7377
  
  
  ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  d8$crop <- d8$Crop
  d8$variety <- d8$Variety
  
  ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  d8$planting_date <- d8$`Date of seeding (dd-mm-yy)`
  d8$harvest_date  <- d8$`Datw of harvest (dd-mm-yy)`
  d8$transplanting_date <- d8$`Date of transplanting (dd-mm-yy)`
  d8$row_spacing <- d8$`Row spacing (cm)`
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  d8$P_fertilizer <- NA
  d8$K_fertilizer <-NA
  d8$N_fertilizer <- NA
  d8$S_fertilizer <- NA
  d8$lime <- NA
  ## normalize names 
  d8$fertlizer_type <- NA
  d8$inoculated <- FALSE
  d8$inoculant <- NA
  d8$season <- d8$Season  
  d8$emergence <- d8$`100% emergence (DAS)`
  d8$flowering <- d8$`50% anthesis (DAS)`
  d8$maturity <- d8$`80% physiological maturity (DAS)`
  d8$harvest <- d8$`Harvesting (DAS)`
  d8$year <- d8$Year
  ##### in general, add comments to your script if computations are
  ##### based on information gleaned from metadata, a publication, 
  ##### or when they are not immediately obvious for other reasons
  
  ##### Yield #####
  d8$biomass_total <- NA
  
  d8$yield <- NA
  #what plant part does yield refer to?
  d8$yield_part <- NA 
  
  d8 <- d8[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","country","site","adm1","adm2","longitude","latitude","season","treatment","trial_id","crop","variety","planting_date","harvest_date","inoculated","inoculant","biomass_total","emergence","flowering","maturity","harvest")]
  
  ######################## END OF 4- Stand counts & Phenology ##################################
  
  #############################6 - Fertilizer amounts ##########################################
  
  ## process file(s)
  r9 <- readxl::read_excel(f2,sheet = "6 - Fertilizer amounts ") |> as.data.frame()              
  
  d9 <- r9
  
  
  #### about the data #####
  ## (TRUE/FALSE)
  
  d9$dataset_id <- dataset_id
  d9$on_farm <- TRUE
  d9$is_survey <- FALSE
  d9$is_experiment <- TRUE
  d9$irrigated <- TRUE
  ## the treatment code	
  d9$treatment <- d9$Tmnt 
  d9$trial_id <- d9$`Trial Code`
  d9$season <- d9$Season
  d9$year <- d9$Year
  
  ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  d9$country <- "India"
  d9$site <- "West Benga"
  d9$adm1 <- "Malda"
  d9$adm2 <- d9$Node
  d9$adm3 <- NA
  d9$elevation <- NA
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
  d9$longitude [d9$adm2 =="Kalinagar"] <- 88.1075
  d9$latitude [d9$adm2 =="Kalinagar"] <- 22.4403
  d9$longitude [d9$adm2 =="Gaurangapur"] <- 87.8665
  d9$latitude [d9$adm2 =="Gaurangapur"] <- 22.7377
  
  
  ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  d9$crop <- d9$`Types of Trail`
  d9$variety <- NA
  
  ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  d9$planting_date <- NA
  d9$harvest_date  <- NA
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2
  d9$P_fertilizer <- d9$`P2O5 (kg/ha)`
  d9$P_fertilizer2 <- as.character(d9$P_fertilizer)
  d9$P_fertilizer[d9$P_fertilizer==46.8117029257314] <- 20.52
  d9$P_fertilizer[d9$P_fertilizer==39.0097524381095] <- 17.03
  d9$K_fertilizer <- d9$`K2O (kg/ha)`
  d9$K_fertilizer2 <- as.character(d9$K_fertilizer)
  d9$K_fertilizer[d9$K_fertilizer==39.0097524381095] <- 32.3625
  d9$K_fertilizer[d9$K_fertilizer==46.8117029257314] <- 39.0010
  d9$N_fertilizer <- d9$`N  (kg/ha)`
  d9$N_splits <- "3"
  d9$S_fertilizer <- NA
  d9$lime <- NA
  d9$Zn_fertilizer <- NA
  d9$Boric_acid <- d9$`Boric acid (kg/ha)`
  
  
  d9$fertlizer_type_1 <- d9$`Product used...26`
  d9$fertlizer_type_2 <- d9$`Product used...40`
  
  d9$inoculated <- FALSE
  d9$inoculant <- NA
  
  ##### in general, add comments to your script if computations are
  ##### based on information gleaned from metadata, a publication, 
  ##### or when they are not immediately obvious for other reasons
  
  ##### Yield #####
  d9$biomass_total <- NA
  
  d9$yield <- NA
  #what plant part does yield refer to?
  d9$yield_part <- NA 
  
  d9 <- d9[,c("dataset_id","is_survey","is_experiment","on_farm","irrigated","country","site","adm1","adm2","longitude","latitude","treatment","crop","trial_id","P_fertilizer","N_fertilizer","N_splits","K_fertilizer","Boric_acid","fertlizer_type_1","fertlizer_type_2")]

  ################################### END OF 6 - Fertilizer amounts #######################################
  
  ############################## 12 - Biomass sample###########################################
  
  rr <- readxl::read_excel(f2,sheet = "12 - Biomass samples") |> as.data.frame()
  
  dd <- rr
  
  #### about the data #####
  ## (TRUE/FALSE)
  
  dd$dataset_id <- dataset_id
  dd$on_farm <- TRUE
  dd$is_survey <- FALSE 
  dd$is_experiment <- TRUE
  dd$irrigated <- TRUE
  ## the treatment code	
  dd$treatment <- dd$Tmnt
  
  ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  dd$country <- "India"
  dd$site <- "West Benga"
  dd$adm1 <- "Malda"
  dd$adm2 <- dd$Node
  dd$adm3 <- NA
  dd$elevation <- NA
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
  dd$longitude [dd$adm2 =="Kalinagar"] <- 88.1075
  dd$latitude [dd$adm2 =="Kalinagar"] <- 22.4403
  dd$longitude [dd$adm2 =="Gaurangapur"] <- 87.8665
  dd$latitude [dd$adm2 =="Gaurangapur"] <- 22.7377
  dd$season <- dd$Season
  
  
  ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  dd$crop <- dd$`Types of Trial`
  dd$variety <- NA
  dd$trial_id <- dd$`Trial Code`
  
  ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  dd$planting_date <- NA
  dd$harvest_date  <- NA
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  dd$P_fertilizer <- NA
  dd$K_fertilizer <-NA
  dd$N_fertilizer <- NA
  dd$S_fertilizer <- NA
  dd$lime <- NA
  ## normalize names 
  dd$fertlizer_type <- NA
  dd$inoculated <- FALSE
  dd$inoculant <- NA
  
  ##### in general, add comments to your script if computations are
  ##### based on information gleaned from metadata, a publication, 
  ##### or when they are not immediately obvious for other reasons
  
  ##### Yield #####
  dd$above_ground_biomass1 <- dd$`Top portion 1 (g)...14`
  dd$above_ground_biomass2 <- dd$`Top portion 2 (g)...16`
  dd$above_ground_biomass3 <- dd$`Top portion 3 (g)...18`
  dd$biomass_total <- dd$`Sun dry biomass (t/ha)`
  
  dd$yield <- NA
  #what plant part does yield refer to?
  dd$yield_part <- "grain"
 
  dd <- dd[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","country","site","adm1","adm2","longitude","latitude","season","treatment","trial_id","inoculated","inoculant","above_ground_biomass1","above_ground_biomass2","above_ground_biomass3","biomass_total","yield_part")]
  ########################### END OF 12 - Biomass samples ################################################
  
  ##############################14 - Grain Harvest ##########################################################
  
  rr1 <- readxl::read_excel(f2,sheet = "14 - Grain Harvest ") |> as.data.frame() 
  
  dd1 <- rr1
  
  #### about the data #####
  ## (TRUE/FALSE)
  
  dd1$dataset_id <- dataset_id
  dd1$on_farm <- TRUE
  dd1$is_survey <- FALSE
  dd1$is_experiment <- TRUE
  dd1$irrigated <- TRUE
  ## the treatment code	
  dd1$treatment <- dd1$Tmnt
  
  ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  dd1$country <- "India"
  dd1$site <- "West Benga"
  dd1$adm1 <- "Malda"
  dd1$adm2 <- dd1$Node
  dd1$adm3 <- NA
  dd1$elevation <- NA
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
  dd1$longitude [dd1$adm2 =="Kalinagar"] <- 88.1075
  dd1$latitude [dd1$adm2 =="Kalinagar"] <- 22.4403
  dd1$longitude [dd1$adm2 =="Gaurangapur"] <- 87.8665
  dd1$latitude [dd1$adm2 =="Gaurangapur"] <- 22.7377
  dd1$season <- dd1$Season
  
  ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  dd1$crop <- dd1$`Types of Trial`
  dd1$trial_id <- dd1$`Trial Code`
  dd1$variety <- NA
  dd1$season <- dd1$Season
  
  ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  dd1$planting_date <- NA
  dd1$harvest_date  <- NA
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  dd1$P_fertilizer <- NA
  dd1$K_fertilizer <-NA
  dd1$N_fertilizer <- NA
  dd1$S_fertilizer <- NA
  dd1$lime <- NA
  ## normalize names 
  dd1$fertlizer_type <- NA
  dd1$inoculated <- FALSE
  dd1$inoculant <- NA
  
  ##### in general, add comments to your script if computations are
  ##### based on information gleaned from metadata, a publication, 
  ##### or when they are not immediately obvious for other reasons
  
  ##### Yield #####
  dd1$biomass_total <- dd1$`Biomass (t/ha)`
  dd1$residue_yield <- dd1$`Straw yield (t/ha)`  
  dd1$yield <- dd1$`Grain yield (t/ha)`
  dd1$Harvestable_index <- dd1$HI
  #what plant part does yield refer to?
  dd1$yield_part <- "grain"
  
  dd1 <- dd1[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","treatment","trial_id","country","site","adm1","adm2","longitude","latitude","season","inoculated","inoculant","biomass_total","residue_yield","yield","yield_part")]       
  
  
  # all scripts must end like this
  carobiner::write_files(dset, d, path=path)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

