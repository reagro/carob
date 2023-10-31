carob_script <- function(path) {
  
  "Description:

    [Farmers’ participatory researchers managed long-term trails aimed to improve the productivity, profitability, and sustainability of smallholder agriculture in the EGP by activities carried out to address the objectives: 1. Understand farmer circumstances with respect to cropping systems, natural and economic resources base, livelihood strategies, and capacity to bear risk and undertake technological innovation. 2. Develop with farmers more productive and sustainable technologies that are resilient to climate risks and profitable for small holders. 3. Facilitate widespread adoption of sustainable, resilient, and more profitable farming systems. (2018-02-18)]

"
  
  uri <- "hdl:11529/10547970"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "wheat_trials"
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
    carob_date="2023-10-19" 
  )
  
  ## download and read data 
  path <- "C:/Users/user/Documents/DataAnalysis/carob-Malda"
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  
  f1 <- ff[basename(ff) == "Wheat 2016-17-LT-all nodes-Malda.xlsx"]
  library("readxl")
  r4 <- read_excel(f1)
  r4 <- readxl::read_excel(f1,sheet = "4- Stand counts & Phenology") |> as.data.frame()
  
  #################################################### 4- Stand counts & Phenology #############################################	
  ## use a subset
  d4 <- r4
  
  
  #### about the data #####
  ## (TRUE/FALSE)
  
  d4$dataset_id <- dataset_id
  d4$on_farm <- TRUE
  d4$is_survey <- FALSE 
  d4$is_experiment <- TRUE
  d4$irrigated <- TRUE
  ## the treatment code	
  d4$treatment <- d4$Tmnt
  d4$trial_id <- d4$`Trial Code`
  
  ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  d4$country <- "India"
  d4$site <- "West Benga"
  d4$adm1 <- "Malda"
  d4$adm2 <- d4$Node
  d4$adm3 <- NA
  d4$elevation <- NA
  d4$year <- d4$Year
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
  d4$longitude [d4$adm2 =="Mahadipur"] <- 88.1265
  d4$latitude [d4$adm2 =="Mahadipur"] <- 24.8501 
  d4$longitude [d4$adm2 =="Bidyanandapur"] <- 87.9903
  d4$latitude [d4$adm2 =="Bidyanandapur"] <- 25.9517
  d4$longitude [d4$adm2 =="Urgitola"] <- 88.1411
  d4$latitude [d4$adm2 =="Urgitola"] <- 25.0108
  d4$longitude [d4$adm2 =="Gaurangapur"] <- 87.8503
  d4$latitude [d4$adm2 =="Gaurangapur"] <- 25.4189
  ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  d4$crop <- d4$Crop
  d4$variety <- d4$Variety
  
  ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  d4$planting_date <- d4$`Date of seeding (dd-mm-yy)`
  d4$harvest_date  <- d4$`Datw of harvest (dd-mm-yy)`
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  d4$P_fertilizer <- NA
  d4$K_fertilizer <-NA
  d4$N_fertilizer <- NA
  d4$S_fertilizer <- NA
  d4$lime <- NA
  ## normalize names 
  d4$fertlizer_type <- NA
  d4$inoculated <- FALSE
  d4$inoculant <- NA
  d4$season <- d4$Season  
  d4$emergence <- d4$`100% emergence (DAS)`
  d4$flowering <- d4$`50% anthesis (DAS)`
  d4$maturity <- d4$`80% physiological maturity (DAS)`
  d4$harvest <- d4$`Harvesting (DAS)`
  
  ##### in general, add comments to your script if computations are
  ##### based on information gleaned from metadata, a publication, 
  ##### or when they are not immediately obvious for other reasons
  
  ##### Yield #####
  d4$biomass_total <- NA
  
  d4$yield <- NA
  #what plant part does yield refer to?
  d4$yield_part <- NA 
  
  d4 <- d4[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","year","country","site","adm1","adm2","longitude","latitude","season","treatment","trial_id","crop","variety","planting_date","harvest_date","inoculated","inoculant","biomass_total","emergence","flowering","maturity","harvest")]
  
  ######################## END OF 4- Stand counts & Phenology ##################################
  
  #############################6 - Fertilizer amounts ##########################################
  
  ## process file(s)
  r5 <- readxl::read_excel(f1,sheet = "6 - Fertilizer amounts ") |> as.data.frame()              
  
  d5 <- r5
  
  #### about the data #####
  ## (TRUE/FALSE)
  d5$dataset_id <- dataset_id
  d5$on_farm <- TRUE
  d5$is_survey <- FALSE
  d5$is_experiment <- TRUE
  d5$irrigated <- TRUE
  ## the treatment code	
  d5$treatment <- d5$Tmnt 
  d5$trial_id<- d5$`Trial Code`
  d5$season <- d5$Season
  
  ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  d5$country <- "India"
  d5$site <- "West Benga"
  d5$adm1 <- "Malda"
  d5$adm2 <- d5$Node
  d5$adm3 <- NA
  d5$elevation <- NA
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
  d5$longitude [d5$adm2 =="Mahadipur"] <- 88.1265
  d5$latitude [d5$adm2 =="Mahadipur"] <- 24.8501 
  d5$longitude [d5$adm2 =="Bidyanandapur"] <- 87.9903
  d5$latitude [d5$adm2 =="Bidyanandapur"] <- 25.9517
  d5$longitude [d5$adm2 =="Urgitola"] <- 88.1411
  d5$latitude [d5$adm2 =="Urgitola"] <- 25.0108
  d5$longitude [d5$adm2 =="Gaurangapur"] <- 87.8503
  d5$latitude [d5$adm2 =="Gaurangapur"] <- 25.4189 
  ##### Crop #####
  ## see carobiner::fix_name
  
  d5$crop <- d5$`Types of Trail`
  d5$variety <- NA
  d5$trial_id <- d5$`Trial Code`
  
  ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  d5$planting_date <- NA
  d5$harvest_date  <- NA
  d5$year <- d5$Year
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  d5$P_fertilizer <- "23"
  d5$K_fertilizer <- "121"
  d5$N_fertilizer <- "122"
  d5$B_fertilizer <- "21"
  d5$S_fertilizer <- NA
  d5$lime <- NA
  d5$Boric_acid <- "0.20"
  
  #MoP stands for Murate of Potash
  d5$fertlizer_type_1 <- d5$`Product used...12`
  d5$fertlizer_type_2 <- d5$`Product used...19`
  d5$fertlizer_type_3 <- d5$`Product used...26`
  d5$fertlizer_type_4 <- d5$`Product used...33`
  d5$fertlizer_type_5 <- d5$`Product used...40`
  
  d5$inoculated <- FALSE
  d5$inoculant <- NA
  
  ##### in general, add comments to your script if computations are
  ##### based on information gleaned from metadata, a publication, 
  ##### or when they are not immediately obvious for other reasons
  
  ##### Yield #####
  d5$biomass_total <- NA
  
  d5$yield <- NA
  #what plant part does yield refer to?
  d5$yield_part <- NA 
  
  d5 <- d5[,c("dataset_id","is_survey","on_farm","is_experiment","irrigated","year","country","site","adm1","adm2","longitude","latitude","treatment","crop","trial_id","P_fertilizer","N_fertilizer","K_fertilizer","B_fertilizer","Boric_acid","fertlizer_type_1","fertlizer_type_2","fertlizer_type_3","fertlizer_type_4","fertlizer_type_5")]
  ################################### END OF 6 - Fertilizer amounts #######################################
  
  ############################## 12 - Biomass sample###########################################
  
  r6 <- readxl::read_excel(f1,sheet = "12 - Biomass samples") |> as.data.frame()
  
  d6 <- r6
  
  #### about the data #####
  ## (TRUE/FALSE)
  
  d6$dataset_id <- dataset_id
  d6$on_farm <- TRUE
  d6$is_survey <- FALSE 
  d6$is_experiment <- TRUE
  d6$irrigated <- TRUE
  ## the treatment code	
  d6$treatment <- d6$Tmnt
  
  ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  d6$country <- "India"
  d6$site <- "West Benga"
  d6$adm1 <- "Malda"
  d6$adm2 <- d6$Node
  d6$adm3 <- NA
  d6$elevation <- NA
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
  d6$longitude [d6$adm2 =="Mahadipur"] <- 88.1265
  d6$latitude [d6$adm2 =="Mahadipur"] <- 24.8501 
  d6$longitude [d6$adm2 =="Bidyanandapur"] <- 87.9903
  d6$latitude [d6$adm2 =="Bidyanandapur"] <- 25.9517
  d6$longitude [d6$adm2 =="Urgitola"] <- 88.1411
  d6$latitude [d6$adm2 =="Urgitola"] <- 25.0108
  d6$longitude [d6$adm2 =="Gaurangapur"] <- 87.8503
  d6$latitude [d6$adm2 =="Gaurangapur"] <- 25.4189 
  d6$season <- d6$Season
  d6$year <- d6$Year
  
  ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  d6$crop <- "wheat"
  d6$variety <- NA
  d6$trial_id <- d6$`Trial Code`
  
  ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  d6$planting_date <- NA
  d6$harvest_date  <- NA
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  d6$P_fertilizer <- NA
  d6$K_fertilizer <-NA
  d6$N_fertilizer <- NA
  d6$S_fertilizer <- NA
  d6$lime <- NA
  ## normalize names 
  d6$fertlizer_type <- NA
  d6$inoculated <- FALSE
  d6$inoculant <- NA
  
  ##### in general, add comments to your script if computations are
  ##### based on information gleaned from metadata, a publication, 
  ##### or when they are not immediately obvious for other reasons
  
  ##### Yield #####
  d6$above_ground_biomass1 <- d6$`Top portion 1 (g)...14`
  d6$above_ground_biomass2 <- d6$`Top portion 2 (g)...16`
  d6$above_ground_biomass3 <- d6$`Top portion 3 (g)...24`
  d6$biomass_total <- d6$`Sun dry biomass (t/ha)`
  
  d6$yield <- NA
  #what plant part does yield refer to?
  d6$yield_part <- "grain"
  
  d6 <- d6[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","country","site","adm1","adm2","longitude","latitude","season","year","treatment","trial_id","crop","inoculated","inoculant","above_ground_biomass1","above_ground_biomass2","above_ground_biomass3","biomass_total","yield_part")]
  ########################### END OF 12 - Biomass samples ################################################
  
  ##############################14 - Grain Harvest ##########################################################
  
  r7 <- readxl::read_excel(f1,sheet = "14 - Grain Harvest ") |> as.data.frame() 
  
  d7 <- r7
  
  #### about the data #####
  ## (TRUE/FALSE)
  
  d7$dataset_id <- dataset_id
  d7$on_farm <- TRUE
  d7$is_survey <- FALSE
  d7$is_experiment <- TRUE
  d7$irrigated <- TRUE
  ## the treatment code	
  d7$treatment <- d7$Tmnt
  
  ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  d7$country <- "India"
  d7$site <- "West Benga"
  d7$adm1 <- "Malda"
  d7$adm2 <- d7$Node
  d7$adm3 <- NA
  d7$elevation <- NA
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
  d7$longitude [d7$adm2 =="Mahadipur"] <- 88.1265
  d7$latitude [d7$adm2 =="Mahadipur"] <- 24.8501 
  d7$longitude [d7$adm2 =="Bidyanandapur"] <- 87.9903
  d7$latitude [d7$adm2 =="Bidyanandapur"] <- 25.9517
  d7$longitude [d7$adm2 =="Urgitola"] <- 88.1411
  d7$latitude [d7$adm2 =="Urgitola"] <- 25.0108
  d7$longitude [d7$adm2 =="Gaurangapur"] <- 87.8503
  d7$latitude [d7$adm2 =="Gaurangapur"] <- 25.4189 
  d7$season <- d7$Season
  d7$year <- d7$Year
  ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  d7$crop <- d7$`Types of Trial`
  d7$trial_id <- d7$`Trial Code`
  d7$variety <- NA
  
  
  ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  d7$planting_date <- NA
  d7$harvest_date  <- NA
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  d7$P_fertilizer <- NA
  d7$K_fertilizer <-NA
  d7$N_fertilizer <- NA
  d7$S_fertilizer <- NA
  d7$lime <- NA
  ## normalize names 
  d7$fertlizer_type <- NA
  d7$inoculated <- FALSE
  d7$inoculant <- NA
  
  ##### in general, add comments to your script if computations are
  ##### based on information gleaned from metadata, a publication, 
  ##### or when they are not immediately obvious for other reasons
  
  ##### Yield #####
  d7$biomass_total <- d7$`Biomass (t/ha)`
  d7$residue_yield <- d7$`Straw yield (t/ha)`  
  d7$yield <- d7$`Grain yield`
  d7$harvestable_index <- d7$HI
  #what plant part does yield refer to?
  d7$yield_part <- "grain"
  
  d7 <- d7[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","treatment","trial_id","country","site","adm1","adm2","longitude","latitude","season","inoculated","inoculant","biomass_total","residue_yield","yield","yield_part","harvestable_index")]       
  
  
  # all scripts must end like this
  carobiner::write_files(dset, d, path=path)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

