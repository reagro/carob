# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "Description:

  Farmer participatory on-farm trials with CA technologies comparing with farmers’ practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory validation trials were conducted comparing to existing practices and to find out suitable and more profitable crop production practices, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers
    


"
  
  uri <- "hdl:11529/10548008"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "conservation_agriculture"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    data_citation="Gathala, Mahesh K.; Tiwari, Thakur P.; Islam, Saiful; Ghosh, Anup K.; Islam, Rashadul; Anwar, Mazharul; Molla, Samim H.; Akhter-Ul-Alam, Md., 2018, '6.2- Rabi (winter) crops-all nodes- Validation trials -Rangpur-Bangladesh', https://hdl.handle.net/11529/10548008, CIMMYT Research Data & Software Repository Network, V2",
    ## if there is a paper, include the paper's doi here
    ## also add a RIS file in references folder (with matching doi)
    publication= NA,
    data_institutions = "CIMMYT",
    data_type="on-farm experiment",
    carob_contributor="Mitchelle Njukuya",
    carob_date="2023-09-21"
  )
  
  ## download and read data 
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  
  f3 <-  "C:/Users/user/Documents/DataAnalysis/carob-BangladashDataset/data/raw/wheat_trials/hdl_11529_10548008/Wheat 2014-15-DS&BP-all nodes Rangpur.xlsx"
  library("readxl")
  rr <- read_excel(f3)
  rr1 <- readxl::read_excel(f3,sheet = "4- Stand counts & Phenology") |> as.data.frame()
  rr2 <- readxl::read_excel(f3,sheet = "12 - Biomass samples") |> as.data.frame()
  rr3 <- readxl::read_excel(f3,sheet = "14 - Grain Harvest ") |> as.data.frame()
  rr4 <- readxl::read_excel(f3,sheet = "6 - Fertilizer amounts ") |> as.data.frame()
  
  ##################################### 4- stand counts & Phenology #####################################################################################################
  rr1 <- readxl::read_excel(f3,sheet = "4- Stand counts & Phenology") |> as.data.frame() 
  
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
  dd1$country <- "Bangladash"
  dd1$site <- "Rajshahi"
  dd1$adm1 <- dd1$Node
  dd1$adm2 <- NA
  dd1$adm3 <- NA
  dd1$elevation <- NA
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
  dd1$longitude [dd1$adm1=="Mohanpur"] <- 86.0160 
  dd1$latitude[dd1$adm1=="Mohanpur"] <- 25.7196
  dd1$longitude [dd1$adm1=="Lakkhhitari"] <- 89.2611
  dd1$latitude[dd1$adm1=="Lakkhhitari"] <- 25.7494
  ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  dd1$crop <- dd1$Crop...5
  dd1$variety <- dd1$Variety
  dd1$trial_id <- dd1$`Trial Code`
  ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  dd1$planting_date <- dd1$`Date of seeding (dd-mm-yy)`
  dd1$harvest_date  <- dd1$`Date of harvest (dd-mm-yy)`
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  dd1$P_fertilizer <- NA
  dd1$K_fertilizer <- NA
  dd1$N_fertilizer <- NA
  dd1$S_fertilizer <- NA
  dd1$lime <- NA
  ## normalize names 
  dd1$fertlizer_type <- NA
  dd1$inoculated <- FALSE
  dd1$inoculant <- NA
  dd1$season <- dd1$Season  
  dd1$emergence <- dd1$`100% emergence (DAS)`
  dd1$flowering <- dd1$`50% first flowering (DAS)`
  dd1$maturity <- dd1$`80% physiological maturity (DAS)`
  dd1$harvest <- dd1$`Harvesting (DAS)`
  
  ##### Yield #####
  dd1$biomass_total <- NA
  
  dd1$yield <- NA
  #what plant part does yield refer to?
  dd1$yield_part <- NA 
  
  dd1 <- dd1[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","country","site","adm1","longitude","latitude","season","treatment","trial_id","crop","variety","planting_date","harvest_date","inoculated","inoculant","biomass_total","emergence","flowering","maturity","harvest")]
  
  ################################################END OF 4- stand counts & Phenology############################################################################################################################################################################################
  
  ############################################## 12 - Biomass samples##########################################################################################################################################################################################
  ## process file(s)
  rr2 <- readxl::read_excel(f3,sheet = "12 - Biomass samples") |> as.data.frame()
     
  dd2 <- rr2
  
  
  #### about the data #####
  ## (TRUE/FALSE)
  
  dd2$dataset_id <- dataset_id
  dd2$on_farm <- TRUE
  dd2$is_survey <- FALSE 
  dd2$is_experiment <- TRUE
  dd2$irrigated <- TRUE
  ## the treatment code	
  dd2$treatment <- dd2$Tmnt
  
  ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  dd2$country <- "Bangladash"
  dd2$site <- "Rajshahi"
  dd2$adm1 <- dd2$Node
  dd2$adm2 <- NA
  dd2$adm3 <- NA
  dd2$elevation <- NA
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
  dd2$longitude[dd2$adm1=="Mohanpur"] <- 86.0106
  dd2$latitude[dd2$adm1=="Mohanpur"] <- 25.7196
  dd2$longitude[dd2$adm1=="Lakkhhitari"] <- 89.2611
  dd2$latitude[dd2$adm1=="Lakkhhitari"] <- 25.7494
  dd2$season <- dd2$Season
  
  
  ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  dd2$crop <- dd2$`Types of Trial`
  dd2$variety <- NA
  dd2$trial_id <- dd2$`Trial Code`
  
  ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  dd2$planting_date <- NA
  dd2$harvest_date  <- NA
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  dd2$P_fertilizer <- NA
  dd2$K_fertilizer <- NA
  dd2$N_fertilizer <- NA
  dd2$S_fertilizer <- NA
  dd2$lime <- NA
  ## normalize names 
  dd2$fertlizer_type <- NA
  dd2$inoculated <- FALSE
  dd2$inoculant <- NA
  
  ##### in general, add comments to your script if computations are
  ##### based on information gleaned from metadata, a publication, 
  ##### or when they are not immediately obvious for other reasons
  
  ##### Yield #####
  dd2$biomass_total <- dd2$`Total biomass after sun dry (t/ah)`
  
  dd2$yield <- NA
  #what plant part does yield refer to?
  dd2$yield_part <- NA
  
  dd2 <- dd2[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","country","site","adm1","longitude","latitude","season","crop","treatment","trial_id","inoculated","inoculant","biomass_total")]
  
  ###########################################END OF 12 - Biomass samples ######################################################################################################################################################
  
  ############################################14 - Grain Harvest #########################################################################################################################################
  ## process file(s)
  rr3 <- readxl::read_excel(f3,sheet = "14 - Grain Harvest ") |> as.data.frame()      
  
  dd3 <- rr3
  
  
  #### about the data #####
  ## (TRUE/FALSE)
  
  dd3$dataset_id <- dataset_id
  dd3$on_farm <- TRUE
  dd3$is_survey <- FALSE
  dd3$is_experiment <- TRUE
  dd3$irrigated <- TRUE
  ## the treatment code	
  dd3$treatment <- dd3$Tmnt
  
  ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  dd3$country <- "Bangladash"
  dd3$site <- "Rangpur"
  dd3$adm1 <- dd3$Node
  dd3$adm2 <- NA
  dd3$adm3 <- NA
  dd3$elevation <- NA
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
  dd3$longitude[dd3$adm1=="Mohanpur"] <- 86.0106
  dd3$latitude[dd3$adm1=="Mohanpur"] <- 25.7196
  dd3$longitude[dd3$adm1=="Lakkhhitari"] <- 89.2611
  dd3$latitude[dd3$adm1=="Lakkhhitari"] <- 25.7494
  ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  dd3$crop <- dd3$`Types of Trial`
  dd3$trial_id <- dd3$`Trial Code`
  dd3$variety <- NA
  dd3$season <- dd3$Season
  
  ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  dd3$planting_date <- NA
  dd3$harvest_date  <- NA
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  dd3$P_fertilizer <- NA
  dd3$K_fertilizer <- NA
  dd3$N_fertilizer <- NA
  dd3$S_fertilizer <- NA
  dd3$lime <- NA
  ## normalize names 
  dd3$fertlizer_type <- NA
  dd3$inoculated <- FALSE
  dd3$inoculant <- NA
  
  ##### in general, add comments to your script if computations are
  ##### based on information gleaned from metadata, a publication, 
  ##### or when they are not immediately obvious for other reasons
  
  ##### Yield #####
  dd3$biomass_total <- dd3$`Biomass (t/ha)`
  dd3$residue_yield <- dd3$`Straw yield (t/ha)`  
  dd3$yield <- dd3$`Grain yield (t/ha)`
  #what plant part does yield refer to?
  dd3$yield_part <- "grain"
  
  dd3 <- dd3[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","country","site","adm1","longitude","latitude","season","crop","trial_id","treatment","yield_part","biomass_total","residue_yield","yield")]       
  
  ################################################ END OF 14 - Grain Harvest #############################################################################################
  
  
  ################################################ 6 - Fertilizer amounts ###########################################################################################
  
  
  ## process file(s)
  rr4 <- readxl::read_excel(f3,sheet = "6 - Fertilizer amounts ") |> as.data.frame()              
  
  dd4 <- rr4
  
  
  #### about the data #####
  ## (TRUE/FALSE)
  
  dd4$dataset_id <- dataset_id
  dd4$on_farm <- TRUE
  dd4$is_survey <- FALSE
  dd4$is_experiment <- TRUE
  dd4$irrigated <- TRUE
  ## the treatment code	
  dd4$treatment <- dd4$Tmnt 
  dd4$season <- dd4$Season
  
  ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  dd4$country <- "Bangladash"
  dd4$site <- "Rangpur"
  dd4$adm1 <- dd4$Node
  dd4$adm2 <- NA
  dd4$adm3 <- NA
  dd4$elevation <- NA
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
  dd4$longitude[dd4$adm1=="Mohonpur"] <- 86.0106
  dd4$latitude[dd4$adm1=="Mohonpur"] <- 25.7196
  dd4$longitude[dd4$adm1=="Lakkhhatari"] <- 89.2611
  dd4$latitude[dd4$adm1=="Lakkhhatari"] <- 25.7494
  
  ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  dd4$crop <- dd4$`Types of Trial`
  dd4$variety <- NA
  dd4$trial_id <- dd4$`Trial Code`
  
  ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  dd4$planting_date <- NA
  dd4$harvest_date  <- NA
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## abbreviation meanings for fertilizers 
  #MOP-Murate of Potash
  #TSP-Tripple Super Phosphate
  dd4$P_fertilizer <- dd4$`TSP (kg/ha)`
  dd4$K_fertilizer <- dd4$`K    (kg/ha)`
  dd4$N_fertilizer <- dd4$`N    (kg/ha)`
  dd4$S_fertilizer <- dd4$`S   (kg/ha)` 
  dd4$lime <- NA
  dd4$urea <- dd4$`Urea (kg/ha)`
  dd4$TSP <- dd4$`TSP (kg/ha)`
  dd4$MOP <- dd4$`MOP(kg/ha)`
  dd4gypsum <- dd4$`Gypsum  (kg/ha)`
  
 #Fertilizer types
  
  dd4$fertlizer_type_1 <- dd4$ `Product used...12`
  dd4$fertlizer_type_2 <- dd4$`Product used...19`
  dd4$fertlizer_type_3 <- dd4$`Product used...26`
  dd4$fertlizer_type_4 <- dd4$`Product used...33`
  dd4$fertlizer_type_5 <- dd4$`Product used...40`
  
  dd4$inoculated <- FALSE
  dd4$inoculant <- NA
  
  ##### in general, add comments to your script if computations are
  ##### based on information gleaned from metadata, a publication, 
  ##### or when they are not immediately obvious for other reasons
  
  ##### Yield #####
  dd4$biomass_total <- NA
  
  dd4$yield <- NA
  #what plant part does yield refer to?
  dd4$yield_part <- NA 
  
  dd4 <- dd4[,c("dataset_id","on_farm","is_experiment","is_survey","irrigated","season","country","site","adm1","longitude","latitude","crop","trial_id","treatment", "P_fertilizer","N_fertilizer","fertlizer_type_1","fertlizer_type_2","fertlizer_type_3","fertlizer_type_4","fertlizer_type_5","urea","MOP","TSP")]
  
  # all scripts must end like this
  carobiner::write_files(dset, d, path=path)
}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)
