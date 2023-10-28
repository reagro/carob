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
    carob_date="2023-09-19"
  )
  
  ## download and read data 
  path <- "C:/Users/user/Documents/DataAnalysis/carob-BangladashDataset"
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  
  f2 <-  "C:/Users/user/Documents/DataAnalysis/carob-BangladashDataset/data/raw/maize_trial/hdl_11529_10548008/Rabi maize 2016-17-DS&BP-all nodes-Rangpur.xlsx"
  library("readxl")
  r9 <- read_excel(f2)
  r9 <- readxl::read_excel(f2,sheet = "4- Stand counts & Phenology") |> as.data.frame()
  r10 <- readxl::read_excel(f2,sheet = "12 - Biomass samples") |> as.data.frame()
  r11 <- readxl::read_excel(f2,sheet = "14 - Grain Harvest ") |> as.data.frame()
  r12 <- readxl::read_excel(f2,sheet = "6 - Fertilizer amounts ") |> as.data.frame()
 
############################################  4- Stand counts & Phenology ###############################################
  
  ## process file(s)
  r9 <- readxl::read_excel(f2,sheet = "4- Stand counts & Phenology") |> as.data.frame()
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
    
    ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  d9$country <- "Bangladash"
    d9$site <- "Rangpur"
    d9$adm1 <- d9$Node
    d9$adm2 <- NA
    d9$adm3 <- NA
    d9$elevation <- NA
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
    d9$longitude [d9$adm1=="Kolkondo"] <- 89.0160 
    d9$latitude[d9$adm1=="Kolkondo"] <- 25.9318
    d9$longitude [d9$adm1=="Mohanpur"] <- 86.0160 
    d9$latitude[d9$adm1=="Mohanpur"] <- 25.7196
    d9$longitude [d9$adm1=="Lakkhhitari"] <- 89.2611
    d9$latitude[d9$adm1=="Lakkhhitari"] <- 25.7494
    
    ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  d9$crop <- d9$`Types of Trial`
    d9$variety <- d9$Variety 
    d9$trial_id <- d9$`Trial Code`
    
    ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  d9$planting_date <- d9$`Date of seeding (dd-mm-yy)`
  d9$harvest_date  <- d9$`Datw of harvest (dd-mm-yy)`
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  d9$P_fertilizer <- NA
    d9$K_fertilizer <- NA
    d9$N_fertilizer <- NA
    d9$S_fertilizer <- NA
    d9$lime <- NA
    ## normalize names 
    d9$fertlizer_type <- NA
    d9$inoculated <- FALSE
  d9$inoculant <- NA
    
    ##### in general, add comments to your script if computations are
    ##### based on information gleaned from metadata, a publication, 
    ##### or when they are not immediately obvious for other reasons
    
    ##### Yield #####
  d9$season <- d9$Season  
  d9$emergence <- d9$`100% emergence (DAS)`
  d9$flowering <- d9$`50% first flowering (DAS)`
  d9$maturity <- d9$`80% physiological maturity (DAS)`
  d9$harvest <- d9$`Harvesting (DAS)`
  
  d9$biomass_total <- NA
    
    d9$yield <- NA
    #what plant part does yield refer to?
    d9$yield_part <- NA 
    
  d9 <- d9[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","season","country","site","adm1", "longitude","latitude","crop","variety","trial_id","planting_date","harvest_date","emergence","flowering","maturity","harvest")]
    
#######################################END OF 4- Stand counts & Phenology #####################################################################
  
######################################## 12 - Biomass samples #################################################################################
  
  ## process file(s)
  r10 <- readxl::read_excel(f2,sheet = "12 - Biomass samples") |> as.data.frame()
  d10 <- r10
  
  
  #### about the data #####
  ## (TRUE/FALSE)
  
  d10$dataset_id <- dataset_id
  d10$on_farm <- TRUE
    d10$is_survey <- FALSE
    d10$is_experiment <- TRUE
    d10$irrigated <- TRUE
    ## the treatment code	
    d10$treatment <- d10$Tmnt
    
    ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  d10$country <- "Bangladash"
    d10$site <- "Rangpur"
    d10$adm1 <- d10$Node
    d10$adm2 <- NA
    d10$adm3 <- NA
    d10$elevation <- NA
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
    d10$longitude [d10$adm1=="Kolkondo"] <- 89.0160 
    d10$latitude[d10$adm1=="Kolkondo"] <- 25.9318
    d10$longitude [d10$adm1=="Mohanpur"] <- 86.0160 
    d10$latitude[d10$adm1=="Mohanpur"] <- 25.7196
    d10$longitude [d10$adm1=="Lakkhhitari"] <- 89.2611
    d10$latitude[d10$adm1=="Lakkhhitari"] <- 25.7494
    
    ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  d10$crop <- d10$`Types of Trial`
    d10$trial_id <- d10$`Trial Code`
    d10$variety <- NA
    
    ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  d10$planting_date <- NA
  d10$harvest_date  <- NA
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  d10$P_fertilizer <- NA
    d10$K_fertilizer <- NA
    d10$N_fertilizer <- NA
    d10$S_fertilizer <- NA
    d10$lime <- NA
    ## normalize names 
    d10$fertlizer_type <- NA
    d10$inoculated <- FALSE
  d10$inoculant <- NA
    
    ##### in general, add comments to your script if computations are
    ##### based on information gleaned from metadata, a publication, 
    ##### or when they are not immediately obvious for other reasons
    
    ##### Yield #####
  d10$biomass_total <- d10$`Total biomass after sun dry (t/ha)`
    
    d10$yield <- NA
    #what plant part does yield refer to?
    d10$yield_part <- NA
    
    d10 <- d10[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","country","site","adm1","longitude","latitude","treatment","trial_id","crop","biomass_total")]
#########################################END OF 12 - Biomass samples ################################################################
  
############################################## 14 - Grain Harvest ###################################################################
    ## process file(s)
    r11 <- readxl::read_excel(f2,sheet = "14 - Grain Harvest ") |> as.data.frame()
    d11 <- r11
  
  #### about the data #####
  ## (TRUE/FALSE)
  
  d11$dataset_id <- dataset_id
  d11$on_farm <- TRUE
    d11$is_survey <- FALSE
    d11$is_experiment <- TRUE
    d11$irrigated <- TRUE
    ## the treatment code	
    d11$treatment <- d11$Tmnt
    
    ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  d11$country <- "Bangladash"
    d11$site <- "Rangpur"
    d11$adm1 <- d11$Node
    d11$adm2 <- NA
    d11$adm3 <- NA
    d11$elevation <- NA
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
    d11$longitude[d11$adm1=="Kolkondo"] <- 89.7873
    d11$latitude[d11$adm1=="Kolkondo"] <- 25.9318 
    d11$longitude[d11$adm1=="Mohanpur"] <- 86.0106
    d11$latitude[d11$adm1=="Mohanpur"] <- 25.7196
    d11$longitude[d11$adm1=="Lakkhhitari"] <- 89.2611
    d11$latitude[d11$adm1=="Lakkhhitari"] <- 25.7494
    
    ##### Crop #####
  ## normalize variety names

  d11$crop <- d11$`Types of Trial`
    d11$variety <- NA
    d11$trial_id <- d11$`Trial Code`
    
    ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  d11$planting_date <- NA
  d11$harvest_date  <- NA
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  d11$P_fertilizer <- NA
    d11$K_fertilizer <-NA
    d11$N_fertilizer <- NA
    d11$S_fertilizer <- NA
    d11$lime <- NA
    ## normalize names 
    d11$fertlizer_type <- NA
    d11$inoculated <- FALSE
  d11$inoculant <- NA
    d11$season <- d11$Season
    ##### in general, add comments to your script if computations are
    ##### based on information gleaned from metadata, a publication, 
    ##### or when they are not immediately obvious for other reasons
    
    ##### Yield #####
  d11$biomass_total <- d11$`Biomass (t/ha)`
  d11$residue_yield <- d11$`Straw yield (t/ha)`  
  d11$yield <- d11$`Grain yield (t/ha)`
  #what plant part does yield refer to?
  d11$yield_part <- "grain"
  
  d11 <- d11[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","country","site","adm1","longitude","latitude","season","crop","trial_id","treatment","yield_part","biomass_total","residue_yield","yield")]       
  
#########################################END OF 14 - Grain Harvest #############################################################
  
########################################### 6 - Fertilizer amounts#############################################################
    
    ## process file(s)
  r12 <- readxl::read_excel(f2,sheet = "6 - Fertilizer amounts ") |> as.data.frame()
    d12 <- r12
  
  
  #### about the data #####
  ## (TRUE/FALSE)
  
  d12$dataset_id <- dataset_id
  d12$on_farm <- TRUE
    d12$is_survey <- FALSE
    d12$is_experiment <- TRUE
    d12$irrigated <- TRUE
    ## the treatment code	
    d12$treatment <- d12$Tmnt
    
    ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  d12$country <- "Bangladash"
    d12$site <- "Rangpur"
    d12$adm1 <- d12$Node
    d12$adm2 <- NA
    d12$adm3 <- NA
    d12$elevation <- NA
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
    d12$longitude[d12$adm1=="Kolkondo"] <- 89.7873
    d12$latitude[d12$adm1=="Kolkondo"] <- 25.9318 
    d12$longitude[d12$adm1=="Mohanpur"] <- 86.0106
    d12$latitude[d12$adm1=="Mohanpur"] <- 25.7196
    d12$longitude[d12$adm1=="Lakkhhitari"] <- 89.2611
    d12$latitude[d12$adm1=="Lakkhhitari"] <- 25.7494
  
    ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  d12$crop <- d12$`Types of Trial`
    d12$variety <- NA
    d12$trial_id <- d12$`Trial Code`
    
    ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  d12$planting_date <- NA
  d12$harvest_date  <- NA
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  #MOP-Murate of Potash
  #TSP-Tripple Super Phosphate
  d12$P_fertilizer <- d12$`TSP (kg/ha)`
  d12$K_fertilizer <- d12$`K    (kg/ha)`
  d12$N_fertilizer <- d12$`N    (kg/ha)`
  d12$S_fertilizer <- d12$`S   (kg/ha)` 
  d12$lime <- NA
  d12$Zn_fertilizer <- d12$`Zn (kg/ha)`
  
    ## normalize names 
  
  
  d12$fertlizer_type_1 <- d12$ `Product used...12`
  d12$fertlizer_type_2 <- d12$`Product used...19`
  d12$fertlizer_type_3 <- d12$`Product used...26`
  d12$fertlizer_type_4 <- d12$`Product used...33`
  d12$fertlizer_type_5 <- d12$`Product used...40`

    d12$inoculated <- TRUE/FALSE
  d12$inoculant <- NA 
    
    ##### in general, add comments to your script if computations are
    ##### based on information gleaned from metadata, a publication, 
    ##### or when they are not immediately obvious for other reasons
    
    ##### Yield #####
  d12$biomass_total <- NA
    
    d12$yield <- NA
    #what plant part does yield refer to?
    d12$yield_part <- NA
 
    d12 <- d12[,c("dataset_id","is_survey","on_farm","is_experiment","irrigated","country","site","adm1","longitude","latitude","treatment","crop","trial_id","P_fertilizer","N_fertilizer","K_fertilizer","S_fertilizer","Zn_fertilizer","fertlizer_type_1","fertlizer_type_2","fertlizer_type_3","fertlizer_type_4","fertlizer_type_5","Urea (kg/ha)","MOP(kg/ha)","TSP (kg/ha)","Gypsum(Kg/ha)","ZnSO4 (kg/ha)")]
    
#######################################END OF 6 - Fertilizer amounts #################################################################################       
    
    # all scripts must end like this
    carobiner::write_files(dset, d, path=path)
}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)
