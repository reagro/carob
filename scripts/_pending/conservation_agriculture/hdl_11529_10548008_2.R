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
    carob_contributor="Mitchelle Njukuya"
  )
  
  ## download and read data 
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  
  f1 <-  "C:/Users/user/Documents/DataAnalysis/carob-BangladashDataset/data/raw/maize_trial/hdl_11529_10548008/Rabi maize 2015-16-DS&BP-all nodes-Rangpur.xlsx"
  library("readxl")
  r5 <- read_excel(f1)
  r5 <- readxl::read_excel(f1,sheet = "4- Stand counts & Phenology") |> as.data.frame()
  r6 <- readxl::read_excel(f1,sheet = "12 - Biomass samples") |> as.data.frame()
  r7 <- readxl::read_excel(f1,sheet = "14 - Grain Harvest ") |> as.data.frame()
  r8 <- readxl::read_excel(f1,sheet = "6 - Fertilizer amounts ") |> as.data.frame()
  
##################################### 4- stand counts & Phenology #####################################################################################################
  r5 <- readxl::read_excel(f1,sheet = "4- Stand counts & Phenology") |> as.data.frame() 
  
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
    
    ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  d5$country <- "Bangladash"
    d5$site <- "Rangpur"
    d5$adm1 <- d5$Node
    d5$adm2 <- NA
    d5$adm3 <- NA
    d5$elevation <- NA
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
  d5$longitude [d5$adm1=="Kolkondo"] <- 89.0160 
    d5$latitude[d5$adm1=="Kolkondo"] <- 25.9318
    d5$longitude [d5$adm1=="Mohanpur"] <- 86.0160 
    d5$latitude[d5$adm1=="Mohanpur"] <- 25.7196
    d5$longitude [d5$adm1=="Lakkhhitari"] <- 89.2611
    d5$latitude[d5$adm1=="Lakkhhitari"] <- 25.7494
    ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  d5$crop <- d5$Crop
    d5$variety <- d5$Variety
    d5$trial_id <- d5$`Trial Code`
    ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  d5$planting_date <- d5$`Date of seeding (dd-mm-yy)`
  d5$harvest_date  <- d5$`Datw of harvest (dd-mm-yy)`
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  d5$P_fertilizer <- NA
    d5$K_fertilizer <-NA
    d5$N_fertilizer <- NA
    d5$S_fertilizer <- NA
    d5$lime <- NA
    ## normalize names 
    d5$fertlizer_type <- NA
    d5$inoculated <- FALSE
  d5$inoculant <- NA
  d5$season <- d5$Season  
  d5$emergence <- d5$`100% emergence (DAS)`
  d5$flowering <- d5$`50% first flowering (DAS)`
  d5$maturity <- d5$`80% physiological maturity (DAS)`
  d5$harvest <- d5$`Harvesting (DAS)`
    
    ##### Yield #####
  d5$biomass_total <- NA
    
    d5$yield <- NA
    #what plant part does yield refer to?
    d5$yield_part <- NA 
    
    d5 <- d5[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","country","site","adm1","longitude","latitude","season","treatment","trial_id","crop","variety","planting_date","harvest_date","inoculated","inoculant","biomass_total","emergence","flowering","maturity","harvest")]

################################################END OF 4- stand counts & Phenology############################################################################################################################################################################################
    
############################################## 12 - Biomass samples ###########################################################################################################################################################################################################
      ## process file(s)
    
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
      d6$country <- "Bangladash"
        d6$site <- "Rangpur"
        d6$adm1 <- d6$Node
        d6$adm2 <- NA
        d6$adm3 <- NA
        d6$elevation <- NA
      ## each site must have corresponding longitude and latitude
      ## see carobiner::geocode
      d6$longitude[d6$adm1=="Kolkondo"] <- 89.7873
        d6$latitude[d6$adm1=="Kolkondo"] <- 25.9318
        d6$longitude[d6$adm1=="Mohanpur"] <- 86.0106
        d6$latitude[d6$adm1=="Mohanpur"] <- 25.7196
        d6$longitude[d6$adm1=="Lakkhhitari"] <- 89.2611
        d6$latitude[d6$adm1=="Lakkhhitari"] <- 25.7494
        d6$season <- d6$Season
        
        
        ##### Crop #####
      ## normalize variety names
      ## see carobiner::fix_name
      d6$crop <- d6$`Types of Trial`
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
      d6$biomass_total <- d6$`Total biomass after sun dry (t/ha)`
        
        d6$yield <- NA
        #what plant part does yield refer to?
        d6$yield_part <- NA
        
        d6 <- d6[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","country","site","adm1","longitude","latitude","season","treatment","trial_id","crop","inoculated","inoculant","biomass_total")]
 
###########################################END OF 12 - Biomass samples ######################################################################################################################################################
        
############################################14 - Grain Harvest #########################################################################################################################################
         ## process file(s)
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
          d7$country <- "Bangladash"
            d7$site <- "Rangpur"
            d7$adm1 <- d7$Node
            d7$adm2 <- NA
            d7$adm3 <- NA
            d7$elevation <- NA
          ## each site must have corresponding longitude and latitude
          ## see carobiner::geocode
          d7$longitude[d7$adm1=="Kolkondo"] <- 89.7873
            d7$latitude[d7$adm1=="Kolkondo"] <- 25.9318 
            d7$longitude[d7$adm1=="Mohanpur"] <- 86.0106
            d7$latitude[d7$adm1=="Mohanpur"] <- 25.7196
            d7$longitude[d7$adm1=="Lakkhhitari"] <- 89.2611
            d7$latitude[d7$adm1=="Lakkhhitari"] <- 25.7494
            ##### Crop #####
          ## normalize variety names
          ## see carobiner::fix_name
          d7$crop <- d7$`Types of Trial`
            d7$trial_id <- d7$`Trial Code`
            d7$variety <- NA
            d7$season <- d7$Season
            
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
            d7$yield <- d7$`Grain yield (t/ha)`
            #what plant part does yield refer to?
            d7$yield_part <- "grain"
        
            d7 <- d7[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","country","site","adm1","longitude","latitude","season","crop","trial_id","treatment","yield_part","biomass_total","residue_yield","yield")]       
            
    ################################################ END OF 14 - Grain Harvest #############################################################################################
            
            
  ################################################ 6 - Fertilizer amounts ###########################################################################################
              
              
              ## process file(s)
            r8 <- readxl::read_excel(f1,sheet = "6 - Fertilizer amounts ") |> as.data.frame()              
    
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
                d8$season <- d8$Season
                
                ##### Location #####
              ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
              ## you can use carobiner::fix_name()
              d8$country <- "Bangladash"
                d8$site <- "Rangpur"
                d8$adm1 <- d8$Node
                d8$adm2 <- NA
                d8$adm3 <- NA
                d8$elevation <- NA
              ## each site must have corresponding longitude and latitude
              ## see carobiner::geocode
                d8$longitude[d8$adm1=="Kolkondo"] <- 89.7873
                d8$latitude[d8$adm1=="Kolkondo"] <- 25.9318 
                d8$longitude[d8$adm1=="Mohanpur"] <- 86.0106
                d8$latitude[d8$adm1=="Mohanpur"] <- 25.7196
                d8$longitude[d8$adm1=="Lakkhhitari"] <- 89.2611
                d8$latitude[d8$adm1=="Lakkhhitari"] <- 25.7494
          
                ##### Crop #####
              ## normalize variety names
              ## see carobiner::fix_name
              d8$crop <- d8$`Types of Trial`
                d8$variety <- NA
                d8$trial_id <- d8$`Trial Code`
                
                ##### Time #####
              ## time can be year (four characters), year-month (7 characters) or date (10 characters).
              ## use 	as.character(as.Date()) for dates to assure the correct format.
              d8$planting_date <- NA
              d8$harvest_date  <- NA
              
              ##### Fertilizers #####
              ## note that we use P and K, not P2O5 and K2O
              d8$P_fertilizer <- d8$`TSP (kg/ha)`
                d8$K_fertilizer <- d8$`K    (kg/ha)`
                d8$N_fertilizer <- d8$`N    (kg/ha)`
                d8$S_fertilizer <- d8$`S   (kg/ha)` 
                d8$lime <- NA
                d8$Zn_fertilizer <- d8$`Zn (kg/ha)`
              
                ## abbreviation meanings for fertilizers 
                #MOP-Murate of Potash
                #TSP-Tripple Super Phosphate
                
                d8$fertlizer_type_1 <- d8$ `Product used...12`
                d8$fertlizer_type_2 <- d8$`Product used...19`
                d8$fertlizer_type_3 <- d8$`Product used...26`
                d8$fertlizer_type_4 <- d8$`Product used...33`
                d8$fertlizer_type_5 <- d8$`Product used...40`
                
                d8$inoculated <- FALSE
              d8$inoculant <- NA
                
                ##### in general, add comments to your script if computations are
                ##### based on information gleaned from metadata, a publication, 
                ##### or when they are not immediately obvious for other reasons
                
                ##### Yield #####
              d8$biomass_total <- NA
                
                d8$yield <- NA
                #what plant part does yield refer to?
                d8$yield_part <- NA 
                
              d8 <- d8[,c("dataset_id","is_survey","on_farm","is_experiment","irrigated","country","site","adm1","longitude","latitude","treatment","crop","trial_id","P_fertilizer","N_fertilizer","K_fertilizer","S_fertilizer","Zn_fertilizer","fertlizer_type_1","fertlizer_type_2","fertlizer_type_3","fertlizer_type_4","fertlizer_type_5","Urea (kg/ha)","MOP(kg/ha)","TSP (kg/ha)","Gypsum(Kg/ha)","ZnSO4 (kg/ha)")]
                
    # all scripts must end like this
    carobiner::write_files(dset, d, path=path)
}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)