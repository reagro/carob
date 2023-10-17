# R script for "carob"

## ISSUES
# ....

## should also be written to fertilizer

carob_script <- function(path) {

"Description:
Farmer participatory on-farm trials with CA technologies comparing with farmers’ practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory validation trials were conducted comparing to existing practices and to find out suitable and more profitable crop production practices, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers.
"

	uri <- "hdl:11529/10548007"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "conservation_agriculture"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation='Gathala, Mahesh K. and Tiwari, Thakur P. and Islam, Saiful and Khan, A.S.M. and Anwar, Mazharul and Hossain, Illias and Siddique, Nur-E-A and Hossain, Shakhawat and Rahman, Mohammad Atiqur,2018,"Rabi (winter) crops-all nodes-Validation trials-Rajshahi-Bangladesh", https://hdl.handle.net/11529/10548007, CIMMYT Research Data & Software Repository Network, V2',
		
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "11529/10548007",
		data_institutions = "CIMMYT",
   		data_type="on-farm experiment",
		carob_contributor="Mitchelle Njukuya" 
	  )

## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)

	f <- ff[basename(ff) == "Wheat 2014-15-DS&BP-all nodes Rajshahi.xlsx"]

	r <- carobiner::read.excel(f,sheet = "6 - Fertilizer amounts ", skip=3 )
	r <- r[-1,]
	r1 <- carobiner::read.excel(f,sheet = "12 - Biomass samples" )
	r2 <- carobiner::read.excel(f,sheet = "14 - Grain Harvest " ) 
	r3 <- carobiner::read.excel(f,sheet = "4- Stand counts & Phenology" ) 
## process file(s)

## use a subset
###########	6 - Fertilizer amounts ###########
	
#### about the data #####
## (TRUE/FALSE)
	d <- data.frame(dataset_id = rep(dataset_id, nrow(r)))
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- TRUE
## the treatment code	
	d$treatment <- d$Tmnt

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "Bangladash"
	d$site <- NA
	d$adm1 <- NA
	d$adm2 <- "Rajshahi"
	d$adm3 <- NA
	d$elevation <- NA
	d$location <- r$Node
	
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude[d$location=="Dharampur"] <- 20.5401
	d$latitude[d$location=="Dharampur"] <- 73.1792
	d$longitude[d$location=="Baduria"] <- 22.7286
	d$latitude[d$location=="Baduria"] <- 88.8023
	d$longitude[d$location=="Laxmipur"] <- 22.9447
	d$latitude[d$location=="Laxmipur"] <- 90.8282
	d$longitude[d$location=="Nabinagar"] <- 24.6068
	d$latitude[d$location=="Nabinagar"] <- 84.1275

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- r$`Types of Trial`
## RH variety is available.
	d$variety <- NA

##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- 2014
	d$harvest_date  <- 2015

##### Fertilizers #####
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051

## RH these fields are not present. It would also be wrong for P and K (we need the elements, not the product)
   d$P_fertilizer <- r$`TSP (kg/ha)`
   d$K_fertilizer <- r$`MOP(kg/ha)`
   d$N_fertilizer <- r$`N    (kg/ha)`
## RH not correct, overwriting the data
   d$Zn_fertilizer <- r$`ZnSO4 (kg/ha)...51`
   d$Zn_fertilizer <- r$`ZnSO4 (kg/ha)...57`
   d$gypsum <- r$`Gypsum (kg/ha)`
   d$S_fertilizer <- 0
   d$lime <- 0
## normalize names 
## RH not correct, overwriting the data
   d$fertilizer_type <- d$`Product used...12`
   d$fertilizer_type <- d$`Product used...19`
   d$fertilizer_type <- d$`Product used...26`
   d$fertilizer_type <- d$`Product used...33`
   d$fertilizer_type <- d$`Product used...40`
   d$inoculated <- FALSE
   d$inoculant <- NA
"inoculated","longitude","latitude")]
   
##### in general, add comments to your script if computations are
##### based on information gleaned from metadata, a publication, 
##### or when they are not immediately obvious for other reasons

##### Yield #####
	d$biomass_total <- NA

	d$yield <- NA
	#what plant part does yield refer to?
	d$yield_part <- NA
	
########### END OF 6 - Fertilizer amounts############

########### 12 - Biomass samples ####################
	## use a subset
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
	  ##### Location #####
	## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
	## you can use carobiner::fix_name()
	d1$country <- "Bangladash"
	  d1$site <- NA
	  d1$adm1 <- NA
	  d1$adm2 <- NA
	  d1$adm3 <- NA
	  d1$elevation <- NA
	  d1$location <- d1$Node
	  
	## each site must have corresponding longitude and latitude
	## see carobiner::geocode
	  d1$longitude[d1$location=="Dharampur"] <- 20.5401
	  d1$latitude[d1$location=="Dharampur"] <- 73.1792
	  d1$longitude[d1$location=="Baduria"] <- 22.7286
	  d1$latitude[d1$location=="Baduria"] <- 88.8023
	  d1$longitude[d1$location=="Laxmipur"] <- 22.9447
	  d1$latitude[d1$location=="Laxmipur"] <- 90.8282
	  d1$longitude[d1$location=="Nabinagar"] <- 24.6068
	  d1$latitude[d1$location=="Nabinagar"] <- 84.1275
	  
	  ##### Crop #####
	## normalize variety names
	## see carobiner::fix_name
	d1$crop <- d1$`Types of Trial`
	  d1$variety <- NA
	  
	  ##### Time #####
	## time can be year (four characters), year-month (7 characters) or date (10 characters).
	## use 	as.character(as.Date()) for dates to assure the correct format.
	d1$planting_date <- NA
	d1$harvest_date  <- NA
	
	##### Fertilizers #####
	## note that we use P and K, not P2O5 and K2O
	## P <- P2O5 / 2.29
	## K <- K2O / 1.2051
	d1$P_fertilizer <- NA
	  d1$K_fertilizer <-NA
	  d1$N_fertilizer <-NA 
	  d1$S_fertilizer <- NA
	  d1$lime <- NA
	  ## normalize names 
	  d1$fertlizer_type <- NA
	  d1$inoculated <- FALSE
	d1$inoculant <- NA
	  
	  ##### in general, add comments to your script if computations are
	  ##### based on information gleaned from metadata, a publication, 
	  ##### or when they are not immediately obvious for other reasons
	  
	  ##### Yield #####
	d1$biomass_total <- d1$`Total biomass after sun dry (t/ah)`
	d1$biomass_leaves <- d1$`above ground sundried weight-1`
	
	  d1$yield <- d1$`Total biomass at harvest (t/ah)`
	  #what plant part does yield refer to?
	  d1$yield_part <- NA
	  d1<-d1[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","treatment","country","location","longitude","latitude","crop","inoculated","inoculant","biomass_leaves","biomass_total","yield")]

	  ###########END OF 12 - Biomass samples ####################	  
	
	  ############ 14 - Grain Harvest ##########################
	  
	  ## use a subset
	  r2 <- readxl::read_excel(f,sheet = "14 - Grain Harvest " ) |> as.data.frame()
	  t
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
	  d2$country <- "Bangladash"
	    d2$site <- NA
	    d2$adm1 <- NA
	    d2$adm2 <- NA
	    d2$adm3 <- NA
	    d2$elevation <- NA
	    d2$location <- d2$Node
	    d2$season <- d2$Season
	  ## each site must have corresponding longitude and latitude
	  ## see carobiner::geocode
	    d2$longitude[d2$location=="Dharampur"] <- 20.5401
	    d2$latitude[d2$location=="Dharampur"] <- 73.1792
	    d2$longitude[d2$location=="Baduria"] <- 22.7286
	    d2$latitude[d2$location=="Baduria"] <- 88.8023
	    d2$longitude[d2$location=="Laxmipur"] <- 22.9447
	    d2$latitude[d2$location=="Laxmipur"] <- 90.8282
	    d2$longitude[d2$location=="Nabinagar"] <- 24.6068
	    d2$latitude[d2$location=="Nabinagar"] <- 84.1275
	  
	    
	    ##### Crop #####
	  ## normalize variety names
	  ## see carobiner::fix_name
	  d2$crop <- d2$`Types of Trial`
	    d2$variety <- NA
	    
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
	    d2$S_fertilizer <-NA 
	    d2$lime <- NA
	    ## normalize names 
	    d2$fertlizer_type <- NA
	    d2$inoculated <- FALSE
	  d2$inoculant <- NA
	    
	    ##### in general, add comments to your script if computations are
	    ##### based on information gleaned from metadata, a publication, 
	    ##### or when they are not immediately obvious for other reasons
	    
	    ##### Yield #####
	  d2$biomass_total <- d2$`Biomass (t/ha)`
	  d2$residue_yield <- d2$`Straw yield (t/ha)`
	    
	    d2$yield <- d2$ `Grain yield (t/ha)` 
	    #what plant part does yield refer to?
	    d2$yield_part <- d2$`No. of kernels/ sample (for kernel weight)`
	 
	      d2<-d2[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","treatment","country","site","location","season","longitude","latitude","crop","inoculated","inoculant","biomass_total","residue_yield","yield","yield_part")] 
###################END OF 14 - Grain Harvest ############################	    
	
	 ################ 4- Stand counts & Phenology #########################
	    
	    ## use a subset
	    
	    r3 <- readxl::read_excel(f,sheet = "4- Stand counts & Phenology" ) |> as.data.frame() 
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
	    d3$country <- "Bangladash"
	      d3$site <- NA
	      d3$adm1 <- NA
	      d3$adm2 <- NA
	      d3$adm3 <- NA
	      d3$elevation <- NA
	      d3$location <- d3$Node
	    ## each site must have corresponding longitude and latitude
	    ## see carobiner::geocode
	      d3$longitude[d3$location=="Dharampur"] <- 20.5401
	      d3$latitude[d3$location=="Dharampur"] <- 73.1792
	      d3$longitude[d3$location=="Baduria"] <- 22.7286
	      d3$latitude[d3$location=="Baduria"] <- 88.8023
	      d3$longitude[d3$location=="Laxmipur"] <- 22.9447
	      d3$latitude[d3$location=="Laxmipur"] <- 90.8282
	      d3$longitude[d3$location=="Nabinagar"] <- 24.6068
	      d3$latitude[d3$location=="Nabinagar"] <- 84.1275
	    
	      
	      ##### Crop #####
	    ## normalize variety names
	    ## see carobiner::fix_name
	    d3$crop <- d3$`Types of Trial`
	      d3$variety <- d3$Variety
	      
	      ##### Time #####
	    ## time can be year (four characters), year-month (7 characters) or date (10 characters).
	    ## use 	as.character(as.Date()) for dates to assure the correct format.
	    d3$planting_date <- d3$`Date of seeding (dd-mm-yy)`
	    d3$harvest_date  <- d3$`Datw of harvest (dd-mm-yy)`
	    d3$emergence <- d3$`100% emergence (DAS)`
	    d3$flowering <- d3$`50% first flowering (DAS)`
	    d3$maturity <- d3$`80% physiological maturity (DAS)`
	    d3$harvest <- d3$`Harvesting (DAS)`
	    d3$season <- d3$Season
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
	    d3$biomass_total <- NA
	      
	      d3$yield <- NA
	      #what plant part does yield refer to?
	      d3$yield_part <- NA
	      d3<- d3[,c("dataset_id" ,"on_farm","is_survey" ,"is_experiment","irrigated","treatment","country","site","location","longitude","latitude","crop","variety","planting_date","harvest_date","emergence","flowering","maturity","harvest","season","biomass_total")]

	      #################### Wheat 2015-16-DS&BP-all nodes Rajshahi##########
	      f1 <- "C:/Users/user/Documents/DataAnalysis/carob/data/raw/wheat_trials/hdl_11529_10548007/Wheat 2015-16-DS&BP-all nodes Rajshahi.xlsx"
	      library("readxl")
	      r4 <- read_xlsx(f1) 
	      r5 <- readxl::read_excel(f1,sheet = "6 - Fertilizer amounts " ) |> as.data.frame()
	      r6 <- readxl::read_excel(f1,sheet = "4- Stand counts & Phenology" ) |> as.data.frame()
	      r7 <- readxl::read_excel(f1,sheet = "12 - Biomass samples" ) |> as.data.frame()
	 
	      r8 <- readxl::read_excel(f1,sheet = "14 - Grain Harvest " ) |> as.data.frame()
	      
####################### f1->6 - Fertilizer amounts###############################
	      r5 <- readxl::read_excel(f1,sheet = "6 - Fertilizer amounts " ) |> as.data.frame()	      
	      
	      
	      ## use a subset
	      
	      d4<- r5
	      
	      #### about the data #####
	      ## (TRUE/FALSE)
	      
	      d4$dataset_id <- dataset_id
	      d4$on_farm <- TRUE
	        d4$is_survey <- FALSE
	        d4$is_experiment <- TRUE
	        d4$irrigated <- TRUE
	        ## the treatment code	
	        d4$treatment <- d4$Tmnt
	        
	        ##### Location #####
	      ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
	      ## you can use carobiner::fix_name()
	      d4$country <- "Bangladash"
	        d4$site <- NA
	        d4$adm1 <- "Rangpur"
	        d4$adm2 <- d4$Node
	        d4$adm3 <- NA
	        d4$elevation <- NA
	      ## each site must have corresponding longitude and latitude
	      ## see carobiner::geocode
	        d4$longitude[d4$adm2=="Baduria"] <- 22.7286
	        d4$latitude[d4$adm2=="Baduria"] <- 88.8023
	        d4$longitude[d4$adm2=="Nabinagar"] <- 24.6068
	        d4$latitude[d4$adm2=="Nabinagar"] <- 84.1275
	        d4$longitude[d4$adm2=="Dharmapur"] <- 20.5401
	        d4$latitude[d4$adm2=="Dharmapur"] <- 73.1792
	      
	        
	        ##### Crop #####
	      ## normalize variety names
	      ## see carobiner::fix_name
	      d4$crop <- d4$`Types of Trial`
	        d4$variety <- NA
	        
	        ##### Time #####
	      ## time can be year (four characters), year-month (7 characters) or date (10 characters).
	      ## use 	as.character(as.Date()) for dates to assure the correct format.
	      d4$planting_date <- NA
	      d4$harvest_date  <- NA
	      
	      ##### Fertilizers #####
	      ## note that we use P and K, not P2O5 and K2O
	      ## P <- P2O5 / 2.29
	      ## K <- K2O / 1.2051
	      d4$P_fertilizer <- d4$`TSP (kg/ha)`
	        d4$K_fertilizer <-d4$`MOP(kg/ha)`
	        d4$N_fertilizer <-d4$`Fert Grade N...13` 
	        d4$S_fertilizer <- NA
	        d4$lime <- NA
	        d4$gypsum <- d4$`Gypsum(Kg/ha)...56`
	        d4$Zn_fertilizer <- d4$`ZnSO4 (kg/ha)...57`
	        ## normalize names 
	        d4$fertlizer_type <- d4$`Product used...12`
	          d4$fertlizer_type <- d4$`Product used...19`
	          d4$fertlizer_type <-d4$`Product used...26`
	          d4$fertlizer_type <-d4$`Product used...40`
	        d4$inoculated <- FALSE
	      d4$inoculant <- NA
	        
	        ##### in general, add comments to your script if computations are
	        ##### based on information gleaned from metadata, a publication, 
	        ##### or when they are not immediately obvious for other reasons
	        
	        ##### Yield #####
	      d4$biomass_total <- NA
	        
	        d4$yield <- NA
	        #what plant part does yield refer to?
	        d4$yield_part <- NA
	        
	        d4 <- d4[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","country","adm1","adm2","treatment","longitude","latitude","crop","P_fertilizer","K_fertilizer","N_fertilizer","Zn_fertilizer","gypsum","fertlizer_type")]
	        
######################## END OF f1->6 - Fertilizer amounts#########################################################
	        
#######################	f1 -> 4- Stand counts & Phenology###########################
	        r6 <- readxl::read_excel(f1,sheet = "4- Stand counts & Phenology" ) |> as.data.frame()	        
	        ## use a subset
	        d5 <- r6
	        
	        
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
	          d5$site <- NA
	          d5$adm1 <- "Rangpur"
	          d5$adm2 <- d5$Node
	          d5$adm3 <- NA
	          d5$elevation <- NA
	        ## each site must have corresponding longitude and latitude
	        ## see carobiner::geocode
	          d5$longitude[d5$adm2=="Baduria"] <- 22.7286
	          d5$latitude[d5$adm2=="Baduria"] <- 88.8023
	          d5$longitude[d5$adm2=="Nabinagar"] <- 24.6068
	          d5$latitude[d5$adm2=="Nabinagar"] <- 84.1275
	          d5$longitude[d5$adm2=="Dharmapur"] <- 20.5401
	          d5$latitude[d5$adm2=="Dharmapur"] <- 73.1792 
	          
	          ##### Crop #####
	        ## normalize variety names
	        ## see carobiner::fix_name
	        d5$crop <- d5$`Type of trial`
	          d5$variety <- d5$Variety
	          
	          ##### Time #####
	        ## time can be year (four characters), year-month (7 characters) or date (10 characters).
	        ## use 	as.character(as.Date()) for dates to assure the correct format.

	        d5$planting_date <- d5$`Date of seeding (dd-mm-yy)`
	        d5$harvest_date  <- d5$`Datw of harvest (dd-mm-yy)`
	        d5$emergence <- d5$`100% emergence (DAS)`
	        d5$flowering <- d5$`50% first flowering (DAS)`
	        d5$maturity <- d5$`80% physiological maturity (DAS)`
	        d5$harvest <- d5$`Harvesting (DAS)`
	        d5$season <- d5$Season
	        d5$row_spacing <- d5$`Row spacing (cm)`
	        ##### Fertilizers #####
	        ## note that we use P and K, not P2O5 and K2O
	        ## P <- P2O5 / 2.29
	        ## K <- K2O / 1.2051
	        d5$P_fertilizer <- NA
	          d5$K_fertilizer <- NA
	          d5$N_fertilizer <- NA
	          d5$S_fertilizer <- NA
	          d5$lime <- NA
	          ## normalize names 
	          d5$fertlizer_type <- NA
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

########################### END OF 	f1 -> 4- Stand counts & Phenology#########################################
	          
###################### f1 -> 12 - Biomass samples ################################################# 
	          
	          r7 <- readxl::read_excel(f1,sheet = "12 - Biomass samples" ) |> as.data.frame()
	          ## use a subset
	          d6 <- r7
	          
	          
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
	            d6$site <- NA
	            d6$adm1 <- "Rangpur"
	            d6$adm2 <- d6$Node
	            d6$adm3 <- NA
	            d6$elevation <- NA
	          ## each site must have corresponding longitude and latitude
	          ## see carobiner::geocode
	            d6$longitude[d6$adm2=="Baduria"] <- 22.7286
	            d6$latitude[d6$adm2=="Baduria"] <- 88.8023
	            d6$longitude[d6$adm2=="Nabinagar"] <- 24.6068
	            d6$latitude[d6$adm2=="Nabinagar"] <- 84.1275
	            d6$longitude[d6$adm2=="Dharmapur"] <- 20.5401
	            d6$latitude[d6$adm2=="Dharmapur"] <- 73.1792
	          
	            
	            ##### Crop #####
	          ## normalize variety names
	          ## see carobiner::fix_name
	          d6$crop <- d6$`Types of Trial`
	            d6$variety <- NA
	            
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
	          d6$biomass_total <- d6$`Total biomass at harvest (t/ah)`
	            
	            d6$yield <- d6$`Total biomass after sun dry (t/ah)`
	            #what plant part does yield refer to?
	            d6$yield_part <- NA 
	           
	            d6 <-d6[,c("dataset_id","on_farm","is_survey","is_experiment","treatment","country","adm1","adm2","longitude","latitude","crop","biomass_total","yield")] 
############################END OF f1 -> 12 - Biomass samples ##################################################################################################################
	            
########################### f1 -> 14 - Grain Harvest #############################################################################################
	            
	            r8 <- readxl::read_excel(f1,sheet = "14 - Grain Harvest " ) |> as.data.frame()
	            
	            ## use a subset
	            d7 <- r8
	            
	            
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
	              d7$site <- NA
	              d7$adm1 <- "Rangpur"
	              d7$adm2 <- d7$Node
	              d7$adm3 <- NA
	              d7$elevation <- NA
	            ## each site must have corresponding longitude and latitude
	            ## see carobiner::geocode
	              d7$longitude[d7$adm2=="Baduria"] <- 22.7286
	              d7$latitude[d7$adm2=="Baduria"] <- 88.8023
	              d7$longitude[d7$adm2=="Nabinagar"] <- 24.6068
	              d7$latitude[d7$adm2=="Nabinagar"] <- 84.1275
	              d7$longitude[d7$adm2=="Dharmapur"] <- 20.5401
	              d7$latitude[d7$adm2=="Dharmapur"] <- 73.1792
	            
	              
	              ##### Crop #####
	            ## normalize variety names
	            ## see carobiner::fix_name
	            d7$crop <- d7$`Types of Trial`
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
	              
	              d7$yield <- d7$`Grain yield (t/ha)`
	              d7$residual_yield <- d7$`Straw yield (t/ha)`
	              #what plant part does yield refer to?
	              d7$yield_part <- NA 
	              
	              d7 <- d7[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","treatment","country","adm1","adm2","longitude","latitude","crop","biomass_total","yield","residual_yield")]
	        
	            # all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

