# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [This experiments were established with different rates of nitrogen in order to generate a wide range of values for NDVI and grain yield in order to develop a calibration model for the GreenSeeker in Yaqui Valley. (2022-03-28)]"

	uri <- "doi:https://hdl.handle.net/11529/10548652"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"
	
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group= group,
		project=NA,
		uri= uri,
		data_citation="Ortíz-Monasterio Rosas, José Iván, 2022, Maize experiment with increasing rates of nitrogen to develop a calibration for the GreenSeeker in Yaqui Valley, https://hdl.handle.net/11529/10548652, CIMMYT Research Data & Software Repository Network, V4",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "Maize experiment with increasing rates of nitrogen to develop a calibration for the GreenSeeker in Yaqui Valley",
		data_institutions = "CIMMYT",
   		data_type="experiment",
		carob_contributor="Blessing Dzuda"
	)

## download and read data 
	path <- ("C:/Users/user/Documents/MY WORKING DIRECTORY/carob")
	f <- "C:/Users/user/Documents/MY WORKING DIRECTORY/carob/data/raw/maize_trials/doi_hdl.handle.net_11529_10548652"
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=4, minor=0)
	dset$license <- carobiner::get_license(js)
	

  library("readxl")
	AB250 <- read_excel("data/raw/wheat_trials/doi_hdl.handle.net_11529_10548652/AB250.xlsx")
	AB250 <- read_excel("data/raw/maize_trials/doi_hdl.handle.net_11529_10548652/AB250.xlsx", 
	                    +     sheet = "AB250C1")
	AC250 <- read_excel("data/raw/maize_trials/doi_hdl.handle.net_11529_10548652/AC250.xlsx")
	AC250 <- read_excel("data/raw/maize_trials/doi_hdl.handle.net_11529_10548652/AC250.xlsx", 
	                    +     sheet = "AC250C")
	AD250 <- read_excel("data/raw/maize_trials/doi_hdl.handle.net_11529_10548652/AD250.xlsx")
	AD250 <- read_excel("data/raw/maize_trials/doi_hdl.handle.net_11529_10548652/AD250.xlsx", 
	                    +     sheet = "AD250C")
	U250 <- read_excel("data/raw/maize_trials/doi_hdl.handle.net_11529_10548652/U250.xls", 
	                   +     sheet = "Data")
	V250_ <- read_excel("data/raw/maize_trials/doi_hdl.handle.net_11529_10548652/V250 .xls")
	V250_ <- read_excel("data/raw/maize_trials/doi_hdl.handle.net_11529_10548652/V250 .xls", 
	                    +     sheet = "Data-250p1")
	W250 <- read_excel("data/raw/maize_trials/doi_hdl.handle.net_11529_10548652/W250.xls")
	W250 <- read_excel("data/raw/maize_trials/doi_hdl.handle.net_11529_10548652/W250.xls", 
	                   +     sheet = "Data-250 P1")
	X250 <- read_excel("data/raw/maize_trials/doi_hdl.handle.net_11529_10548652/X250.xls")
	X250 <- read_excel("data/raw/maize_trials/doi_hdl.handle.net_11529_10548652/X250.xls", 
	                   +     sheet = "Data 250 C2")
	X250 <- read_excel("data/raw/maize_trials/doi_hdl.handle.net_11529_10548652/X250.xls", 
	                  +     sheet = "Data 250 P1")
	X250 <- read_excel("data/raw/maize_trials/doi_hdl.handle.net_11529_10548652/X250.xls", 
	                   +     sheet = "Data 250 P2")
	Z250_ <- read_excel("data/raw/maize_trials/doi_hdl.handle.net_11529_10548652/Z250 .xlsx")
	Z250_ <- read_excel("data/raw/maize_trials/doi_hdl.handle.net_11529_10548652/Z250 .xlsx", 
	                    +     sheet = "Data 250 C2")
	Z250_ <- read_excel("data/raw/maize_trials/doi_hdl.handle.net_11529_10548652/Z250 .xlsx", 
	                    +     sheet = "Data 250 P1")
	Z250_ <- read_excel("data/raw/maize_trials/doi_hdl.handle.net_11529_10548652/Z250 .xlsx", 
	                    +     sheet = "Data 250 P2")
## process file(s)

## use a subset
	d <- AB250
	d2 <- AB250
	d3 <- AC250
	d4 <- AC250
	d5 <- AD250
	d6 <- AD250
	d7 <- U250
	d8 <- V250_
	d9 <- V250_
	d10 <- W250
	d11 <- W250
	d12 <- X250
	d13 <- X250
	d14 <- X250
	d15 <- X250
	d16 <- Z250_ 
	d17 <- Z250_ 
	d18 <- Z250_
	d19 <- Z250_
	
	
	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d2$dataset_id <- dataset_id
	d3$dataset_id <- dataset_id
	d4$dataset_id <- dataset_id
	d5$dataset_id <- dataset_id
	d6$dataset_id <- dataset_id
	d7$dataset_id <- dataset_id
	d8$dataset_id <- dataset_id
	d9$dataset_id <- dataset_id
	d10$dataset_id <- dataset_id
	d11$dataset_id <- dataset_id
	d12$dataset_id <- dataset_id
	d13$dataset_id <- dataset_id
	d14$dataset_id <- dataset_id
	d15$dataset_id <- dataset_id
	d16$dataset_id <- dataset_id
	d17$dataset_id <- dataset_id
	d18$dataset_id <- dataset_id
	d19$dataset_id <- dataset_id
	
	
	d$on_farm <- FALSE
	d2$on_farm <- FALSE
	d3$on_farm <- FALSE
	d4$on_farm <- FALSE
	d5$on_farm <- FALSE
	d6$on_farm <- FALSE
	d7$on_farm <- FALSE
	d8$on_farm <- FALSE
	d9$on_farm <- FALSE
	d10$on_farm <- FALSE
	d11$on_farm <- FALSE
	d12$on_farm <- FALSE
	d13$on_farm <- FALSE
	d14$on_farm <- FALSE
	d15$on_farm <- FALSE
	d16$on_farm <- FALSE
	d17$on_farm <- FALSE
	d18$on_farm <- FALSE
	d19$on_farm <- FALSE
	
	
	d$is_survey <- FALSE
	d2$is_survey <- FALSE
	d3$is_survey <- FALSE
	d4$is_survey <- FALSE
	d5$is_survey <- FALSE
	d6$is_survey <- FALSE
	d7$is_survey <- FALSE
	d8$is_survey <- FALSE
	d9$is_survey <- FALSE
	d10$is_survey <- FALSE
	d11$is_survey <- FALSE
	d12$is_survey <- FALSE
	d13$is_survey <- FALSE
	d14$is_survey <- FALSE
	d15$is_survey <- FALSE
	d16$is_survey <- FALSE
	d17$is_survey <- FALSE
	d18$is_survey <- FALSE
	d19$is_survey <- FALSE
	
	
	d$is_experiment <- TRUE
	d2$is_experiment <- TRUE
	d3$is_experiment <- TRUE
	d4$is_experiment <- TRUE
	d5$is_experiment <- TRUE
	d6$is_experiment <- TRUE
	d7$is_experiment <- TRUE
	d8$is_experiment <- TRUE
	d9$is_experiment <- TRUE
	d10$is_experiment <- TRUE
	d11$is_experiment <- TRUE
	d12$is_experiment <- TRUE
	d13$is_experiment <- TRUE
	d14$is_experiment <- TRUE
	d15$is_experiment <- TRUE
	d16$is_experiment <- TRUE
	d17$is_experiment <- TRUE
	d18$is_experiment <- TRUE
	d19$is_experiment <- TRUE
	
	d$irrigated <- TRUE
	d2$irrigated <- TRUE
	d3$irrigated <- TRUE
	d4$irrigated <- TRUE
	d5$irrigated <- TRUE
	d6$irrigated <- TRUE
	d7$irrigated <- TRUE
	d8$irrigated <- TRUE
  d9$irrigated <- TRUE
  d10$irrigated <- TRUE
  d11$irrigated <- TRUE
  d12$irrigated <- TRUE
  d13$irrigated <- TRUE
  d14$irrigated <- TRUE
  d15$irrigated <- TRUE
  d16$irrigated <- TRUE
  d17$irrigated <- TRUE
  d18$irrigated <- TRUE
  d19$irrigated <- TRUE
  
## the treatment code	
	d$treatment <- d$Trt
	d2$treatment <- d2$TRT
	d3$treatment <- d3$Trt
	d4$treatment <- d4$Trt
	d5$treatment <- d5$TRT
	d6$treatment <- d6$TRT
	d7$treatment <- d7$TRT
	d8$treatment <- d8$`Nitrogen Treatment`
	d9$treatment <- d9$TRT
	d10$treatment <- d10$Trt
	d11$treatment <- d11$Trt
	d12$treatment <- d12$TRT
	d13$treatment <- d13$TRT
	d14$treatment <- d14$TRT
	d15$treatment <- d15$TRT
	d16$treatment <- d16$TRT
	d17$treatment <- d17$TRT
	d18$treatment <- d18$TRT
	d19$treatment <- d19$TRT
	
	d$rep <- d$Rep
	d2$rep <- d2$Rep
	d3$rep <- d3$Rep
	d4$rep <- d4$Rep
	d5$rep <- d5$Rep
	d6$rep <- d6$REP
	d7$rep <- d7$Rep
	d8$rep <- d8$Rep
	d9$rep <- d9$Rep
	d10$rep <- d10$Rep
	d11$rep <- d11$Rep
	d12$rep <- d12$Rep
	d13$rep <- d13$Rep
	d14$rep <- d14$Rep
	d15$rep <- d15$Rep
	d16$rep <- d16$Rep
	d17$rep <- d17$Rep
	d18$rep <- d18$Rep
	d19$rep <- d19$Rep
	
	
	d$N_fertilizer <- d$`N at Planting`
	d2$N_fertilizer <- d2$`N at Planting`
	d3$N_fertilizer <- d3$`N at Planting (kg/ha)`
	d4$N_fertiliser <- d4$`N at Planting (kg/ha)`
	d5$N_fertiliser <- d5$`N at Planting`
	d6$N_fertiliser <- d6$`N at Planting (kg/ha)`
	d7$N_fertiliser <- d7$`N at Planting`
	d8$N_fertiliser <- d8$`Preplant or Planting N`
	d9$N_fertiliser <- d9$`N at Planting`
	d10$N_fertiliser <- d10$`Total N`
	d11$N_fertiliser <- d11$`Total N`
	d12$N_fertiliser <- d12$`Total N TRT`
	d13$N_fertiliser <- d13$`Total N`
	d14$N_fertiliser <- d14$`Total N TRT`
	d15$N_fertiliser <- d15$`Total N TRT`
  d16$N_fertiliser <- d16$`N at Planting`
  d17$N_fertiliser <- d17$`N at Planting`
  d18$N_fertiliser <- d18$`N at Planting`
  d19$N_fertiliser <- d19$`N at Planting`
	
	d$yield <- d$`Yield at 14 % humidity`
	d2$yield <- d2$`Yield at 14 % humidity`
	d3$yield <- d3$`Yield at 14% hum`
	d4$yield <- d4$`Yield at 14 % humidity`
	d5$yield <- d5$`Yield at`
	d6$yield <- d6$`Yield at 14% hum`
	d7$yield <- d7$`Yield at 14% hum`
	d8$yield <- d8$`Yield at14% hum`
	d9$yield <- d9$`Yield at 14 % humidity`
	d10$yield <- d10$`Yield at14% hum`
	d11$yield <- d11$`Yield at14% hum`
	d12$yield <- d12$`Yield at 14 % humidity`
	d13$yield <- d13$`Calculated Yield (14%)`
	d14$yield <- d14$`Yield at 14 % humidity`
	d15$yield <- d15$`Yield at 14 % humidity`
	d16$yield <- d16$`Yield at`
	d17$yield <- d17$`Yield at`
	d18$yield <- d18$`yield at`
	d19$yield <- d19$`Yield at 14 % humidity`
	
	d$plant_density <- d2$`Harvest Pop.`
	d2$plant_density <- d2$`Harvest Pop.`
	d3$plant_density <- d3$`Harvest Pop.`
	d4$plant_density <- d4$`Harvest Pop.`
	d5$plant_density <- d5$`Harvest Pop.`
	d6$plant_density <- d6$`Harvest Pop.`
	d7$plant_density <- d7$`Harvest Pop.`
	d8$plant_density <- d8$`Harvest Pop.`
	d9$plant_density <- d9$`Harvest Pop.`
	d10$plant_density <- d10$`Harvest Pop.`
	d11$plant_density <- d11$`Harvest Pop.`
	d12$plant_density <- d12$`Harvest Pop.`
	d13$plant_density <- d13$`Harvest Pop.`
	d14$plant_density <- d14$`Harvest Pop.`
	d15$plant_density <- d15$`Harvest Pop.`
	d16$plant_density <- d16$`Harvest Pop.`
	d17$plant_density <- d17$`Harvest Pop.`
	d18$plant_density <- d18$`Harvest Pop.`
	d19$plant_density <- d19$`Harvest Pop.`
	
	d$biomass_total <- d$BIOMASS
	d2$biomass_total <- d2$BIOMASS
	d3$biomass_total <- d3$BIOMASS
	d4$biomass_total <- d4$BIOMASS
	d5$biomass_total <- d5$BIOMASS
	d6$biomass_total <- d6$BIOMASS
  d7$biomass_total <- d7$BIOMASS
  d8$biomass_total <- d8$Biomass
  d9$biomass_total <- d9$BIOMASS
  d10$biomass_total <- d10$Biomass
  d11$biomass_total <- d11$Biomass
	d12$biomass_total <- d12$BIOMASS
	d13$biomass_total <- d13$Biomass
	d14$biomass_total <- d14$BIOMASS
	d15$biomass_total <- d15$BIOMASS
	d16$biomass_total <- d16$BIOMASS
	d17$biomass_total <- d17$BIOMASS
	d18$biomass_total <- d18$BIOMASS
	d19$biomass_total <- d19$BIOMASS
  
##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "Mexico"
	d2$country <- "Mexico"
	d3$country <- "Mexico"
	d4$country <- "Mexico"
	d5$country <- "Mexico"
	d6$country <- "Mexico"
	d7$country <- "Mexico"
	d8$country <- "Mexico"
	d9$country <- "Mexico"
	d10$country <- "Mexico"
	d11$country <- "Mexico"
	d12$country <- "Mexico"
	d13$country <- "Mexico"
	d14$country <- "Mexico"
	d15$country <- "Mexico"
	d16$country <- "Mexico"
	d17$country <- "Mexico"
	d18$country <- "Mexico"
	d19$country <- "Mexico"
	
	d$site <- "Yaqui Valley"
	d2$site <- "Yaqui Valley"
	d3$site <- "Yaqui Valley"
	d4$site <- "Yaqui Valley"
	d5$site <- "Yaqui Valley"
	d6$site <- "Yaqui Valley"
	d7$site <- "Yaqui Valley"
	d8$site <- "Yaqui Valley"
	d9$site <- "Yaqui Valley"
	d10$site <- "Yaqui Valley"
	d11$site <- "Yaqui Valley"
	d12$site <- "Yaqui Valley"
	d13$site <- "Yaqui Valley"
	d14$site <- "Yaqui Valley"
	d15$site <- "Yaqui Valley"
	d16$site <- "Yaqui Valley"
	d17$site <- "Yaqui Valley"
	d18$site <- "Yaqui Valley"
	d19$site <- "Yaqui Valley"
	
	d$adm1 <- "Sonora"
	d2$adm1 <- "Sonora"
	d3$adm1 <- "Sonora"
	d4$adm1 <- "Sonora"
	d5$adm1 <- "Sonora"
	d6$adm1 <- "Sonora"
	d7$adm1 <- "Sonora"
	d8$adm1 <- "Sonora"
	d9$adm1 <- "Sonora"
	d10$adm1 <- "Sonora"
	d11$adm1 <- "Sonora"
	d12$adm1 <- "Sonora"
	d13$adm1 <- "Sonora"
	d14$adm1 <- "Sonora"
	d15$adm1 <- "Sonora"
	d16$adm1 <- "Sonora"
	d17$adm1 <- "Sonora"
	d18$adm1 <- "Sonora"
	d19$adm1 <- "Sonora"
	
	d$adm2 <- NA
	d$adm3 <- NA
	
	d$elevation <- 3
	d2$elevation <- 3
	d3$elevation <- 3
	d4$elevation <- 3
	d5$elevation <- 3
	d6$elevation <- 3
	d7$elevation <- 3
	d8$elevation <- 3
	d9$elevation <- 3
	d10$elevation <- 3
	d11$elevation <- 3
	d12$elevation <- 3
	d13$elevation <- 3
	d14$elevation <- 3
	d15$elevation <- 3
	d16$elevation <- 3
	d17$elevation <- 3
	d18$elevation <- 3
	d19$elevation <- 3
	
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- -110.38863
	d2$longitude <- -110.38863
	d3$longitude <- -110.38863
	d4$longitude <- -110.38863
	d5$longitude <- -110.38863
	d6$longitude <- -110.38863
	d7$longitude <- -110.38863
	d8$longitude <- -110.38863
	d9$longitude <- -110.38863
	d10$longitude <- -110.38863
	d11$longitude <- -110.38863
	d12$longitude <- -110.38863
	d13$longitude <- -110.38863
	d14$longitude <- -110.38863
	d15$longitude <- -110.38863
	d16$longitude <- -110.38863
	d17$longitude <- -110.38863
	d18$longitude <- -110.38863
	d19$longitude <- -110.38863
	
	d$latitude <- 27.36915
	d2$latitude <- 27.36915
	d3$latitude <- 27.36915
	d4$latitude <- 27.36915
	d5$latitude <- 27.36915
	d6$latitude <- 27.36915
	d7$latitude <- 27.36915
	d8$latitude <- 27.36915
	d9$latitude <- 27.36915
	d10$latitude <- 27.36915
	d11$latitude <- 27.36915
	d12$latitude <- 27.36915
	d13$latitude <- 27.36915
	d14$latitude <- 27.36915
	d15$latitude <- 27.36915
	d16$latitude <- 27.36915
	d17$latitude <- 27.36915
	d18$latitude <- 27.36915
	d19$latitude <- 27.36915

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "Maize"
	d2$crop <- "Maize"
	d3$crop <- "Maize"
	d4$crop <- "Maize"
	d5$crop <- "Maize"
	d6$crop <- "Maize"
	d7$crop <- "Maize"
	d8$crop <- "Maize"
	d9$crop <- "Maize"
	d10$crop <- "Maize"
	d11$crop <- "Maize"
	d12$crop <- "Maize"
	d13$crop <- "Maize"
	d14$crop <- "Maize"
	d15$crop <- "Maize"
	d16$crop <- "Maize"
	d17$crop <- "Maize"
	d18$crop <- "Maize"
	d19$crop <- "Maize"
	
	d$variety <- 

##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	
	d$planting_date <- 2013
	d2$planting_date <- 2013
	d3$planting_date <- 2014
	d4$planting_date <- 2014
	d5$planting_date <- 2015
	d6$planting_date <- 2015
	d7$planting_date <- 2009
	d8$planting_date <- 2007
	d9$planting_date <- 2007
	d10$planting_date <- 2008
	d11$planting_date <- 2008
	d12$planting_date <- 2009
	d13$planting_date <- 2009
	d14$planting_date <- 2009
	d15$planting_date <- 2009
	d16$planting_date <- 2011
	d17$planting_date <- 2011
	d18$planting_date <- 2011
	d19$planting_date <- 2011
	
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
   d$fertlizer_type <- FALSE 
	d2$fertlizer_type <- FALSE
	d3$fertlizer_type <- FALSE
	d4$fetilizer_type <- FALSE
	d5$fetilizer_type <- FALSE
	d6$fetilizer_type <- FALSE
	d7$fetilizer_type <- FALSE
	d8$fetilizer_type <- FALSE
	d9$fetilizer_type <- FALSE
	d10$fetilizer_type <- FALSE
	d11$fetilizer_type <- FALSE
	d12$fetilizer_type <- FALSE
	d13$fetilizer_type <- FALSE
	d14$fetilizer_type <- FALSE
	d15$fetilizer_type <- FALSE
	d16$fetilizer_type <- FALSE
	d17$fetilizer_type <- FALSE
	d18$fetilizer_type <- FALSE
	d19$fetilizer_type <- FALSE
	
	 d$inoculated <- FALSE
   d2$inoculated <- FALSE
   d3$inoculated <- FALSE
   d4$inoculated <- FALSE
   d5$inoculated <- FALSE
   d6$inoculated <- FALSE
   d7$inoculated <- FALSE
   d8$inoculated <- FALSE
   d9$inoculated <- FALSE
   d10$inoculated <- FALSE
   d11$inoculated <- FALSE
   d12$inoculated <- FALSE
   d13$inoculated <- FALSE
   d14$inoculated <- FALSE
   d15$inoculated <- FALSE
   d16$inoculated <- FALSE
   d17$inoculated <- FALSE
   d18$inoculated <- FALSE
   d19$inoculated <- FALSE
   
   d$inoculant <- FALSE
   d2$inoculant <- FALSE
   d3$inoculant <- FALSE
   d4$inoculant <- FALSE
   d5$inoculant <- FALSE
   d6$inoculant <- FALSE
   d7$inoculant <- FALSE
   d8$inoculant <- FALSE
   d9$inoculant <- FALSE
   d10$inoculant <- FALSE
   d11$inoculant <- FALSE
   d12$inoculant <- FALSE
   d13$inoculant <- FALSE
   d14$inoculant <- FALSE
   d15$inoculant <- FALSE
   d16$inoculant <- FALSE
   d17$inoculant <- FALSE
   d18$inoculant <- FALSE
   d19$inoculant <- FALSE
   
##### in general, add comments to your script if computations are
##### based on information gleaned from metadata, a publication, 
##### or when they are not immediately obvious for other reasons

#what plant part does yield refer to?
	d$yield_part <- "grain"
	d2$yield_part <- "grain"
	d3$yield_part <- "grain"
	d4$yield_part <- "grain"
	d5$yield_part <- "grain"
	d6$yield_part <- "grain"
	d7$yield_part <- "grain"
	d8$yield_part <- "grain"
	d9$yield_part <- "grain"
	d10$yield_part <- "grain"
	d11$yield_part <- "grain"
	d12$yield_part <- "grain"
	d13$yield_part <- "grain"
	d14$yield_part <- "grain"
	d15$yield_part <- "grain"
	d16$yield_part <- "grain"
	d17$yield_part <- "grain"
	d18$yield_part <- "grain"
	d19$yield_part <- "grain"
	
	d<- d[, c("country","adm1","site","latitude","longitude","elevation","planting_date","crop","rep","treatment","plant_density","N_fertilizer","yield","yield_part","biomass_total")]
	d2<- d2[, c("country","adm1","site","latitude","longitude","elevation","planting_date","crop","rep","treatment","plant_density","N_fertilizer","yield","yield_part","biomass_total")]
	d3<- d3[, c("country","adm1","site","latitude","longitude","elevation","planting_date","crop","rep","treatment","plant_density","N_fertilizer","yield","yield_part","biomass_total")]
	d4<- d4[, c("country","adm1","site","latitude","longitude","elevation","planting_date","crop","rep","treatment","plant_density","N_fertiliser","yield","yield_part","biomass_total")]
	d5<- d5[, c("country","adm1","site","latitude","longitude","elevation","planting_date","crop","rep","treatment","plant_density","N_fertiliser","yield","yield_part","biomass_total")]
	d6<- d6[, c("country","adm1","site","latitude","longitude","elevation","planting_date","crop","rep","treatment","plant_density","N_fertiliser","yield","yield_part","biomass_total")]
	d7<- d7[, c("country","adm1","site","latitude","longitude","elevation","planting_date","crop","rep","treatment","plant_density","N_fertiliser","yield","yield_part","biomass_total")]
	d8<- d8[, c("country","adm1","site","latitude","longitude","elevation","planting_date","crop","rep","treatment","plant_density","N_fertiliser","yield","yield_part","biomass_total")]
	d9<- d9[, c("country","adm1","site","latitude","longitude","elevation","planting_date","crop","rep","treatment","plant_density","N_fertiliser","yield","yield_part","biomass_total")]
	d10<- d10[, c("country","adm1","site","latitude","longitude","elevation","planting_date","crop","rep","treatment","plant_density","N_fertiliser","yield","yield_part","biomass_total")]
	d11<- d11[, c("country","adm1","site","latitude","longitude","elevation","planting_date","crop","rep","treatment","plant_density","N_fertiliser","yield","yield_part","biomass_total")]
	d12<- d12[, c("country","adm1","site","latitude","longitude","elevation","planting_date","crop","rep","treatment","plant_density","N_fertiliser","yield","yield_part","biomass_total")]
	d13<- d13[, c("country","adm1","site","latitude","longitude","elevation","planting_date","crop","rep","treatment","plant_density","N_fertiliser","yield","yield_part","biomass_total")]
	d14<- d14[, c("country","adm1","site","latitude","longitude","elevation","planting_date","crop","rep","treatment","plant_density","N_fertiliser","yield","yield_part","biomass_total")]
	d15<- d15[, c("country","adm1","site","latitude","longitude","elevation","planting_date","crop","rep","treatment","plant_density","N_fertiliser","yield","yield_part","biomass_total")]
	d16<- d16[, c("country","adm1","site","latitude","longitude","elevation","planting_date","crop","rep","treatment","plant_density","N_fertiliser","yield","yield_part","biomass_total")]
	d17<- d17[, c("country","adm1","site","latitude","longitude","elevation","planting_date","crop","rep","treatment","plant_density","N_fertiliser","yield","yield_part","biomass_total")]
	d18<- d18[, c("country","adm1","site","latitude","longitude","elevation","planting_date","crop","rep","treatment","plant_density","N_fertiliser","yield","yield_part","biomass_total")]
	d19<- d19[, c("country","adm1","site","latitude","longitude","elevation","planting_date","crop","rep","treatment","plant_density","N_fertiliser","yield","yield_part","biomass_total")]
	
	
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)

