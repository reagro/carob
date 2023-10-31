# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description: On-farm demonstration plots were set in Zambia to demonstrate the effects of conservation agriculture (CA) technologies as compared to the traditional farmers practice (ploughing with a mouldboard plough). The CA treatments included basins (BA), ripping (RI) and direct seeding with a direct seeder (DS) and direct seeding with a jab planter (JP). Also superimposed to the treatments are rotations and intercropping of maize with a grain legume (either soyabean or cowpea) and these are compared with continuous maize planting. The study is carried out in various communities of Zambia. Thus, the data set presents yields for maize and the legumes from these sites over 9 seasons (2006-2015). (2016-12-08)

    

"

	uri <- "hdl:11529/10825"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "conservation_agriculture"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Thierfelder, Christian, 2016, 'Facilitating the widespread adoption of conservation agriculture in maize-based systems in Zambia', https://hdl.handle.net/11529/10825, CIMMYT Research Data & Software Repository Network, V3",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= NA,
		data_institutions = "CIMYTT",
   		data_type="on-farm experiment",
		carob_contributor="Mitchelle Njukuya"  ,
		carob_date="2023-08-23"
	)

## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=3, minor=1)
	dset$license <- carobiner::get_license(js)


	f <- "C:Users/user/Documents/DataAnalysis/carob-ZambiaDataset/data/raw/maize_trials/hdl_11529_10825/Summary Zambia On-farm Demonstration 2006-2015.xls"
library("readxl")
	r <- read_excel(f)
	r <- readxl::read_excel(f,sheet = "Zambia all sites all maize") |> as.data.frame()
	r1 <- readxl::read_excel(f,sheet = "Zambia all legume all years") |> as.data.frame()
	
## process file(s)
############################### Zambia all sites all maize#####################
## use a subset
	d <- r

	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- FALSE
## the treatment code	
	d$treatment <- d$Tmnt.

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "Zambia"
	d$site <- NA 
	d$adm1 <- d$District
	d$adm2 <- d$Village
	d$adm3 <- NA
	d$elevation <- NA
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude[d$adm1=="Monze"] <- 27.4733
	d$latitude[d$adm1=="Monze"] <- 16.2803
	d$longitude[d$adm1=="Chipata"] <- 32.6447
	d$latitude[d$adm1=="Chipata"] <- 13.6445
	d$longitude[d$adm1=="Lundazi"] <- 33.1745
	d$latitude[d$adm1=="Lundazi"] <- 12.2849
	d$longitude[d$adm2=="Waya"] <- 27.9833
	d$latitude[d$adm2=="Waya"] <- 14.3667
	d$longitude[d$adm2=="Chibombo"] <- 28.0889
	d$latitude[d$adm2=="Chibombo"] <- 14.6554
	d$longitude[d$adm2=="Chanje"] <- 32.8515
	d$latitude[d$adm2=="Chanje"] <- 13.3812
	d$longitude[d$adm2=="Kawalala"] <- 31.8080
	d$latitude[d$adm2=="Kawalala"] <- -14.1996
	d$longitude[d$adm2=="Mafumbwe"] <- 32.1747
	d$latitude[d$adm2=="Mafumbwe"] <- -14.3067
	d$longitude[d$adm2=="Mtaya"] <- 32.0500
	d$latitude[d$adm2=="Mtaya"] <- -14.3067
	d$longitude[d$adm2=="Waya Camp"] <- 27.9833 
	d$latitude[d$adm2=="Waya Camp"] <- 14.3667

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- d$`Crop grown`
	d$variety <- NA

##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- NA
	d$harvest_date  <- d$`Harvest Year`

##### Fertilizers #####
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- NA
   d$K_fertilizer <- NA
   d$N_fertilizer <- NA 
   d$S_fertilizer <- NA
   d$lime <- NA
## normalize names 
   d$fertlizer_type <- NA
   d$inoculated <- FALSE
   d$inoculant <- NA
   
##### in general, add comments to your script if computations are
##### based on information gleaned from metadata, a publication, 
##### or when they are not immediately obvious for other reasons

##### Yield #####
	d$biomass_total <- NA
  d$plant_density <- d$`Final stand (pl/ha)`
	d$yield <- d$`Grain yield (kg/ha)`
	d$residue_yield <- d$`Stalk yield (kg/ha)`
	#what plant part does yield refer to?
	d$yield_part <- "grain"
	
 d <- d[,c("dataset_id","is_experiment","on_farm","irrigated","is_survey","inoculated","inoculant","country","adm1","adm2","longitude","latitude","crop","treatment","plant_density","harvest_date","yield_part","residue_yield","yield")]	
########################END OF Zambia all sites all maize##########################################
	
###########################Zambia all legume all years#############################################

		 ## use a subset
	r1 <- readxl::read_excel(f,sheet = "Zambia all legume all years") |> as.data.frame()	
	d1 <- r1
  
  
  #### about the data #####
  ## (TRUE/FALSE)
  
  d1$dataset_id <- dataset_id
  d1$on_farm <- TRUE
    d1$is_survey <- FALSE
    d1$is_experiment <- TRUE
    d1$irrigated <- FALSE
    ## the treatment code	
    d1$treatment <- d1$Tmnt
    
    ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  d1$country <- "Zambia"
    d1$site <- NA
    d1$adm1 <- d1$District
    d1$adm2 <- d1$Village
    d1$adm3 <- NA
    d1$elevation <- NA
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
    d1$longitude[d1$adm1=="Monze"] <- 27.4733
    d1$latitude[d1$adm1=="Monze"] <- 16.2803
    d1$longitude[d1$adm1=="Chipata"] <- 32.6447
    d1$latitude[d1$adm1=="Chipata"] <- 13.6445
    d1$longitude[d1$adm1=="Lundazi"] <- 33.1745
    d1$latitude[d1$adm1=="Lundazi"] <- 12.2849
    d1$longitude[d1$adm2=="Waya"] <- 27.9833
    d1$latitude[d1$adm2=="Waya"] <- 14.3667
    d1$longitude[d1$adm2=="Chibombo"] <- 28.0889
    d1$latitude[d1$adm2=="Chibombo"] <- 14.6554
    d1$longitude[d1$adm2=="Chanje"] <- 32.8515
    d1$latitude[d1$adm2=="Chanje"] <- 13.3812
    d1$longitude[d1$adm2=="Kawalala"] <- 31.8080
    d1$latitude[d1$adm2=="Kawalala"] <- -14.1996
    d1$longitude[d1$adm2=="Mafumbwe"] <- 32.1747
    d1$latitude[d1$adm2=="Mafumbwe"] <- -14.3067
    d1$longitude[d1$adm2=="Mtaya"] <- 32.0500
      d1$latitude[d1$adm2=="Mtaya"] <- -14.1067
      d1$longitude[d1$adm2=="Waya Camp"] <- 27.9833 
    d1$latitude[d1$adm2=="Waya Camp"] <- 14.3667
  
    
    ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  d1$crop <- d1$`Crop grown`
    d1$variety <- NA
    
    ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  d1$planting_date <- NA
  d1$harvest_date  <- d1$`Harvest Year`
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  d1$P_fertilizer <- NA
    d1$K_fertilizer <- NA
    d1$N_fertilizer <- NA
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
  d1$biomass_total <- NA
  d1$plant_density <- d1$`Final stand (pl/ha)`
   d1$residue_yield <- d1$`Stalk yield (kg/ha)` 
    d1$yield <- d1$`Grain yield (kg/ha)`
    #what plant part does yield refer to?
    d1$yield_part <- "grain" 
    
    d1 <- d1[,c("dataset_id","is_experiment","on_farm","is_survey","irrigated","inoculated","inoculant","country","adm1","adm2","longitude","latitude","crop","treatment","plant_density","harvest_date","yield_part","residue_yield","yield")]
    
  
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)

