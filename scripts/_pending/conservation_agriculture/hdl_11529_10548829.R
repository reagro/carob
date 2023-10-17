# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [Weed development is one of the major constraints to cereal cropping systems in Southern Africa with potential severe crop losses. Understanding weed community responses to different conservation agriculture (CA) components (i.e., no-tillage, NT; crop rotation, R; and mulching, M) and/or their combinations is crucial in Southern Africa where farmers apply different combinations depending on local context. Here, for the first time, we assessed how weed density, community diversity and structure respond to different combinations of CA components [conventional tillage (CT), CT+M, CT+R, CT+M+R, NT, NT+M, NT+R, NT+M+R]. The study was carried out over three seasons at two locations with contrasting soil textures i.e., clayish, and sandy. At the sandy location, across seasons, weed density (number of individuals per unit area) and community diversity (distribution of individuals within the species) were significantly and positively affected by precipitation and not by cropping system. Weed richness (number of species) was affected by the interaction of season and cropping system, with the highest number of weed species being recorded in the NT+M+R system in the seasons with medium to high precipitation. At the clayish location, an opposite pattern was observed, and weed density was lower in seasons with medium-high precipitation than under low precipitation. Weed community diversity was 50 % higher under NT+M than under CT+R, whereas weed species richness decreased with the increase of precipitation. At both locations, the implementation of rotation and mulching either in NT or CT systems resulted in the modification of the structure of weed community with respect to CT and NT alone, and these CA combinations were associated with highest maize grain yield. Overall, eight weed species common to both locations were responsible for most of the community structure differences among cropping systems. Structural equation modelling showed that at the sandy location precipitation did not affect grain yield, but positively affected weed density, diversity, evenness, and richness. By contrast, at the clay location, precipitation positively affected grain yield, but did not modify weed density and evenness, and reduced weed community diversity and richness. At this location, weed density negatively affected grain yield. The differential weed-crop relationship supports the need to find a site-specific equilibrium between the control of weeds and the maintenance of their diversity]

"

	uri <- "doi:11529/10548829"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "conservation_agriculture"
	## dataset level data 
	dset <- data.frame(
		dataset_id =dataset_id,
		group=group,
		project= NA,
		uri=uri,
		data_citation="Mhlanga, Blessing; Pellegrino, Elisa; Thierfelder, Christian; Ercoli, Laura, 2022, Conservation agriculture practices lead to diverse weed communities and higher maize grain yield in Southern Africa, https://hdl.handle.net/11529/10548829, CIMMYT Research Data & Software Repository Network, V1",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "doi.org/10.1016/j.fcr.2022.108724",
		data_institutions = "CIMMYT",
   		data_type="on-farm experiment", 
		carob_contributor="Hope Takudzwa Mazungunye"  
	)

## download and read data 
path <-("C:/carob/wd/data/raw/maize_trials")
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)
	dset$license <- carobiner::get_license(js)


	f <- "C:/carob/scripts/maize_trials/path/to/your/carob/folder/data/raw/maize_trials/doi_11529_10548829"
library("readxl")
	##DATA_DTC_UZ_2019_2020_2021 <- read_excel("path/to/your/carob/folder/data/raw/maize_trials/doi_11529_10548829/DATA DTC UZ - 2019 2020 2021.xlsx", 
	   #                                      +     sheet = "Raw Data")
	r<- read_excel(f)
	r <- DATA_DTC_UZ_2019_2020_2021 

	
## process file(s)

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
	d$treatment <- d$System

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "Zimbabwe"
	d$site <- NA
	d$adm1 <- NA
	d$adm2 <- NA
	d$adm3 <- NA
	d$location<-d$Location
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude[d$location=="DTC"] <- 31.1000
	d$latitude[d$location=="DTC"] <- -17.4833
	d$longitude[d$location=="UZ"] <- 31.0569
	d$latitude[d$location=="UZ"] <- -17.7829
	d$elevation[d$location=="DTC"] <-1400
	d$elevation[d$location=="UZ"]<- 1523

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "maize"
	d$variety <- NA

##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- NA
	d$harvest_date  <- NA

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
   
##### in general, add comments to your script if computations are
##### based on information gleaned from metadata, a publication, 
##### or when they are not immediately obvious for other reasons

##### Yield #####
	d$biomass_total <-d$Biomass

	d$yield <- d$Grain
	#what plant part does yield refer to?
	d$yield_part <- "Grain"
	d <- d[,c("dataset_id", "on_farm", "is_experiment", "longitude", "latitude", "elevation", "Season", "Plot", "Replicate", "treatment", "A.hispidum","crop", "A.hybridus", "B.pilosa", "C.albida", "C.benghalensis", "C.dactylon", "C.esculentus", "C.tridens", "D.stramonium", "D.uncinatum","E.heterophylla", "E.indica", "G.aparine", "G.parviflora", "H.meeusei", "H.zosterifolia", "I.purpurea", "L.matinensis", "M.repens", "Natal red", "O.latifolia", "P.oleracea", "R.cochinchinensis", "R.scabra", "S.alba", "S.asiatica", "T.minuta", "U.panicoides", "V.poskeana", "Other", "M.sativa", "T.rotundifolia", "E.panicoides", "T. fluminensis", "E. arvense", "biomass_total", "yield_part")] 
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)

