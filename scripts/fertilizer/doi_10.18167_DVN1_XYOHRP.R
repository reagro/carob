

carob_script <- function(path) {
   
   "
	Description:
	Dataset recording the observation of different variables related to rice growth, weeds, nitrogen content in rice biomass and grains, rice yield, macrofauna and grub countings, and nematodes under 3 different rotations
	(one with rice followed by groundnut, one with rice followed by a cereal-legume mixture, one with rice followed by a legume mixture) and a rice monocropping during 4 years.in Malagasy highlands Climatic data (monthly) for the 4 years of the trial are also included (rainfall, temperature).
	
"
   
   uri <-  "doi:10.18167/DVN1/XYOHRP"
   dataset_id <- carobiner::simple_uri(uri)
   group <- "fertilizer" 
   ## dataset level data 
   dset <- data.frame(
      dataset_id = dataset_id,
      group=group,
      uri=uri,
      publication= NA,# 
      data_citation ="Ripoche, Aude; Autfray, Patrice; Rabary, Bodo; Randriamanantsoa, Richard; Trap, Jean; Sauvadet, Marie; Letourmy, Philippe; Blanchart, Eric; Randriamandimbisoa Christian, 2021, Ecosystem functions in rainfed rice based short rotations in Malagasy highlands,
      https://doi.org/10.18167/DVN1/XYOHRP",
      data_institutions = "CIRAD",
      carob_contributor="Cedric Ngakou",
      carob_date="2023-10-15",
      data_type="experiment",
      project=NA 
   )
   
   ## download and read data 
   ff <- carobiner::get_data(uri, path, group)
   js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)
   dset$license <- carobiner::get_license(js)
   
   bn <- basename(ff)
   
   # process files
   
   # Yield biomass
   r <- read_excel(ff[bn=="DonneesDATAVERSE_F1.xlsx"],sheet = 3)  
   r$yield <- r$`Yield (14% moisture content)`
   d1 <- r[,c("Season","Rotation","yield","TotalWeedBiomass","RiceBiomassD5")]
   colnames(d1) <- c("season","crop_rotation","yield","weeds_biomass","biomass_total")
   
   ### Fertilizer file
   r1 <- read_excel(ff[bn=="DonneesDATAVERSE_F1.xlsx"],sheet = 2) 
   
   d2 <- r1[,c(2,3,5,6,8,9,10,11)]#
   colnames(d2) <- c("season","Qty_apply","OM","N","P","K","Ca","Mg")#,
   
   d2$OM_applied <-   d2$Qty_apply*d2$OM*10
   d2$N_fertilizer <- d2$Qty_apply*d2$N*10 
   d2$P_fertilizer <- d2$Qty_apply*d2$P*10
   d2$K_fertilizer <- d2$Qty_apply*d2$K*10
   d2$Ca_fertilizer <- d2$Qty_apply*d2$Ca*10
   d2$Mg_fertilizer <- d2$Qty_apply*d2$Mg*10
   d2$Qty_apply <- d2$OM <- d2$N <- d2$P <- d2$K <- d2$Ca <- d2$Mg <- NULL
   
   ## merge d1,d2
   d <- merge(d1,d2,by="season")
   d$crop_rotation[d$crop_rotation=="RG"] <- "groundnut"
   d$crop_rotation[d$crop_rotation=="RR"] <- "rice"
   d$crop_rotation[d$crop_rotation=="RVC"] <- "cereal; legume"
   d$crop_rotation[d$crop_rotation=="RSC"] <- "cereal"
   d$yield <- d$yield*1000 # in kg/ha
   d$weeds_biomass <- d$weeds_biomass*1000 # in kg/ha
   d$biomass_total <- d$biomass_total*1000 # in kg/ha

   # add columns
   
   d$crop <- "rice" 
   d$dataset_id <- dataset_id
   d$country <- "Madagascar"
   d$location <- "Vakinankaratra"
   d$trial_id <- paste(d$country,d$location,sep = "-")
   d$yield_part <- "grain"
   d$on_farm <- TRUE
   d$irrigated <- FALSE
   d$is_survey <- FALSE
   d$planting_date <- NA
   d$planting_date[d$season==1516] <- "2015"
   d$planting_date[d$season==1617] <- "2016"
   d$planting_date[d$season==1718] <- "2017"
   d$planting_date[d$season==1819] <- "2018"
   ### add long and lat coordinate
   d$longitude[d$location=="Vakinankaratra"] <- 46.8355481
   d$latitude[d$location=="Vakinankaratra"] <- -19.7113095
   
   #data type
   d$season <- as.character(d$season)
   d$planting_date <- as.character(d$planting_date)
   # all scripts must end like this
   carobiner::write_files(dset, d, path=path)
   
}

