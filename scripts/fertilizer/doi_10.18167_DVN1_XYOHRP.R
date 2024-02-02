

carob_script <- function(path) {
   
   "
	Description:
	Dataset recording the observation of different variables related to rice growth, weeds, nitrogen content in rice biomass and grains, rice yield, macrofauna and grub countings, and nematodes under 3 different rotations (one with rice followed by groundnut, one with rice followed by a cereal-legume mixture, one with rice followed by a legume mixture) and a rice monocropping during 4 years.in Malagasy highlands Climatic data (monthly) for the 4 years of the trial are also included (rainfall, temperature).
"
   
   uri <-  "doi:10.18167/DVN1/XYOHRP"
   dataset_id <- carobiner::simple_uri(uri)
   group <- "fertilizer" 

   dset <- data.frame(
      dataset_id = dataset_id,
      group=group,
      uri=uri,
      publication= NA,
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
	dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)
   
	r1 <- carobiner::read.excel(ff[basename(ff)=="DonneesDATAVERSE_F1.xlsx"], sheet="DataBiomassYieldN")  
	d1 <- data.frame(
		crop = "rice", 
		dataset_id = dataset_id,
		country = "Madagascar",
		season = as.character(r1$Season),
		crop_rotation = r1$Rotation, 
		yield = 1000 * r1$`Yield (14% moisture content)`,
		weed_biomass = 1000 * r1$TotalWeedBiomass,
		dmy_total = 1000 * r1$RiceBiomassD5,
		rep = as.integer(as.factor(r1$Block))
	)
   
	r2 <- carobiner::read.excel(ff[basename(ff)=="DonneesDATAVERSE_F1.xlsx"], sheet = "DataFertilization", fix=TRUE) 
      
	qom <- r2$Quantity.of.applied.manure.t.ha.1.of.DM  * 10
	d2 <- data.frame(
		season = r2$Season,
		OM_amount = qom,
		N_fertilizer = r2$N.pct * qom, 
		P_fertilizer = r2$P.pct * qom,
		K_fertilizer = r2$K.pct * qom,
		Ca_fertilizer = r2$Ca.pct * qom,
		Mg_fertilizer = r2$Mg.pct * qom
	)
	
	d <- merge(d1, d2, by="season")

	d$crop_rotation[d$crop_rotation=="RG"] <- "rice; groundnut"
	d$crop_rotation[d$crop_rotation=="RR"] <- "rice; rice"
	d$crop_rotation[d$crop_rotation=="RVC"] <- "rice; cereal; legume"
	d$crop_rotation[d$crop_rotation=="RSC"] <- "rice; cereal"

 
	d$planting_date <- paste0("20", substr(d$season, 1, 2))
	d$harvest_date <- paste0("20", substr(d$season, 3, 4))
	   
	d$location <- "Vakinankaratra"
	d$longitude <- 46.836
	d$latitude <- -19.711
	d$trial_id <- "1"
	d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$irrigated <- FALSE
	d$is_survey <- FALSE
	d$planting_date <- NA

message("should also process soil and weather data")

	carobiner::write_files(path, dset, d)
   
}

