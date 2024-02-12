# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    Drought is a key maize (Zea mays L.) production constraint in sub-Saharan Africa. Fourteen, fifteen, and twenty-five extra-early maturing maize cultivars, with varying Striga resistance and drought and low soil N tolerance, were developed from 1995 to 2000 (Period 1), 2001 to 2006 (Period 2), and 2007 to 2012 (Period 3), respectively. The objectives of this study were to examine yield gains in the cultivars and to investigate inter-trait relationships and yield stability under six drought and 17 rainfed conditions in West Africa from 2013 to 2016. Annual rate of yield increase across cultivars was 0.034 (3.28%) and 0.068 Mg ha−1 (2.25%), whereas yield gains per period were 0.17 and 0.38 Mg ha−1 under drought and rainfed environments, respectively. Yield gains under drought and rainfed environments were related to prolonged flowering period, increased plant and ear heights, improved stalk lodging, and ear and plant aspects, whereas delayed leaf senescence and increased number of ears per plant accompanied yield improvement under drought only. Ear aspect and number of ears per plant were primary contributors to yield and could be used as selection criteria for yield enhancement under drought and rainfed conditions. High-yielding and stable cultivars across all environments based on additive main effects and multiplicative interaction (AMMI) biplot included ‘2004 TZEE-Y Pop STR C4’ and ‘TZEE-W Pop STR BC2 C0’ of Period 2 and ‘2009 TZEE-W STR’, ‘TZEE-Y STR 106’, ‘TZEE-W STR 107’, and ‘TZEE-W DT C0 STR C5’ of Period 3. These cultivars could be commercialized to improve food self-sufficiency in sub-Saharan Africa.
"

	uri <- "doi.org/10.25502/20181101/1228/BB"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
	
		data_institutions = "International Institute of Tropical Agriculture (IITA)",

   		data_type="on-farm experiment", 
		carob_contributor="Shumirai Manzvera",
		# date of first submission to carob
		carob_date="2023-11-23",
		# name(s) of others who made significant improvements
		revised_by=NA
	)

## download and read data 
  path <- "C:/carob"
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=0)
	dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)


	
## process file(s)
	d<- X20181029aao_combd_ba15150_final_ww_data_csv
	d$Country[d$LOC=="IKENNE"]<- "Nigeria"
	d$country[d$LOC=="ZARIA"] <- "Nigeria"
  d$country[d$LOC=="IFE"] <- "Nigeria"
	d$Country[d$LOC=="MOKWA"] <- "Nigeria"
	d$Country[d$LOC=="INA"] <- "Nigeria"
	d$Country[d$LOC=="ANGARADEBOU"] <- "Benin"
	d$country[d$LOC=="MAINA-HARI"] <- "Nigeria"
	d$country[d$LOC=="BAGAUDA"] <-"Nigeria"
	d$country[d$LOC=="FUMESUA"] <- "Ghana"
	d$country[d$LOC=="MANGA"] <- "Nigeria"
	d$country[d$LOC=="MOKWA"] <- "Nigeria"
	d$country[d$LOC=="NYANKPALA"] <- "Ghana"
	
	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- TRUE
## the treatment code	
	d$treatment <- 

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- d$Country
	d$site <- d$LOC
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude[d$country=="Nigeria"] <- 8.6753 	
	d$longitude[d$country=="Ghana"] <- 1.0232
	d$longitude[d$country=="Benin"] <- 2.3158
	d$latitude[d$country=="Nigeria"] <- 9.0820
	d$latitude[d$country=="Ghana"] <- 7.9465
	d$latitude[d$country=="Benin"] <- 9.3077
	  
	  

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "maize"
	d$variety <- d$Pedigree
	d$erot <- d$EROT
	d$pl_ht <- d$PLHT
	d$polshed <- d$POLLEN
	d$sl <- d$SL
	d$rl <- d$RL
	d$p_asp <- d$PASP
	d$e_asp <- d$EASP
	d$e_ht <- d$EHT
	d$dy_sk <- d$DYSK
	d$asi <- d$ASI
	
	
##### Yield #####
	
	d$yield <- d$YIELD
	##Reading excel sheet on drought Stress
	d1<-X20181025aao_combd_ba15150_final_ds_data_csv
	
	d1$dataset_id <- dataset_id
	d1$on_farm <- TRUE
	d1$is_survey <- FALSE
	d1$is_experiment <- TRUE
	d1$irrigated <- FALSE
	 
	##### Crop #####
	## normalize variety names
	## see carobiner::fix_name
	
	d1$crop <- "maize"
	d1$variety <- d1$Pedigree
	d1$erot <- d1$EROT
	d1$pl_ht <- d1$PLHT
	d1$polshed <- d1$POLLEN
	d1$sl <- d1$SL
	d1$rl <- d1$RL
	d1$p_asp <- d1$PASP
	d1$e_asp <- d1$EASP
	d1$e_ht <- d1$EHT
	d1$dy_sk <- d1$DYSK
	d1$asi <- d1$ASI
	
	##### Yield #####
	
	d1$yield <- d1$YIELD
	
	##### Location #####
	## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
	## you can use carobiner::fix_name()
	d1$country <- d1$Country
	d1$site <- d1$Location
	
	d1$longitude <- 8.6753
	d1$latitude <-  9.0820
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

