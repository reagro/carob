# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [Durum wheat dataset collected on HAO-DEMETER research station in Thessaloniki, Greece from 2018 to 2020. The experiment was set up to study 2 agronomic factors: 4 cultivars, 2 levels of fertilization. The dataset includes phenology, morphology, leaf area index, biomass, yield components. Funds: Project ANR ARIMNet2 TomorrowS]

"

	uri <- "doi:10.18167/DVN1/KKMZAD"
	group <- "wheat_trials"


	dataset_id <- carobiner::simple_uri(uri)


	
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)

	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
		data_citation="Tsaliki, Eleni; Korpetis, Evangelos; Loison, Romain; Panoras, Ioannis; Milonas, Ioannis, 2024, Experimental dataset on sustainable durum wheat production in Greece 2018 to 2020, https://doi.org/10.18167/DVN1/KKMZAD , CIRAD Dataverse, V1, UNF:6:rkq7qpnvybYvTmyUR6naKQ== [fileUNF]",
		data_institutions = "CIRAD",
		publication= NA,
		project=NA,
		data_type= "experiment",
		carob_contributor= "Hope Mazungunye",
		carob_date="2024-04-09"
	)
	

	f <- ff[basename(ff) == "Data.csv"]
	r <- read.csv(f)
	r <-r[1:18] 


	#d <- data.frame(plant_density=r$"V1", heading=r$"V4", plant_height =r$"V6", dmy_total= r$"V7", yield=r$"v10", grain_weight= r$"V11", variety=r$cultivar,treatment = r$treatment)  
					
	d1 <- data.frame(planting_date=r$"year", plant_density=r$"V1", heading=r$"V4", plant_height =r$"V6", dmy_total= r$"V7", yield=r$"V10", grain_weight= r$"V11", variety=r$cultivar,treatment = r$treatment )

	#changing data type in some variables with bad data type
	d1$plant_density<-as.numeric(d1$plant_density)
	d1$plant_height<-as.numeric(d1$plant_height)
	d1$dmy_total<- as.numeric(d1$dmy_total)
	d1$heading<-as.numeric(d1$heading)
	d1$grain_weight<-as.numeric(d1$grain_weight)
	d1$yield<-as.numeric(d1$yield*1000)
	
	d1$dataset_id <- dataset_id
	d1$on_farm <- TRUE
	d1$is_survey <- FALSE
	d1$irrigated <- FALSE

d1$planting_date<-as.character(d1$planting_date)
	d1$country <- "Greece"

	d1$longitude <- 40.5363
	d1$latitude <- 22.8624


	d1$crop <- "wheat"
	d1$variety <- d1$variety
	d1$yield_part <- "grain"
	trial_id<-"1"


	carobiner::write_files(dset, d1, path=path)
}



