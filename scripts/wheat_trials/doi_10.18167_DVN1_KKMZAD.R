# R script for "carob"


carob_script <- function(path) {

"Durum wheat dataset collected on HAO-DEMETER research station in Thessaloniki, Greece from 2018 to 2020. The experiment was set up to study 2 agronomic factors: 4 cultivars, 2 levels of fertilization. The dataset includes phenology, morphology, leaf area index, biomass, yield components. Funds: Project ANR ARIMNet2 TomorrowS]"

	uri <- "doi:10.18167/DVN1/KKMZAD"
	group <- "wheat_trials"
	ff  <- carobiner::get_data(uri, path, group)

	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institutions = "CIRAD",
		publication= NA,
		project=NA,
		data_type= "experiment",
		carob_contributor= "Hope Mazungunye",
		carob_date="2024-04-09"
	)
	
	f <- ff[basename(ff) == "Data.csv"]
	r <- read.csv(f)
				
	d <- data.frame(
		planting_date=as.character(r$year), 
		plant_density=r$V1 * 10000, 
		spike_density=r$V2,
		heading_days=r$V4, 
		plant_height =r$V6, 
		dmy_total= r$V8*1000, 
		yield=r$V10*1000, 
		grain_weight= r$V11, 
		variety=r$cultivar,
		LAI=r$V13, # at anthesis
		rep=r$block,
		treatment = r$treatment 
	)

	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	d$country <- "Greece"
	d$longitude <- 22.998
	d$latitude <- 40.538

	d$crop <- "durum wheat"
	d$yield_part <- "grain"
	d$trial_id <- "1"

	carobiner::write_files(dset, d, path=path)
}



