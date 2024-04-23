

carob_script <- function(path) {
   
"Soybean (Glycine max (L.) Merrill.) is one of the most important oil crops of the world which also has tremendous importance as a food legume. The work on soybean aims at providing farmers, both commercial and subsistence, varieties with their preferred attributes to increase yield and income. These include high yield, resistance to deadly diseases, such as soybean rust, and insect pests, early maturity, good seed quality, and resistance to other stresses such as drought and soil acidity. The International Institute of Tropical Agriculture (IITA) is a key player in tropical soybean research and a partner of the Soybean Innovation Lab.
"
   
	uri <- "doi:10.25502/wpce-te77/d"
	group <- "soybean_trials"

	ff <- carobiner::get_data(uri, path, group)

	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=2, minor=1),
		publication= NA,
		#data_citation = "Chigeza, G. (2019). Advanced Variety Trials (AVT), Mozambique- 2018 [dataset]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/WPCE-TE77/D",
		data_institutions = "IITA",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-09-21",
		data_type="experiment",
		project=NA 
	)
	
	
	# read the dataset
	r <- read.csv(ff[basename(ff)=="data.csv"])
	
	### process file()
	
	d <- r[,c("ID","Country", "City", "REP_NO", "DESIGNATION", "YIELD", "BIOM", "PLHT", "SWT100", "DM","HARVEST", "DFFL")]
	 colnames(d) <- c("ID", "country", "location", "rep", "variety", "yield", "dmy_total", "plant_height", "grain_weight", "maturity_days", "harvest_days", "flowering_days")

	d$location <- carobiner::fix_name(d$location, "title") 
	d$country <- carobiner::fix_name(d$country, "title") 
	
	 # add columns
	d$crop <- "soybean" 
	d$yield_part <- "seed" 

	
	d$trial_id <- paste(d$ID, d$adm1, sep = "-")
	d$ID <- NULL
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	# Nampula
	d$longitude <- 39.271
	d$latitude <- -14.967

	## Planting and harvest date are taken from metadata 
	d$planting_date <- "2018-01-20"
	d$harvest_date <- "2018-06-18"
	
	# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
	
}


