

carob_script <- function(path) {
   
   "
	Description:
   Soybean (Glycine max (L.) Merrill.) is one of the most important oil crops of the world which also has tremendous importance as a food legume. The work on soybean aims at providing farmers, both commercial and subsistence, varieties with their preferred attributes to increase yield and income. These include high yield, resistance to deadly diseases, such as soybean rust, and insect pests, early maturity, good seed quality, and resistance to other stresses such as drought and soil acidity. The International Institute of Tropical Agriculture (IITA) is a key player in tropical soybean research and a partner of the Soybean Innovation Lab.
"
   
	uri <- "doi:10.25502/wpce-te77/d"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "soybean_trials"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		uri=uri,
		publication= NA,
		data_citation = "Chigeza, G. (2019). Advanced Variety Trials (AVT), Mozambique- 2018 [dataset]. International Institute of Tropical Agriculture (IITA).
		https://doi.org/10.25502/WPCE-TE77/D",
		data_institutions = "IITA",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-09-21",
		data_type="experiment",
		project=NA 
	)
	
	## download and read data 
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)
	
	bn <- basename(ff)
	
	# read the dataset
	r <- read.csv(ff[bn=="data.csv"])
	
	### process file()
	
	d <- r[,c("ID","Country","City","REP_NO","DESIGNATION","YIELD","BIOM","PLHT","SWT100","DM","HARVEST", "DFFL")]
	 colnames(d) <- c("ID", "country", "location", "rep", "variety", "yield", "dmy_total", "plant_height", "grain_weight", "maturity", "harvest", "flowering")
	
	 # add columns
	d$crop <- "soybean" 
	d$dataset_id <- dataset_id
	d$trial_id <- paste(d$ID, d$location, sep = "-")
	d$ID <- NULL
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	 ## Add fertilizer
	d$N_fertilizer <- NA
	d$K_fertilizer <- NA
	d$P_fertilizer <- NA
	 
	d$longitude[d$location=="NAMPULA"] <- 39.2707752
	d$latitude[d$location=="NAMPULA"] <- -14.966969
	d$location <- carobiner::fix_name(d$location, "title") 
	d$country <- carobiner::fix_name(d$country, "title") 
	d$harvest[d$harvest< 45] <- NA
	d$yield_part <- "seed" 

	d$planting_date <- "2018"
	
	 # all scripts must end like this
	carobiner::write_files(dset, d, path=path)
	
}


