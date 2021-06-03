#################################################################################
# Grain yield and other agronomic traits of international maize trials-Camerron 1989-2002
# Description: This is an international study that contains data on yield and other Agronomic
# traits of maize including borer and striga attacks on maize in Africa. 
# The study was carried out by the International Institute of Tropical Agriculture between 1989
# and 2015 in over thirty African countries. This dataset contains output of the research for Cameroon.

# see: cs-58-6-2399.pdf
#################################################################################

carob_script <- function(path) {


	uri <- "doi:10.25502/20180716/0907/MA"
	dataset_id <- agro::get_simple_URI(uri)
	group <- "international_maize_trials"	
		
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   uri=uri,
	   publication="",
	   contributor="Camila Bonilla",
	   experiment_type="varieties",
	   has_weather=FALSE,
	   has_management=FALSE
	)

	## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, major=2, minor=1, group)
	dset$license <- carobiner::get_license(js)

	f <- ff[tolower(basename(ff))=="international_maize_trial_cameroon_striga.csv"]
	d <- read.csv(f)
	names(d) <- tolower(names(d))
	names(d)[1] <- "id"
	d$id <- NULL
	
	d$start_date <- d$year
	d$start_date[d$year==22] <- 2002
	d$start_date[d$year==24] <- 2004
	d$start_date[d$year==25] <- 2005
	
	d$country <- carobiner::capitalize_words(d$country)
	d$location <- carobiner::capitalize_words(d$location)
	d$location[	d$location == "Cameroon"] <- ""

	d$crop <- 'maize'

	d$on_farm <- FALSE
	d$year <- NULL
	d$aezone <- NULL
	d <- carobiner::change_names(d, c("instinf", "bltin_m", "bltun_m"), c("instin", "blight_in", "blight_un"))

	nms <- names(d)
	inf <- grep("in$", nms, value=TRUE)
	unf <- grep("un$", nms, value=TRUE)
	iu  <- c(inf, unf)
	nm <- nms[!(nms %in% iu)]
	din <- d[, c(nm, inf)]
	dun <- d[, c(nm, unf)]
	names(din) <- gsub("in$", "", names(din))	
	names(dun) <- gsub("un$", "", names(dun))
	din$striga_infected <- "yes"
	dun$striga_infected <- "no"
	# eldun does not exist
	dun$eld <- NA
	
	dd <- rbind(din, dun)
	names(dd) <- gsub("_$", "", names(dd))

	dd <- carobiner::change_names(dd, c("yld", "str_ra1", "str_ra2", "r_l", "s_l"), c("yield", "str_rat1", "str_rat2", "rl", "sl"))
	
	dd$striga_trial <- "yes"
	dd$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, dd, path, dataset_id, group)

}