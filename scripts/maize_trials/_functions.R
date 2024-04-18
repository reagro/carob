
## to do
## extract fertilizer use (and more?) from "description"
## standardize variable names specific to this dataset


intmztrial_striga <- function(ff, sf=NULL) {

	doit <- function(sf, striga=FALSE, borer=FALSE) {
		d  <- read.csv(sf)
		names(d) <- tolower(names(d))
		d$x <- NULL
		names(d)[1] <- "id"
		d$id <- NULL
		d$gwt <- NULL
		if (!is.null(d$str_rat1)) {
			d$str_ra1_in <- NULL
			d$str_ra1_un <- NULL
		}
		if (!is.null(d$str_rat2)) {
			d$str_ra2_in <- NULL
			d$str_ra2_un <- NULL
		}

		if (!is.null(d$str_co1)) {
			d$str_co1_in <- NULL
			d$str_co1_un <- NULL
		}
		if (!is.null(d$str_co2)) {
			d$str_co2_in <- NULL
			d$str_co2_un <- NULL
		}
		if (!is.null(d$str_co3)) {
			d$str_co3_in <- NULL
			d$str_co3_un <- NULL
		}
		
		d <- carobiner::change_names(d, 
		 c("trl_titl", "entry", "entryno", "instinf", "bltin_m", "bltun_m", "x1000gwt", "x_1000gwt", "cobdamco", "cobdamrt", "borerdmrat", "sbdamat", "nitr_rat"), 
		 c("trial_name", "variety", "variety_code", "instin", "blight_in", "blight_un", "grain_weight", "grain_weight", "cob_dam_co", "cob_dam_rt", "borer_dam_rat", "sb_dam_rat", "grain_N")
		 , must_have=FALSE)

		d$planting_date <- d$year
		d$planting_date[d$year==199] <- 1990 #??
		d$planting_date[d$year==2] <- 2000
		d$planting_date[d$year==21] <- 2001
		d$planting_date[d$year==22] <- 2002
		d$planting_date[d$year==23] <- 2003
		d$planting_date[d$year==24] <- 2004
		d$planting_date[d$year==25] <- 2005
		d$planting_date[d$year==26] <- 2006
		d$planting_date[d$year==27] <- 2007
		d$planting_date[d$year==28] <- 2008
		d$planting_date[d$year==29] <- 2009
		d$planting_date[d$year==211] <- 2011
		d$planting_date[d$year==212] <- 2012
		d$planting_date[d$year==213] <- 2013
		d$planting_date[d$year==214] <- 2014
		d$planting_date[d$year==215] <- 2015

		d$planting_date <- as.character(d$planting_date)
		
		d$country <- carobiner::fix_name(d$country, "title")
		d$location <- carobiner::fix_name(d$location, "title")

		d$crop <- "maize"
		d$season <- "first"
		d$on_farm <- FALSE
		d$year <- NULL
		d$aezone <- NULL
		if (striga) {
			nms <- names(d)
			inf <- grep("in$", nms, value=TRUE)
			unf <- grep("un$", nms, value=TRUE)
			iu  <- c(inf, unf)
			nm <- nms[!(nms %in% iu)]
			din <- d[, c(nm, inf)]
			dun <- d[, c(nm, unf)]
			names(din) <- gsub("in$", "", names(din))	
			names(dun) <- gsub("un$", "", names(dun))
			din$striga_infected <- TRUE
			dun$striga_infected <- FALSE
			# eldun does not exist
			dun$eld <- NA
			d <- rbind(din, dun)
			names(d) <- gsub("_$", "", names(d))
			d$striga_trial <- TRUE
		} else {
			d$striga_trial <- FALSE
			d$striga_infected <- FALSE
		}

		d <- carobiner::change_names(d, 
		 c("str_ra1", "str_ra2", "yld", "r_l", "s_l", "plst", "eld", "borer_dm_rat", "deadheart", "yieldin1", "yieldun2"), 
		 c("str_rat1", "str_rat2", "yield", "rl", "sl", "pl_st", "eldana", "borer_dam_rat", "dead_heart", "yieldin", "yield2"), must_have=FALSE)

		if (!is.null(d$e_dam_rat)) {
			if (all(is.na(d$e_dam_rat))) {
				d$ear_dam_rat <- d$e_dam_rat
			}
			d$e_dam_rat <- NULL
		}

		if (!is.null(d$variety_code)) {
			d$variety_code <- as.character(d$variety_code)
		}
		if (!is.null(d$yield2)) {
			d$yield2 <- as.numeric(d$yield2)	
			dd <- d[!is.na(d$yield2), ]
			if (nrow(dd) > 0) {
				dd$season <- "second"
				if (!is.null(dd$yieldin2)) {
					dd$yieldin <- dd$yieldin2
					d$yieldin2 <- NA
				}
				d <- rbind(d, dd)
				d$yieldin2 <- NULL
			}
			d$yield2 <- NULL
		}

		if (!is.null(d$yieldin2)) {
			if (!all(is.na(d$yieldin2))) {
				message("     yieldin2. this should not happen")
			}
			d$yieldin2 <- NULL
		}
		if (!is.null(d$sesamia)) {
			if (!all(is.na(d$sesamia))) {
				message("     sesamia. this should not happen")
			}
			d$sesamia <- NULL
		}

		if (is.null(d$grain_weight)) {
			d$grain_weight <- as.numeric(NA)
		} else {
			d$grain_weight <- as.numeric(d$grain_weight)	
		}
		qv <- c("yield", "pl_ht", "e_ht", "asi", "dy_sk", "p_asp", "p_harv", "e_asp", "husk", "rl", "sl", "e_rot", "rust", "anthesis", "moist", "pl_st")
		for (v in qv) {
			if (!is.null(d[[v]])) {
				d[[v]][d[[v]] == "."] <- NA
				d[[v]][d[[v]] == "S"] <- NA
				d[[v]] <- as.numeric(d[[v]])			
			}
		}
		
		d$yield_part <- "grain"
		d$striga_trial <- striga
		d$borer_trial <- striga
		d$variety <- trimws(d$variety)
		d$variety[d$variety == ""] <- NA
		d$description <- as.character(d$description)
		d$description[d$description==""] <- "not provided"

		if (inherits(d$latitude, "character")) {
			d$latitude <- trimws(d$latitude)
			d$latitude <- gsub(" ", "", d$latitude)
			d$latitude[d$latitude == ""] <- NA
			d$latitude <- as.numeric(d$latitude)
		}
		if (inherits(d$longitude, "character")) {
			d$longitude <- trimws(d$longitude)
			d$longitude <- gsub(" ", "", d$longitude)
			d$longitude[d$longitude == ""] <- NA
			d$longitude <- as.numeric(d$longitude)
		}
		d <- d[!is.na(d$yield), ]
		d
	}

	if (is.null(sf)) {
		sf <- grep("regular", ff, ignore.case=TRUE, value=TRUE)
		d <- doit(sf)
		sf <- grep("striga", ff, ignore.case=TRUE, value=TRUE)		
		if (length(sf) > 0) {
			e <- doit(sf, striga=TRUE)
			if (nrow(e) > 0) {
				d <- carobiner::bindr(d, e)
			}
		}
		sf <- grep("borer", ff, ignore.case=TRUE, value=TRUE)		
		if (length(sf) > 0) {
			e <- doit(sf, borer=TRUE)
			if (nrow(e) > 0) {
				d <- carobiner::bindr(d, e)
			}
		}
	} else {
		sf <- grep(sf, ff, ignore.case=TRUE, value=TRUE)
		d <- doit(sf)
	}
	
	d <- carobiner::change_names(d, c("pl_ht", "dy_sk", "dy_tass", "pl_st", "anthesis", "e_ht"), 
				c("plant_height", "silking_days", "tassling_days", "plant_density", "anthesis_days", "ear_height"), must_have=FALSE)

	if (!is.null(d$grain_N)) d$grain_N <- as.numeric(d$grain_N)
	if (!is.null(d$tassling_days)) d$tassling_days <- as.numeric(d$tassling_days)
	if (!is.null(d$plant_density)) d$plant_density <- as.numeric(d$plant_density) * 10000
	if (!is.null(d$anthesis_days)) d$anthesis_days <- round(d$anthesis_days)

	d$yield[d$yield < 0] <- NA
	for (v in c("silking_days", "tassling_days", "anthesis_days", "plant_height")) {
		if (!is.null(d[[v]])) d[[v]][d[[v]] < 10] <- NA
	}
	
	d <- carobiner::change_names(d, c("gwt"), 
				c("grain_weight"), must_have=FALSE)
	
	
	d
}
