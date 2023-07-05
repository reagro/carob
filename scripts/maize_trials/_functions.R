
intmztrial_borer <- function(ff, f, striga=FALSE) {

	combine_syn <- function(dd, keep, drop) {
		if (!is.null(d[[keep]]) & !is.null(d[[drop]])) {
			d[[keep]] <- rowMeans(d[, c(keep, drop)], na.rm=TRUE)
			d[[drop]] <- NULL
		}
		d
	}
	
}


intmztrial_striga <- function(ff, sf=NULL) {

	doit <- function(sf, striga=FALSE, borer=FALSE) {
		d  <- read.csv(sf)
		names(d) <- tolower(names(d))
		d$x <- NULL
		names(d)[1] <- "id"
		d$id <- NULL
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
		 c("trl_titl", "entry", "entryno", "instinf", "bltin_m", "bltun_m", "x1000gwt", "x_1000gwt", "cobdamco", "cobdamrt", "borerdmrat", "sbdamat"), 
		 c("trial_name", "variety", "variety_code", "instin", "blight_in", "blight_un", "grain_weight", "grain_weight", "cob_dam_co", "cob_dam_rt", "borer_dam_rat", "sb_dam_rat")
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
		 c("str_ra1", "str_ra2",    "yld", "r_l", "s_l", "plst", "eld", "borer_dm_rat"), 
		 c("str_rat1", "str_rat2", "yield", "rl", "sl", "pl_st", "eldana", "borer_dam_rat")
		 , must_have=FALSE)

		if (!is.null(d$variety_code)) {
			d$variety_code <- as.character(d$variety_code)
		}
		if (is.null(d$yield2)) {
			d$yield2 <- as.numeric(NA)
		} else {
			d$yield2 <- as.numeric(d$yield2)	
		}
		if (is.null(d$grain_weight)) {
			d$grain_weight <- as.numeric(NA)
		} else {
			d$grain_weight <- as.numeric(d$grain_weight)	
		}
		qv <- c("yield", "pl_ht", "e_ht", "asi", "gwt", "dy_sk", "p_asp", "p_harv", "e_asp", "husk", "rl", "sl", "e_rot", "rust", "anthesis", "moist", "pl_st")
		for (v in qv) {
			if (!is.null(d[[v]])) {
				d[[v]][d[[v]] == "."] <- NA
				d[[v]][d[[v]] == "S"] <- NA
				d[[v]] <- as.numeric(d[[v]])			
			}
		}
		d$yield[d$yield < 0] <- NA
		
		d$yield_part <- "grain"
		d$striga_trial <- striga
		d$borer_trial <- striga
		d$variety <- trimws(d$variety)
		d$variety[d$variety == ""] <- NA
		d$description <- as.character(d$description)
		d$description[d$description==""] <- "not provided"

		if (inherits(d$latitude, "character")) {
			d$latitude <- trimws(d$latitude)
			d$latitude <- gsub(" ", "", d$latitude)
			d$latitude[d$latitude == ""] <- NA
			d$latitude <- as.numeric(d$latitude)
		}
		if (inherits(d$longitude, "character")) {
			d$longitude <- trimws(d$longitude)
			d$longitude <- gsub(" ", "", d$longitude)
			d$longitude[d$longitude == ""] <- NA
			d$longitude <- as.numeric(d$longitude)
		}
		d
	}

	if (is.null(sf)) {
		sf <- grep("regular", ff, ignore.case=TRUE, value=TRUE)
		d <- doit(sf)
		sf <- grep("striga", ff, ignore.case=TRUE, value=TRUE)		
		if (length(sf) > 0) {
			e <- doit(sf, striga=TRUE)
			d <- carobiner::bindr(d, e)
		}
		sf <- grep("borer", ff, ignore.case=TRUE, value=TRUE)		
		if (length(sf) > 0) {
			e <- doit(sf, borer=TRUE)
			d <- carobiner::bindr(d, e)
		}
		d
	} else {
		sf <- grep(sf, ff, ignore.case=TRUE, value=TRUE)
		doit(sf)
	}
}

