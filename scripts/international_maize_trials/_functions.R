
intmztrial_borer <- function(ff, f, striga=FALSE) {

	combine_syn <- function(dd, keep, drop) {
		if (!is.null(d[[keep]]) & !is.null(d[[drop]])) {
			d[[keep]] <- rowMeans(d[, c(keep, drop)], na.rm=TRUE)
			d[[drop]] <- NULL
		}
		d
	}
	
}


intmztrial_striga <- function(ff, f, striga=FALSE) {

	sf <- ff[tolower(basename(ff))==tolower(f)]
	d <- read.csv(sf)
	names(d) <- tolower(names(d))
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
	 c("trl_titl", "entry", "entryno", "instinf", "bltin_m", "bltun_m", "x1000gwt", "cobdamco", "cobdamrt", "borerdmrat", "sbdamat"), 
	 c("trial_name", "variety", "variety_code", "instin", "blight_in", "blight_un", "grain_weight", "cob_dam_co", "cob_dam_rt", "borer_dam_rat", "sb_dam_rat")
	 , must_have=FALSE)


	d$start_date <- d$year
	d$start_date[d$year==22] <- 2002
	d$start_date[d$year==24] <- 2004
	d$start_date[d$year==25] <- 2005
	
	d$country <- carobiner::capitalize_words(d$country)
	d$location <- carobiner::capitalize_words(d$location)

	d$crop <- 'maize'

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
		din$striga_infected <- "yes"
		dun$striga_infected <- "no"
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
	 c("str_ra1", "str_ra2", "yld", "r_l", "s_l", "plst", "eld"), 
	 c("str_rat1", "str_rat2", "yield", "rl", "sl", "pl_st", "eldana")
	 , must_have=FALSE)

	d
}

