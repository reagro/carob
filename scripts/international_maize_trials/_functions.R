
intmztrial <- function(ff, f, striga=FALSE) {
	f <- ff[tolower(basename(ff))==f]
	d <- read.csv(f)
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
	 c("trl_titl", "entry", "entryno", "instinf", "bltin_m", "bltun_m", "x1000gwt"), 
	 c("trial_name", "variety", "variety_code", "instin", "blight_in", "blight_un", "grain_weight")
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
	}

	d <- carobiner::change_names(d, 
	 c("str_ra1", "str_ra2", "yld", "r_l", "s_l", "plst"), 
	 c("str_rat1", "str_rat2", "yield", "rl", "sl", "pl_st")
	 , must_have=FALSE)

	d
}

