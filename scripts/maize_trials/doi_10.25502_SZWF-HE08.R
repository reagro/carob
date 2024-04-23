
# border tbd

carob_script <- function(path) {

"
Yield gains and associated changes in an early yellow bi-parental maize population following Genomic Selection for Striga resistance and drought tolerance."
				
	uri <- "doi:10.25502/szwf-he08"
	group <- "maize_trials"	
	ff <- carobiner::get_data(uri, path, group)

	dset <- data.frame(
		carobiner::read_metadata(uri, path, major=2, minor=1, group),
 	    publication="doi:10.1186/s12870-019-1740-z",
		carob_contributor = "Siyabusa Mkuhlani",
		carob_date="2024-17-01",
		data_type = "experiment",
		project="CGIAR Research Program on Maize",
		data_institutions="IITA"
	)

	read_data <- function(f) {
		r <- read.csv(f)
		colnames(r) <- toupper(colnames(r))
		d <- data.frame(
			trial_id=gsub(" ", "_", gsub(".csv", "", basename(f))),
			location = r$LOC,
			## RH: were all locations planted on 04-01?
			## that seems rather unlikely.
			planting_date = paste(r$YEAR, "04", "01", sep="-"),
			## RH: How can it be "x + ASI"? 
			## anthesis = r$DYSK + r$ASI,  
			anthesis_days = r$DYSK - r$ASI,  
			variety = r$PEDIGREE,
			rep = r$REP,
			yield = r$YIELD,
			polshed = r$POLLEN,
			silking_days = r$DYSK,
			asi = r$ASI,
			plant_height = r$PLHT,
			e_ht = r$EHT,
			p_asp = r$PLASP,
			#e_harv = r$EASP #RH: ???
			e_asp = r$EASP, 
			erot = r$E_ROT
		)

		if (is.null(r$RL_PERC)) {
			d$rlper <- r$RL * 100
		} else {
			d$rlper <- r$RL_PERC * 100
		}
		if (is.null(r$SL_PERC)) {
			d$slper = r$SL * 100
		} else {
			d$slper = r$SL_PERC * 100		
		}
		d$husk <- r$HC
		d
	}

	
	#data for IKDS
	f1 <- ff[basename(ff) %in% c("EARLY Drought.csv", "Extra-Early Drought.csv")]
	d1 <- do.call(rbind, lapply(f1, read_data))
	d1$adm1 <- "Ogun"
	d1$adm2 <- "Ikenne"
#RH: please fix:
#	d0$location <- "?????"
	d1$longitude <- 3.711
	d1$latitude <- 6.872



#data set2 Early Drought+Heat.csv
	f2 <- ff[basename(ff) %in% c("Early Drought+Heat.csv",  "Early Heat.csv", "Extra-Early Drought+Heat.csv")] 
	d2 <- do.call(carobiner::bindr, lapply(f2, read_data))

	d2$adm1 <- "Kano"
	d2$adm2 <- "Garum Mallam"
	d2$location <- "Kadawa"
	d2$longitude <- 8.448
	d2$latitude <- 11.645

	d <- rbind(d1, d2)

	
	d$country <- 'Nigeria'
	d$yield_part <- 'grain'
	d$N_fertilizer <- 120
	d$P_fertilizer <- 60
	d$K_fertilizer <- 60
	d$crop <- 'maize'
#	d$pl_st <- NA
	d$striga_trial <- TRUE
	d$striga_infected <- TRUE
	d$borer_trial <- FALSE

	d$variety[d$variety == "Check 3 - 2015 TZE \x96Y DT STR Syn C0"] = "Check 3 - 2015 TZE DT STR Syn C0"
	
	carobiner::write_files(dset, d, path=path)
}


