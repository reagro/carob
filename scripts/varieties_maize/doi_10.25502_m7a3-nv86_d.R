# R script for "carob"

carob_script <- function(path) {

"The GGE biplot tool has potential for determining combining ability effects, identifying distinct heterotic groups and efficient testers in a line × tester study. However, its use for such analysis has not been adequately explored. The objectives of this study were to (i) assess combining ability of extra-early maturing lines (80–85 days to physiological maturity) and testers for grain yield (ii) classify lines into heterotic groups and (iii) identify most efficient testers using GGE biplot. Sixty-three lines crossed to four testers were evaluated under Strga-infested, drought and nonstress environments for 2 years in Nigeria. Results of GGE biplot analyses of combining ability and heterotic patterns of yield of lines, grouping and identification of testers were close to those of the conventional line × tester method. Testers TZEEI 13, TZEEI 21 and TZEEI 29 were highly efficient in grouping lines under stress environments while testers TZEEI 21 and TZEEI 29 were best under nonstress environments. The GGE biplot identified tester TZEEI 13, TZEEI 21 and TZEEI 29 as most efficient across stress environments and TZEEI 21 and TZEEI 29 across nonstress environments."


	uri <- "doi:10.25502/m7a3-nv86/d"
	group <- "varieties_maize"


	ff  <- carobiner::get_data(uri, path, group)

 
	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institute = "IITA",
		publication = "doi.org/10.1556/0806.47.2019.25",
		project = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer",
		response_vars = "yield", 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2024-10-29",
		notes = NA, 
		design = "16×9 lattice design"
	)
	

	sort_data <- function(fname, id, treatment) {
	  
	  f <- ff[basename(ff) == fname]
	  r <- read.csv(f)
	  
	  colnames(r) <- gsub("YIELDLN","YIELD",colnames(r))
	  colnames(r) <- gsub("PLSTLN","PLST",colnames(r))
	  colnames(r) <- gsub("DYSKLN","DYSK",colnames(r))
	  colnames(r) <- gsub("POLLENLN","POLLEN",colnames(r))
	  colnames(r) <- gsub("ASILN","ASI",colnames(r))
	  colnames(r) <- gsub("PLHTLN","PLHT",colnames(r))
	  colnames(r) <- gsub("EHTLN","EHT",colnames(r))
	  colnames(r) <- gsub("RLLNPERCT","RLPERCT",colnames(r))
	  colnames(r) <- gsub("SLLNPERCT","SLPERCT",colnames(r))
	  colnames(r) <- gsub("HUSKLN","HC",colnames(r))
	  colnames(r) <- gsub("PASPLN","PASP",colnames(r))
	  colnames(r) <- gsub("PHARVLN","PHARV",colnames(r))
	  colnames(r) <- gsub("EHARVLN","EHARV",colnames(r))
	  colnames(r) <- gsub("EASPLN","EASP",colnames(r))
	  colnames(r) <- gsub("EROTLN","EROT",colnames(r))
	  colnames(r) <- gsub("MOISTLN","MOIST",colnames(r))
	  
	d <- data.frame(
	  treatment = as.character(treatment),
	  date = r$YEAR,
		location = r$LOC,
		rep = as.integer(r$Rep),
		variety = r$Pedigree,
		anthesis_days = as.numeric(r$POLLEN),
		silking_days = as.numeric(r$DYSK),
		asi = as.numeric(r$ASI),
		plant_height = as.numeric(r$PLHT),
		ear_height = as.numeric(r$EHT),
		rlper = r$RLPERCT,
		slper = r$SLPERCT,
		husk = r$HC,
		p_asp = r$PASP,
		e_asp = r$EASP,
		e_rot = r$EROT,
		p_harv = r$PHARV,
		e_harv = r$EHARV,
		moist = r$MOIST,
		yield = as.numeric(r$YIELD),
		trial_id = as.character(id)
	)}

	d1 <- sort_data("Drought.csv", 1, "drought_stress")
	d2 <- sort_data("Low-N.csv", 2, "low_nitrogen")
	d3 <- sort_data("Opt.csv", 3, "optimum")

	d <- rbind(d1, d2, d3)
	
	d <- d[d$location != "LOC", ]
	d <- d[d$location != "", ]
	
	#site data (see publication)
	d$location <- gsub("IK|IKDS","Ikenne",d$location)
	d$location <- gsub("BG","Bagauda",d$location)
	d$location <- gsub("MO","Mokwa",d$location)
	d$location <- gsub("IFE","Ile-Ife",d$location)
	
	d$country <- "Nigeria"
	
	d$longitude[d$location=="Ikenne"] <- 3.1167
	d$latitude[d$location=="Ikenne"] <- 7.45
	d$longitude[d$location=="Bagauda"] <- 8.3667
	d$latitude[d$location=="Bagauda"] <- 12
	d$longitude[d$location=="Mokwa"] <- 7.3333
	d$latitude[d$location=="Mokwa"] <- 9.2667
	d$longitude[d$location=="Ile-Ife"] <- 4.55
	d$latitude[d$location=="Ile-Ife"] <- 7.4667
	d$geo_from_source <- FALSE
	
	d$elevation[d$location=="Ikenne"] <- 1500
	d$elevation[d$location=="Bagauda"] <- 580
	d$elevation[d$location=="Mokwa"] <- 300
	d$elevation[d$location=="Ile-Ife"] <- 244

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$borer_trial <- FALSE
	d$striga_trial <- FALSE
	d$striga_infected <- FALSE
	
	#fertilizer regimes (see publication)
	d$fertilizer_type <- "NPK;urea"
	d$N_fertilizer <-ifelse(d$treatment=="optimum",120,NA)
	d$P_fertilizer <-ifelse(d$treatment=="optimum",60,NA)
	d$K_fertilizer <-ifelse(d$treatment=="optimum",60,NA)
	
	#weed management (see publication)
	d$herbicide_used <- ifelse(d$treatment=="optimum", TRUE, FALSE)
	d$herbicide_product <- ifelse(d$treatment=="optimum", "primextra;paraquat dichloride", NA)
	d$herbicide_amount <- ifelse(d$treatment=="optimum", 5, 0)
	
	d$planting_date[d$date==2013] <- as.character("2013")
	d$planting_date[d$date==2014] <- as.character("2014")
	d$harvest_date[d$date==2013] <- as.character("2014")
	d$harvest_date[d$date==2014] <- as.character("2015")
	 
	d$crop <- "maize"
	d$yield_part <- "grain"
	
	d$asi[d$asi < -7] <- NA
	d$asi[d$asi > 30] <- NA
	d$silking_days[d$silking_days < 30] <- NA
	d$silking_days[d$silking_days > 125] <- NA
	d$ear_height[d$ear_height < 10] <- NA
	d$plant_height[d$plant_height < 10] <- NA
	
	
	carobiner::write_files(path, meta, d)
}


