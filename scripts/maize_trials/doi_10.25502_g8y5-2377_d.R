# R script for "carob"
#Issues
#error in reading publication DOI

carob_script <- function(path) {

"
Vitamin A deficiency, drought, low soil nitrogen (low N) and Striga hermonthica parasitism of maize (Zea mays L.) cause malnutrition and food insecurity in sub-Saharan Africa. The objectives of this study were to determine combining abilities of extra-early provitamin A (PVA) lines, classify them into heterotic groups (HGs), identify testers, and determine yield stability of hybrids under contrasting environments in two trials. In trial 1, 20 extra-early PVA lines were inter-mated in a diallel mating scheme to obtain 190 F1 hybrids. The 190 F1 hybrids plus six checks were tested under Striga infestation, drought, and stress-free environments in Nigeria from 2015 to 2017. In trial 2, 35 extra-early yellow hybrids were evaluated under low-N, Striga-infested and stress-free environments in 2018. Provitamin A concentrations of 23.98 36 and 22.56 µg g-1 were obtained for TZEEIOR 202 and TZEEIOR 205. TZEEIOR 197 × 37 TZEEIOR 205 (20.1 μg g-1) and TZEEIOR 202 × TZEEIOR 205 (22.7 μg g-1) contained about double the PVA level of the commercial check, TZEEI 58 × TZEE-Y Pop STR C5 (11.4 μg 39 g -1). Both general (GCA) and specific (SCA) combining ability variances were statistically significant for most agronomic traits, although GCA was much larger than SCA effects, indicating that additive genetic effects primarily controlled the inheritance of those traits. TZEEIOR 97 and TZEEIOR 197 were identified as inbred testers. TZEEIOR 197 × TZEEIOR 205 (20.1 μg g-1) was identified as a single-cross tester as well as the most stable and highest yielding hybrid across environments. TZEEIOR 202 and TZEEIOR 205 should be invaluable resources for breeding for high PVA. PVA level was independent of hybrid yield potential, indicating that selection of superior hybrids with elevated PVA levels should be feasible.
"

## Identifiers
	uri <- "doi:10.25502/g8y5-2377/d"
	group <- "maize_trials"

## Download data 
	ff  <- carobiner::get_data(uri, path, group)

## metadata 
	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institute = "IITA",
		publication = NA,
		#publication -> "https://acsess.onlinelibrary.wiley.com/doi/full/10.1002/csc2.20071",
		project = NA,
		data_type = "experiment",
		treatment_vars = "location,irrigation;striga_infeCted", 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2024-07-11"
	)
	
## read data 

	f <- ff[basename(ff) == "Drought Stress.csv"]
	f0 <- ff[basename(ff) == "Optimum.csv"]
	f1 <- ff[basename(ff) == "Striga.csv"]
	
	r <- read.csv(f)
	r0 <- read.csv(f0)
	r1 <- read.csv(f1)
	
	#processing data
	d0 <- data.frame(
	  date=r$YEAR,
		location=r$LOC,
		rep=r$Rep,
		variety_code=r$Pedigree,
		dy_poll=r$POLLEN,
		silking_days=r$DYSK,
		asi=r$ASI,
		plant_height=r$PLHT,
		ear_height=r$EHT,
		rl=r$RL,
		rlper=r$RLPERCT,
		sl=r$SL,
		slper=r$SLPERCT,
		husk=r$HC,
		p_asp=r$PASP,
		e_asp=r$EASP,
		p_harv=r$PHARV,
		e_harv=r$EHARV,
		e_rot=r$EROT,
		moist=r$MOIST,
		yield = r$YIELD
	)
	
	d1 <- data.frame(
	  date=r0$YEAR,
	  location=r0$LOC,
	  rep=r0$Rep,
	  variety_code=r0$Pedigree,
	  dy_poll=r0$POLLEN,
	  silking_days=r0$DYSK,
	  asi=r0$ASI,
	  plant_height=r0$PLHT,
	  ear_height=r0$EHT,
	  rl=r0$RL,
	  rlper=NA,
	  sl=r0$SL,
	  slper=NA,
	  husk=r0$HC,
	  p_asp=r0$PASP,
	  e_asp=r0$EASP,
	  p_harv=r0$PHARV,
	  e_harv=r0$EHARV,
	  e_rot=r0$EROT,
	  moist=r0$MOIST,
	  yield = r0$YIELD
	)
	
	d2 <- data.frame(
	  date=r1$YEAR,
	  location=r1$LOC,
	  rep=r1$Rep,
	  variety_code=r1$Pedigree,
	  dy_poll=r1$POLLENIN,
	  silking_days=r1$DYSKIN,
	  asi=r1$ASIIN,
	  plant_height=r1$PLHTIN,
	  ear_height=NA,
	  rl=NA,
	  rlper=r1$RLINPERCT,
	  sl=NA,
	  slper=r1$SLINPERCT,
	  husk=NA,
	  p_asp=NA,
	  e_asp=NA,
	  p_harv=NA,
	  e_harv=NA,
	  e_rot=NA,
	  moist=NA,
	  ehtin=r1$EHTIN,
	  rlin=r1$RLIN,
	  slin=r1$SLIN,
	  easpin=r1$EASPIN,
	  pharvin=r1$PHARVIN,
	  eharvin=r1$EHARVIN,
	  erotin=r1$EROTIN,
	  moistin=r1$MOISTIN,
	  str_c01=r1$CO1,
	  str_c02=r1$CO2,
	  str_rat1=r1$RAT1,
	  str_rat2=r1$RAT2,
	  yield = r1$YIELDIN
	)

	dd <- rbind(d0, d1)
	
	dd$ehtin <- dd$rlin <-dd$rlper <-	dd$slin <- dd$slper <- dd$easpin <- dd$pharvin <- dd$eharvin <-
	  dd$erotin <- dd$moistin <- dd$str_c01 <- dd$str_c02 <- dd$str_rat1 <- dd$str_rat2 <- NA
	
	d <- rbind(d2, dd)
	
#fertilizer rates(see publication) 
	d$N_fertilizer[d$location %in% c("BG", "IK", "MO_OPT","IKDS")] <- 90
	d$N_fertilizer[d$location=="MO"] <- 30
	d$P_fertilizer[d$location %in% c("BG", "IK", "MO_OPT","IKDS")] <- 60
	d$P_fertilizer[d$location=="MO"] <- 30
	d$K_fertilizer[d$location %in% c("BG", "IK", "MO_OPT","IKDS")] <- 60
	d$K_fertilizer[d$location=="MO"] <- 30
	
#field managment(see publication)
	d$herbicide_used <- TRUE
	d$herbicide_product <- ifelse(d$location%in% c("BG", "IK", "MO_OPT","IKDS"), "atrazine;paraquat dichloride","atrazine")
	d$irrigation <- TRUE
	d$irrigation_amount <- 17
	d$striga_trial <- ifelse(d$location%in% c("MO"), TRUE, FALSE)
	d$striga_infected <- ifelse(d$location%in% c("MO"), TRUE, FALSE)
	
#publication stated all trials began November and ended in March the following year
	d$planting_date[d$date == 2015] <- "2015-11"
	d$planting_date[d$date == 2016] <- "2016-11"
	d$planting_date[d$date == 2017] <- "2017-11"
  d$harvest_date[d$date ==2015] <- "2016-03"
  d$harvest_date[d$date ==2016] <- "2017-03"
  d$harvest_date[d$date ==2017] <- "2018-03"
  d$date <- as.character(d$date)
  
#fixing location names (see publication)
  d$country <- "Nigeria"
  d$location <- gsub("IKDS|IK","Ikenne",d$location)
  d$location <- gsub("MO|MO_OPT","Mokwa",d$location)
  d$location <- gsub("BG","Bagauda",d$location)
  
#corresponding longitude and latitude
  d$longitude[d$location=="Ikenne"] <- 3.7150
  d$latitude[d$location=="Ikenne"] <- 6.8833
  d$elevation[d$location=="Ikenne"] <- 60
  d$longitude[d$location=="Mokwa"] <- 5.0667
  d$latitude[d$location=="Mokwa"] <- 9.3
  d$elevation[d$location=="Mokwa"] <- 457
  d$longitude[d$location=="Bagauda"] <- 8.3667
  d$latitude[d$location=="Bagauda"] <- 12
  d$elevation[d$location=="Bagauda"] <- 580
  

	d$trial_id <- as.character(as.integer(as.factor(r$ID)))
	
# about the data (TRUE/FALSE)
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$borer_trial <- FALSE
	d$crop <- "maize"
	d$yield_part <- "grain"
	

	carobiner::write_files(path, meta, d)
}



