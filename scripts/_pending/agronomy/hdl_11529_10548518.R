# R script for "carob"


carob_script <- function(path) {

"Maize is the staple crop cultivated during the monsoon season in the rainfed uplands in tribal-dominated plateau regions of Odisha in eastern India. However, productivity is low because of multiple factors, including poor adoption of best management practices. We conducted three types of experiments viz..single vs. layered best management practices, hybrids, and decision support tools on nutrient management for two years (2013 and 2014) to explore the opportunities for reducing rainfed maize yield gaps. On-farm trials were conducted in Mayurbhanj district in Odisha and data were collected manually from the treatments in each experiment and processed in excel file and analyzed using R software. (2020-09-30)"

	uri <- "hdl:11529/10548518"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institute = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "on-farm experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield", 
		carob_contributor = "Blessing Dzuda",
		carob_date = "2024-09-05",
		notes = "Fertilizer type and amount of lime not specified"
	)
	
	f1 <- ff[basename(ff) == "Experiment-I.csv"]
	f2 <- ff[basename(ff) == "Experiment-II.csv"]
	f3 <- ff[basename(ff) == "Experiment-III.csv"]
	
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)

	r1$Treat_details <- iconv(r1$Treat_details, from = "latin1", to = "UTF-8", sub = "")
	r3$Treat_details <- iconv(r3$Treat_details, from = "latin1", to = "UTF-8", sub = "")
	
	d1 <- data.frame(
		planting_date = as.character(r1$Year),
		adm1=r1$STATE,
		adm2=r1$District,
		location=r1$Village,
		treatment=r1$Treat_details,
		rep=r1$Rep,
		land_prep_method=r1$Tillage,
		variety=r1$Var,
		maturity_days=r1$Duration,
		seed_density=r1$SdRate,
		row_spacing=NA,
		plant_spacing=NA,
		N_fertilizer=r1$FN_amt,
		P_fertilizer=r1$FP_amt,
		K_fertilizer=r1$FK_amt,
		weeding_done=TRUE,
		weeding_method=r1$Weedmgt_type,
		fertilizer_price=r1$Fert_cost,
		plant_density=r1$PN,
		yield=r1$GY
	)
		
	d2 <- data.frame(
		planting_date = as.character(r2$Year),
		adm1=r2$STATE,
		adm2=r2$District,
		location=NA,
		treatment=r2$Treatment,
		rep=r2$Rep,
		land_prep_method=NA,
		variety=NA,
		maturity_days=r2$Duration,
		seed_density=NA,
		row_spacing=NA,
		plant_spacing=NA,
		N_fertilizer=r2$FN_amt,
		P_fertilizer=r2$FP_amt,
		K_fertilizer=r2$FK_amt,
		weeding_done=TRUE,
		weeding_method=NA,
		fertilizer_price=r2$Fert_cost,
		plant_density=NA,
		yield=r2$GY
	)
		
	d3 <- data.frame(
		planting_date = as.character(r3$Year),
		adm1=r3$STATE,
		adm2=r3$District,
		location=NA,
		treatment=r3$Treat_details,
		rep=r3$Rep,
		land_prep_method=NA,
		variety=r3$Var,
		maturity_days=NA,
		seed_density=r3$SdRate,
		row_spacing=r3$RS,
		plant_spacing=r3$PS,
		N_fertilizer=r3$FN_amt,
		P_fertilizer=r3$FP_amt,
		K_fertilizer=r3$FK_amt,
		weeding_done=TRUE,
		weeding_method=NA,
		fertilizer_price=r3$Fert_cost,
		plant_density=NA,
		yield=r3$GY
	)
	
	d <- rbind(d1, d2, d3)
	d$treatment <- gsub("\u0092", "'", d$treatment)
	
	d$country <- "India"
	d$crop <- "maize"
	d$yield <- d$yield * 1000
### ???
###	d$latitude <- 21.8650
###	d$longitude <- 86.421587
###	d$geo_from_source <- TRUE ????

### ???
###	d$trial_id <- "1"
     
	d$trial_id <- as.character(as.integer(as.factor(paste(d$adm2, d$location, d$year))))
	
	d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$is_survey <- FALSE 
	d$irrigated <- NA
    d$S_fertilizer <- 0

## d$lime_used <- ifelse(d$treatment=="NPK +( Micro-nutrient +S) -Lime (150:70:120)",FALSE,TRUE) 
## more direct
## d$lime_used <- d$treatment != "-Lime (150:70:120)"
## better?
## d$lime_used <- !grepl("-Lime", d$treatment, ignore.case=TRUE)
## or should it be:
	d$lime_used <- grepl("\\+lime", d$treatment, ignore.case=TRUE)


    d$land_prep_method <- gsub("Conventional", "conventional", d$land_prep_method)
## zero is not "reduced", it is "none"   
	d$land_prep_method <- gsub("Zerotillage", "none", d$land_prep_method)
    d$fertilizer_price <- as.character(d$fertilizer_price)
	
	d <- unique(d)
    carobiner::write_files(path, meta, d)
}
