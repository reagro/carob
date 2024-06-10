# R script for "carob"


carob_script <- function(path) {

"Conservation agriculture (CA) had recently gained popularity and promotion in the southern parts of Africa. Research has shown a number of benefits of CA in contrast to the widely practiced conventional ways (CP), which chiefly include water and soil conservation. These gains have positive benefits towards grain yield in maize. However, the maize varieties that performs better than others in these different environments have to be investigated and updated for farmers and breeding purposes. Furthermore, physiological traits that are suitable for the CA system needs to be dissected for breeding purposes. Hence a study was conducted across Zimbabwe at University of Zimbabwe farm (heavy red clay), Domboshawa Training centre (DTC) (sandy loamy soils), Madziva (sandy soils), Hereford (red clays) and Zimuto (sandy soils) from 2012 up to 2015. Investigations of effects of CA and CP practices on emergence, chlorophyll content, early vigour, biomass and grain yield of different maize varieties using 12 hybrids and 4 open pollinated varieties (OPVs) were conducted. Emergence was collected as the number of days taken by the different varieties to emerge. At 6 weeks after sowing a destructive sampling was performed to quantify the vigor of the maize varieties using averages of height, number of leaves per plant, dry matter and chlorophyll content using a SPAD meter. At harvesting grain yield and biomass yield were calculated."

	uri <- "hdl:11529/10869"
	group <- "conservation_agriculture"
	ff  <- carobiner::get_data(uri, path, group)

	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=2),
		data_institute = "CIMMYT",
		publication = "doi:10.5539/jas.v8n11p112",
		project = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method;variety", 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2024-05-30"
	)
	
	f <- ff[basename(ff) == "MOTHER TRIAL 2012-2015.xlsx"]

	r0 <- carobiner::read.excel(f,sheet = "emergence ")
	r1 <- carobiner::read.excel(f,sheet = "ASI")
	r2 <- carobiner::read.excel(f,sheet = "GRAIN ")
	r3 <- carobiner::read.excel(f,sheet = "BIOMASS ")
	r4 <- carobiner::read.excel(f,sheet = "EARLY VIGOUR ")
	r5 <- carobiner::read.excel(f,sheet = "varieties.treats ")
	r6 <- carobiner::read.excel(f,sheet = "BEFORE CP INTRODUCTION ")
	

## process file(s)
	d1 <- data.frame(
		year=r0$YEAR,
		location=r0$SITE, 
		land_prep_method=r0$SYSTEM,
		rep=r0$rep,
		treatment=r0$treat,
		emergence_days= r0$`DAYS TO 50% E`
	)
	
	d2 <- data.frame(
	  year=r1$YEAR,
	  location=r1$SITE, 
	  land_prep_method=r1$SYSTEM,
	  rep=r1$rep,
	  treatment=r1$treat,
	  anthesis_days= r1$tasseling,
	  silking_days=r1$silking, 
	  asi=r1$ASI
	)
	
	d3 <- data.frame(
	  year=r2$YEAR,
	  location=r2$SITE, 
	  land_prep_method=r2$SYSTEM,
	  rep=r2$Replicate,
	  treatment=r2$Treatment,
	  yield=r2$`Grain yield`
	)

	d4 <- data.frame(
	  year=r4$YEAR,
	  location=r4$SITE,
	  land_prep_method=r4$SYSTEM,
	  rep=r4$rep,
	  treatment=r4$treat,
	  leaf_biomass=r4$`fresh wgt`,
	  dmy_total=r4$`dry wght`,
	  plant_height=r4$height
	)	
	
	d5 <- data.frame(
	  year=r3$YEAR,
	  location=r3$SITE, 
	  land_prep_method=r3$SYSTEM,
	  rep=r3$Replicate,
	  treatment=r3$Treatment,
	  fresh_biomass=r3$Biomass
	) 
	
	d6 <- data.frame(
	  year=r6$Year,
	  location=r6$site, 
	  land_prep_method=r6$SYSTEM,
	  rep=r6$REP,
	  treatment=r6$TREAT,
	  yield= r6$`grain yield`
	  
	)
	
	dd <- merge(d1, d2, by=c("year", "location", "land_prep_method", "rep", "treatment"), all.x=TRUE)
	dd <- merge(dd, d3, by=c("year", "location", "land_prep_method", "rep", "treatment"), all.x=TRUE)
	dd <- merge(dd, d4, by=c("year", "location", "land_prep_method", "rep", "treatment"), all.x=TRUE) 
	dd <- merge(dd, d5, by=c("year", "location", "land_prep_method", "rep", "treatment"), all.x=TRUE)
	d  <- merge(dd, d6, by=c("year", "location", "land_prep_method", "rep", "treatment", "yield"), all.x=TRUE)

	varietyname = c("SC 533", "Pristine 601", "Pannar 53", "Pannar 413", "ZM309",
	              "PGS 51", "Zap 61", "PHB 3253", "ZM 525", "ZM 401", "PGS 63",
	              "SC 637", "ZS 265", "SC 301", "SC 513", "ZS 261")
	
	d$variety <- varietyname[d$treatment]
	
	d$land_prep <- gsub("CA","conservation_agriculture", d$land_prep)
	d$land_prep <- gsub("CP","conventional_practices", d$land_prep)
	
	#fixing location names
	d$location <- gsub("DTC","Domboshawa Training Centre", d$location)
	d$location <- gsub("HFORD", "Hereford", d$location)
	d$location <- gsub("UZ", "University of Zimbabwe", d$location)
	d$location <- gsub("MADZIVA", "Madziva", d$location)
	d$location <- gsub("ZIMUTO", "Zimuto", d$location)
	
	#allocation of geo locations from publication
	d$country <- "Zimbabwe"
	d$longitude[d$location=="Domboshawa Training Centre"] <- 31.2833
	d$latitude[d$location=="Domboshawa Training Centre"] <- -18.0333
	d$longitude[d$location=="Hereford"] <- 31.7333
	d$latitude[d$location=="Hereford"] <- -17.7000
	d$longitude[d$location=="Madziva"] <- 31.7167
	d$latitude[d$location=="Madziva"] <- -17.0000
	d$longitude[d$location=="University of Zimbabwe"] <- 31.0528
	d$latitude[d$location=="University of Zimbabwe"] <- -17.4200
	d$longitude[d$location=="Zimuto"] <- 31.4667
	d$latitude[d$location=="Zimuto"] <- -20.4167
	
	#elevation
	d$elevation[d$location=="Domboshawa Training Centre"] <- 1500
	d$elevation[d$location=="Hereford"] <- 1054
	d$elevation[d$location=="Madziva"] <- 1169
	d$elevation[d$location=="Zimuto"] <- 1223
	d$elevation[d$location=="University of Zimbabwe"] <- 1483
	  
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$crop <- "maize"
	d$yield_part <- "grain"
	
##### Time #####

	d$planting_date <- substr(d$year, 1, 4)
	d$harvest_date <- paste0("20", substr(d$year, 6, 7))
	d$year <- NULL

##### Fertilizers #####
#fertilizer rates obtained from publication
   d$P_fertilizer <- 12.2 / 2.29
   d$K_fertilizer <-11.6 / 1.2051
   d$N_fertilizer <- 83
   d$N_splits <- 2
   d$fertilizer_type <- "D-compound; AN"
   d$inoculated <- FALSE
   d$plant_density <- 44444
     
   d$rep <- as.integer(d$rep)
   d$N_splits <- as.integer(d$N_splits)

	d$treatment <- as.character(d$treatment)
   
	carobiner::write_files(path, dset, d)
}



