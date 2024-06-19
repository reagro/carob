# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"The data set evaluates adaptability and suitability of groundnut varieties to different ecozones.
"

#### Identifiers
	uri <- "doi:10.7910/DVN/MY0ATH"
	group <- "fertilizer"

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)

##### dataset level metadata 
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=4),
		data_institute = "IITA",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "P_fertilizer;variety;longitude;latitude", 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2024-06-20"
	)
	
##### PROCESS data records

# read data 

	f <- ff[basename(ff) == "003_phosphorusRateonGroundnutMother_Ghana.xls"]
	f1 <- ff[basename(ff) == "002_phosphorusRateonGroundnut_Ghana.xls"]
	f2 <- ff[basename(ff) == "001_phosphorusRatesonGroundnutMorther_Ghana_2014-2015.xls"]
	
	r <- carobiner::read.excel(f,sheet = "RAW 2014")
	r1 <- carobiner::read.excel(f,sheet = "RAW 2015")
	r2 <- carobiner::read.excel(f1,sheet = "RAW 2014")
	r3 <- carobiner::read.excel(f1,sheet = "RAW DATA 2015")
	r4 <- carobiner::read.excel(f2,sheet = "SAS 2014")
	r5 <- carobiner::read.excel(f2,sheet = "SAS 2015")

## process file(s)

	d1 <- data.frame(
		date="2014",
		site=r$DSTRT,
		location=r$CMMTY...2,
		P_fertilizer=r$PRATE,
		variety=r$VARTY,
		plant_height=r$`PLNTH (cm)`,
		flowering_days=r$DFFLW,
		yield=r$`GYD/ha`,
		residue_yield=NA,
		dym_total=NA
			)
	
	d2 <- data.frame(
	  date="2015",
	  site=r1$DSTRT,
	  location=r1$CMMTY...2,
	  P_fertilizer=r1$PRATE,
	  variety=r1$VARTY,
	  plant_height=r1$PLNTH,
	  flowering_days=NA,
	  yield=r1$`GYD /ha`,
	  residue_yield=r1$`FODDER /ha`,
	  dym_total=r1$TBMSS
	)
	#fixing names as given in label sheet
	d1$site <- carobiner::replace_values(d1$site,c(1,2),c("Kassena Nankana","Bongo"))
	d2$site <- carobiner::replace_values(d2$site,c(1,2),c("Kassena Nankana","Bongo"))
	d1$variety <- carobiner::replace_values(d1$variety,c(1,2,3,4,5),c("chinese","cri-azivivi","obolo","manipinta","yenyawoso"))
	d2$variety <- carobiner::replace_values(d2$variety,c(1,2,3,4,5),c("chinese","cri-azivivi","obolo","manipinta","yenyawoso"))
	
	d3 <- data.frame(
	  date="2014",
	  site=r2$DSTRT,
	  location=r2$CMMTY...2,
	  P_fertilizer=r2$PRATE,
	  variety=r2$VARTY,
	  plant_height=r2$PLNTH,
	  flowering_days=r2$DFFLW,
	  yield=r2$GYD...17,
	  residue_yield=r2$FODDER...13,
	  dym_total=r2$TBMSS
	)
	
	d4 <- data.frame(
	  date="2015",
	  site=r3$DSTRT,
	  location=r3$CMMTY,
	  P_fertilizer=r3$PRATE,
	  variety=r3$VAR,
	  plant_height=r3$PLNTH,
	  flowering_days=NA,
	  yield=r3$GYD...10,
	  residue_yield=r3$STOVER...12,
	  dym_total=r3$TBMSS
	)

	#fixing names as given in label sheet
	d3$site <- carobiner::replace_values(d3$site,c(1,2),c("Tolon","Savelugu"))
	d4$site <- carobiner::replace_values(d4$site,c(1,2),c("Tolon","Savelugu"))
	d4$location <- carobiner::replace_values(d4$location,c(1,2,3,4),c("Tingoli","Gbanjong","Tibogunayili","Duko"))
	d3$variety <- carobiner::replace_values(d3$variety,c(1,2,3,4,5),c("chinese","cri-azivivi","obolo","manipinta","yenyawoso"))
	d4$variety <- carobiner::replace_values(d4$variety,c(1,2,3,4,5),c("chinese","cri-azivivi","obolo","manipinta","yenyawoso"))
	
	d5 <- data.frame(
	  date="2014",
	  site=r4$DSTRT,
	  location=r4$CMMTY,
	  P_fertilizer=r4$PRATE,
	  variety=r4$VARTY,
	  plant_height=r4$PLNTH,
	  flowering_days=r4$DFFLW,
	  yield=r4$GRAIN,
	  residue_yield=r4$FODDER,
	  dym_total=r4$TBMM
	)
	
	d6 <- data.frame(
	  date="2015",
	  site=r5$DSTRT,
	  location=r5$CMMTY,
	  P_fertilizer=r5$PRATE,
	  variety=r5$VARTY,
	  plant_height=r5$PLNTH,
	  flowering_days=NA,
	  yield=r5$GRAIN,
	  residue_yield=r5$FODDER,
	  dym_total=r5$TBMM
	)

	#fixing names as given in label sheet
	d5$site <- carobiner::replace_values(d5$site,c(1,2),c("Nadowli","Wa West"))
	d6$site <- carobiner::replace_values(d6$site,c(1,2),c("Nadowli","Wa West"))
	d5$location <- carobiner::replace_values(d5$location,c(1,2,3,4),c("Goroyori","Gyilli","Zanko","Pase"))
	d6$location <- carobiner::replace_values(d6$location,c(1,2,3,4),c("Goroyori","Gyilli","Zanko","Pase"))
	d5$variety <- carobiner::replace_values(d5$variety,c(1,2,3,4,5),c("G1-Sari Chinese","G2-Cri-Azivivi","G3-Obolo","G4-Manipinta","G5-Yenyawoso"))
	d6$variety <- carobiner::replace_values(d6$variety,c(1,2,3,4,5),c("G1-Sari Chinese","G2-Cri-Azivivi","G3-Obolo","G4-Manipinta","G5-Yenyawoso"))	

	d <- carobiner::bindr(d1, d2, d3, d4, d5, d6)	
	
	d$P_fertilizer <- carobiner::replace_values(d$P_fertilizer,c(1,2),c(60,90))
	
#### about the data #####

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$inoculated <- FALSE
  d$crop <- "groundnut"
  d$yield_part <- "seed"

##### Location #####
	d$country <- "Ghana"
	d$elevation <- NA
	
	geo <- data.frame(
	  location=c("Bonia", "Nyangai", "Samboligo", "Tingoli", "Gbanjong", "Tibogunayili","Duko","Gorogori","Gyilly","Zanko","Pase"), 
	  latitude=c(10.9078, 10.8950, 10.9078, 9.3750, 9.4319, 9.6242, 9.5636, 10.0606, 10.0606, 10.0676, 10.0367), 
	  longitude=c(-0.8081, -1.0939, -0.8081, -1.0095, -1.0658, -0.8253, -0.8357, -2.5017, -2.5017, -2.5959, -2.7109)
	)
	
	d <- merge(d, geo, by="location", all.x = TRUE)
   
	carobiner::write_files(path, dset, d)
}


