# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
Aminu, Adnan, 2020, Nigeria Variety Trial,
https://hdl.handle.net/11529/10548312, CIMMYT Research 
Data & Software Repository Network, V1, UNF:6:gqM5Z/65GCSgV3VJ8bre6Q== [fileUNF]
"

#### Identifiers
	uri <- "hdl:11529/10548312"
	group <- "crop_cuts"

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)

##### dataset level metadata 
	dset <- data.frame(
		# change the major and minor versions if you see a warning
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institutions = "Tamasa",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= NA,
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		project=NA,
		data_type= "experiment",
		carob_contributor= "Shumirai Manzvera",
		carob_date="2024-05-20"
	)
	
##### PROCESS data records

# read data 

	f <- ff[basename(ff) == "NG_VT_Calibration_2016.xlsx"]
	r <- carobiner::read.excel(f, sheet="Data")
	f1 <- ff[basename(ff) == "NG_VT_NGS_2016 (1).xlsx"]
	r1 <-carobiner::read.excel(f1, sheet="Revised")
	f2 <- ff[basename(ff) == "NG_VT_Validation_SS_2016.xlsx"]
	r2<-carobiner::read.excel(f2, sheet="DATA")
	# or  r <- carobiner::read.excel(f)

## process file(s)

## select the variables of interest and assign them to the correct name
	d0 <- data.frame(
	
		yield = r$GY_ha,
		variety= r$Variety,
		rep= as.integer(r$Rep),
		adm3=r$Location,
		silking_days=r$DTS,
		emergence_days=r$DTE,
		tassling_days=r$DTT
		# etc
	)

	
#### about the data #####

	d0$irrigated <- TRUE

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	#d$country <- "Nigeria"
	
	d0$adm1[d0$adm3=="LERE"]<- "Kaduna"
	d0$adm1[d0$adm3=="SMR"]<- "Kaduna"
	d0$adm1[d0$adm3=="BUK"]<- "Kano"
	d0$adm1[d0$adm3=="DBT"]<- "Kano"
	
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d0$longitude[d0$adm3=="LERE"] <-  8.57286
	d0$latitude [d0$adm3=="LERE"]<- 10.38584
	d0$longitude[d0$adm3=="SMR"] <-  8.383
	d0$latitude[d0$adm3=="SMR"] <-  9.750
	d0$longitude[d0$adm3=="BUK"] <- 12.383 
	d0$latitude[d0$adm3=="BUK"] <-  11.233
	d0$longitude[d0$adm3=="DBT"] <-  12.4382197
	d0$latitude[d0$adm3=="DBT"] <-  8.6160674
	
##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	d0$planting_date <- as.character(as.Date(  "2016/03/23" ))
	d0$harvest_date  <- as.character(as.Date(  "2016/06/20" ))
	
	d1 <- data.frame(
	  
	  yield = r1$GY_ha,
	  variety= r1$Variety,
	  rep= NA,silking_days=NA,emergence_days=NA,tassling_days= NA,irrigated=NA,planting_date= NA,
	  harvest_date= NA,adm3=r1$LGA
	  )
	
	d1$adm1[d1$adm3=="LERE"]<- "Kaduna"
	d1$adm1[d1$adm3=="Doguwa"]<- "Kano"
	d1$adm1[d1$adm3=="Ikara"]<- "Kaduna"
	
	 # Coordinates 
	
	d1$longitude[d1$adm3=="LERE"] <-  8.57286
	d1$latitude [d1$adm3=="LERE"]<- 10.38584
	d1$longitude[d1$adm3=="Ikara"] <- 10.167 
	d1$latitude [d1$adm3=="Ikara"]<- 11.183
	d1$longitude[d1$adm3=="Doguwa"] <- 8.233 
	d1$latitude [d1$adm3=="Doguwa"]<- 11.183
	
	
	d2 <- data.frame(
	  
	  yield = r2$GY_ha,
	  variety= r2$Variety,
	  rep=NA,adm3=NA,silking_days=NA,emergence_days=NA,tassling_days=NA,irrigated=NA,planting_date=NA,
	  harvest_date=NA
	)
	
	d2$adm1<- "Kano"
	d2$longitude <-  8.51672
	d2$latitude <-  12.00012
	 
 d<-rbind(d2,d1,d0)
	

	d$crop <- "maize"
	d$yield_part <- "grain"
	d$country <- "Nigeria"
	d$trial_id = "1"
	
	
	
# all scripts must end like this
	carobiner::write_files(path, dset, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

