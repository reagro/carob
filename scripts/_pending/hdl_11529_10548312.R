# R script for "carob"


# the treatments need to be clarified. Validation of what?

carob_script <- function(path) {

"This dataset contains information from variety calibration trials in 4 locations (BUK, Dambatta, Samaru and Lere). The data was collected from two separate sets of experiments. Experiment 1 was under full irrigation conducted from 23rd March to 20th June 2016 accross the 4 locations. Experiment 2 was conducted in the rainy season with supplementary irrigation from 23rd June to 10th November 2016. The data presented here is for the dry season trials. Details can be requested

This dataset contains information from on-farm validation trials conducted across 30 farmer fields in Local government areas of the northern guinea savanna AEZ in Kano. In each farmer field 10 plots (30 meter square each) were planted with a different combination of early maturing maize (10 varieties) under 3 different sowing densitie. Each plot is a different combination of variety and sowing data. Data was collected on tops weight at anthesis, cob yield and stover yield
"

	uri <- "hdl:11529/10548312"
	group <- "??" 
	ff  <- carobiner::get_data(uri, path, group)

	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institutions = "CIMMYT",
		publication= NA,
		project="TAMASA",
		data_type= "experiment",
		carob_contributor= "Shumirai Manzvera",
		carob_date="2024-05-20"
	)
	
	f0 <- ff[basename(ff) == "NG_VT_Calibration_2016.xlsx"]
	r0 <- carobiner::read.excel(f0, sheet="Data")
	f1 <- ff[basename(ff) == "NG_VT_NGS_2016 (1).xlsx"]
	r1 <-carobiner::read.excel(f1, sheet="Revised")
	f2 <- ff[basename(ff) == "NG_VT_Validation_SS_2016.xlsx"]
	r2 <-carobiner::read.excel(f2, sheet="DATA")


## select the variables of interest and assign them to the correct name
	d0 <- data.frame(
	
		yield = r0$GY_ha,
		variety = r0$Variety,
		rep = as.integer(r0$Rep),
		location = r0$Location,
		silking_days=r0$DTS,
		emergence_days=r0$DTE,
		tassling_days=r0$DTT
		# etc
	)

	
#### about the data #####

	d0$irrigated <- TRUE

##### Location #####
	
	d0$adm1[d0$location=="LERE"]<- "Kaduna"
	d0$adm1[d0$location=="SMR"]<- "Kaduna"
	d0$adm1[d0$location=="BUK"]<- "Kano"
	d0$adm1[d0$location=="DBT"]<- "Kano"
	
## each site must have corresponding longitude and latitude
	d0$longitude[d0$location=="LERE"] <-  8.57286
	d0$latitude [d0$location=="LERE"]<- 10.38584
	d0$longitude[d0$location=="SMR"] <-  8.383
	d0$latitude[d0$location=="SMR"] <-  9.750
	d0$longitude[d0$location=="BUK"] <- 12.383 
	d0$latitude[d0$location=="BUK"] <-  11.233
	d0$longitude[d0$location=="DBT"] <-  12.4382197
	d0$latitude[d0$location=="DBT"] <-  8.6160674
	
##### Time #####

	d0$planting_date <- as.character(as.Date(  "2016/03/23" ))
	d0$harvest_date  <- as.character(as.Date(  "2016/06/20" ))
	
	d1 <- data.frame(
	  
	  yield = r1$GY_ha,
	  variety= r1$Variety,
	  rep= NA,
	  silking_days=NA,emergence_days=NA,tassling_days= NA,irrigated=NA,planting_date= NA,
	  harvest_date= NA,
	  adm3=r1$LGA
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
	 
	d <- rbind(d2,d1,d0)

	d$crop <- "maize"
	d$yield_part <- "grain"
	d$country <- "Nigeria"
	
# all scripts must end like this
	carobiner::write_files(path, dset, d)
}

