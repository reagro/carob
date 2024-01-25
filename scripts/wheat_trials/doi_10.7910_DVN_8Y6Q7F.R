# R script for "carob"

## ISSUES
# dataset obtained from outside CGIAR (South Africa Agricultural Research Council)
# had to manually select the license as it was different from the function on the template

carob_script <- function(path) {

"Description:

[The authors acknowledge and thank the South Africa Agricultural Research
Council (ARC) for making the original data available for this study.
ARC data in this repository can be used to replicate a forthcoming study
on wheat yields and climate change in South Africa. Should the data be
used beyond replication, ARC must be acknowledged.]"

	uri <- "doi:10.7910/DVN/8Y6Q7F"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "wheat_trials"
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Aaron M. Shew; Jesse B. Tack; L. Lanier Nalley; Petronella Chaminuka, 2020, Replication Data for: Yield reduction under climate warming varies among wheat cultivars in South Africa, https://doi.org/10.7910/DVN/8Y6Q7F, Harvard Dataverse, V1, UNF:6:fbTkoas09MkUw1KLVuDc2g== [fileUNF]",
		publication=NA,
		data_institutions = "SARC",
   		data_type="experiment", 
		carob_contributor="Blessing Dzuda",
		carob_date="2024-01-24"
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	dset$license <- carobiner::get_license(js)
	dset$title <- carobiner::get_title(js)
	

	f <- ff[basename(ff) == "RegressionDataFinal.dta"]
	r <- haven::read_dta(f)
## use a subset
	d <- data.frame(site=r$Site, adm1=r$Province, variety=r$Cultivar, 
			rep=as.integer(r$Replicate), planting_date=r$plant_date, 
			flowering_date=r$Flower_Date, harvest_date=r$Harvest_date,
			yield = r$Yield*1000, rain=r$prec)
	
#### about the data #####

	d$dataset_id <- dataset_id
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- FALSE

##### Location #####
	#g <-unique(d[,c("country","site")])
	#g1<-carobiner::geocode(country = g$country,location = g$site,service = "nominatim")
	#manually extracting geo coordinates using dput
	#dput(g1)

## it should be possible to find the coordinates for the first three
	
	gg <- data.frame(
			site=c("LANGGEWENS", "RIEBEEKWES", "TYGERHOEK", 
				"ALPHA","ARLINGTON", "BETHLEHEM", "BULTFONTEIN", "CLARENS", "EXCELSIOR", 
	            "HOPEFIELD", "KLIPDALE", "MALMESBURY", "MOORREESBURG", 
	            "NAPIER", "PANORAMA", "PHILADELPHIA", "PIKETBERG", "POOLS", 
	            "PORTERVILLE", "PROTEM", "REITZ", "SENEKAL", "WESSELSBRON"),
	        longitude=c(NA, NA, NA, 31.0052, 27.8531, 28.3089, 26.7755, 28.4194, 
	               27.0611, 18.3498, 19.9658, 18.7231, 18.6564, 19.8919, 
	               18.5738, 18.581, 18.7575, 31.2203, 18.9957, 28.5669, 
	               28.4275, 27.6326, 26.3647),
	        latitude=c(NA, NA, NA, -27.7555, -28.0286, 
	               -28.2308, -29.1681, -28.5137, -28.9414, -33.0634, 
	               -34.305, -33.4582, -33.1553, -34.4715, -33.8795, 
	               -33.6666, -32.9042, -29.515, -33.0141, -25.7003, 
	               -27.8006, -28.3288, -27.8539))
	d <- merge(d, gg, by="site", all.x=TRUE)
	d$site <- carobiner::fix_name(d$site, "title")
	d$variety <- tolower(d$variety)

	d$planting_date <- as.character(d$planting_date)
	d$harvest_date  <- as.character(d$harvest_date)
	d$flowering_date <- as.character(d$flowering_date)
	d$trial_id <- "1"
	##### Crop #####
	d$crop <- "wheat"
	d$yield_part <-"grain"
	d$country <- "South Africa"
### avoid subsetting like this
###	d<- d[,c("trial_id","country","site","adm1","variety","rep","yield_part","crop","yield","planting_date","flowering_date",
##3	         "harvest_date", "longitude","latitude","dataset_id")]
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}


