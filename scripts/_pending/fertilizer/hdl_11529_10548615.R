# R script for "carob"

## ISSUES
# 1. quadrant size used to measure area is unknown 
# 2. biomass yield not stated whether fresh or dry
# 3. dates (years) got large values
# 4. low yield because of lack of units of measurement


carob_script <- function(path) {

"Description:

    [The objective of this survey was to identify the major wheat productivity and profitability 
  drivers in Nepal Terai. Nepal Terai is considered as the part of Indo-Gangetic Basin and its the major 
  cereal production domain in Nepal. In year 2016, immediately following the wheat harvest, this survey was
  conducted. A total of 10 districts were purposively selected for the survey based on the highest wheat 
  acreage. In each district, a total of five sub-districts were further selected purposively based on the 
  highest wheat acreage. In each sub-districts (or village development committees: VDCs) a total of 10 wheat 
  producing farms were selected randomly from the farmers name list provided by the village level administrative 
  authorities. The overall sampling frame consists of 10 samples from each VDC × 5 sub-districts (VDCs) in each 
  district × 10 districts in Nepal Terai = 500 samples. Moreover, in order to check the farmers self reported
  yield, 50% farmers largest plots were selected for the crop cuts and the crop cuts data are also available with
  is dataset. These crop cuts were conducted prior to the survey – during the time when farmers harvest their 
  wheat crop. Farmers may have multiple plots and asking the data from each plot may reduce the data quality.
  Therefore, to increase the precision and collect the quality data, farmers inputs (e.g., seed, fertilizer,
  weed management practices, varieties, sowing time, harvesting time, and other crop management practices), 
  and outputs were asked only for largest plot. Data were collected through a direct farm visit and paper-based
  survey was deployed. The details of the questions asked, codebook, their description, meta-data, and data can
  be found in this dataset. (2021-09-24)]"
  

	uri <- "hdl:11529/10548615"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Gokul P. Paudel; Andrew J. McDonald, 2021, Data on identifying sustainable wheat productivity drivers in Nepal’s Terai, https://hdl.handle.net/11529/10548615, CIMMYT Research Data & Software Repository Network, V1, UNF:6:w5/k07NPArxJgX7NKnRMTA== [fileUNF]",
		publication= NA,
		data_institutions = "CIMMYT",
   		data_type="survey", 
		carob_contributor="Blessing Dzuda",
		carob_date="2023-12-12"
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)


	#process data
	
	f <- ff[basename(ff) == "tbl14_cropcut_data_2016.dta"]
	d <- haven::read_dta(f)
	

	d$dataset_id <- dataset_id
	d$is_survey <- TRUE
	d$trial_id <- as.character(d$q01_hhid)
	#d$planting_date <- as.Date(d$q02_dos, format = "%m-%d-%y") #Cliffoe08: dates contains wrong entries of the year
	 
	  
	  ##### Location #####
	d$country <- "Nepal"
	d$adm1 <- d$q03_devreg 
	d$adm2 <- d$q04_dist
  d$adm3 <- d$q13_wardno
	d$adm4 <- d$q14_tolvillname
	
	d$adm1 <- carobiner::replace_values(d$adm1,1:5,c("Eastern", "Central", "Western", "Midwest", "Farwest"))
	
	d$adm2 <- carobiner::replace_values(d$adm2,1:10,c("Sunsari","Saptari","Dhanusa","Bara","Nawalparasi"
	                                             ,"Rupandehi","Banke","Bardiya","Kailali","Kanchanpur"))
	
	l <- data.frame(country = c("Nepal", "Nepal", "Nepal", "Nepal", "Nepal", "Nepal", "Nepal", 
	                            "Nepal", "Nepal", "Nepal"), 
	                adm2 = c("Sunsari", "Saptari", "Dhanusa", "Bara", "Rupandehi", 
	                         "Nawalparasi", "Banke", "Bardiya", "Kailali", "Kanchanpur"),
	                longitude = c(87.1378, 86.7494, 85.9332, 85.0693, 83.4034, 83.7363, 81.7714, 
	                              81.4515, 80.9037, 86.9148), 
	                latitude = c(26.641, 26.6072,26.7217, 27.1073, 27.5488, 27.5225, 28.0961,
	                             28.3715,28.7315, 26.6416))
	
	d<-merge(d,l, by=c("country","adm2"),all.x = TRUE)
	
	
	  ##### Crop #####
	d$crop <- "wheat"
	 
	  
	  ##### Yield #####
	d$dmy_total <- d$q24_tagbq2
	  
	d$yield <- d$q25_grainyldq2 #data provided is not specific on units
	  
	d$yield_part <- "grain"
	  
	  
	d <- d[,c("dataset_id","country","adm1","adm2","adm4","latitude","longitude","trial_id","is_survey",
	          "crop","dmy_total","yield_part","yield")]
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
 }

