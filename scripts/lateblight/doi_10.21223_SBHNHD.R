# R script for "carob"


carob_script <- function(path) {

"
Dataset for yield and stability advanced trial for late blight and heat tolerant (LBHT) potato population conducted in Huancayo, Peru. 150 advanced clones of the LBHT and heat-tolerant population, with three control varieties Yungay, Kory, and Amarilis, besides with 23 parents were planted in Huancayo, Peru between 2021 and 2022. (16 Rows x 12 Columns)
"

### Identifiers
	uri <- "doi:10.21223/SBHNHD"
	group <- "lateblight"

## Download data 
	ff  <- carobiner::get_data(uri, path, group)

### metadata 
	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=1),
		data_institute = "CIP",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "variety_code", 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2024-07-04"
	)
	
### PROCESS records

	f <- ff[basename(ff) == "01_data_Advanced Trial LBHTC2 Huancayo.xlsx"]
	
	r <- carobiner::read.excel(f)

## select the variables of interest and assign them to the correct name
	lbvars <- grep('^LB', colnames(r), value=TRUE)
	d <- data.frame(
	  country="Peru",
	  crop="potato",
	  variety_code= r$CIPN,
	  variety_type= r$Type,
	  variety_traits=r$PGH,
	  treatment=r$Treatment,
	  rep= as.integer(r$Rep),
	  p_harv=r$PltHrv,
	  yield=r$MTbWg,
	  dym_storage=r$`Dry weight`/1000,
	  on_farm= TRUE,
	  inoculated= FALSE,
	  irrigated= TRUE,
	  herbicide_used= TRUE,
	  herbicide_product = "diuron;metribuzin",
	  yield_part= "tubers",
	  record_id=r$Plot,
	  trial_id= "1"         
	)
	
	d$location <- "Huancayo"
	d$longitude <- -75.2047
	d$latitude <- -12.0650
	
	d$planting_date <- as.character(as.Date("2021-12-25"))
	d$harvest_date  <- as.character(as.Date("2022-05-25"))
  d$irrigation_dates <- as.character(as.Date("2021-12-29"))
  d$herbicide_dates <- as.character(as.Date(c("2021-11-21", "2021-12-06", "2022-03-22")))
  
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
  
  #Adding disease scores for late blight
  dd <- r[,lbvars]
  dd$record_id <- as.integer(1:nrow(dd))
  dates <- as.character(as.Date(c("2022-01-10", "2022-01-17", "2022-01-24", "2022-01-31", "2022-02-07",  "2022-02-14", "2022-02-21","2022-02-28","2022-03-07",  "2022-03-14", "2022-03-21")))
  x <- reshape(dd, direction="long", varying =lbvars, v.names="disease_severity", timevar="step")
  x$time <- dates[x$step]
  x$step <- x$id <- NULL 
  
  d <-merge(d, x, by="record_id", all.x = TRUE)
  
  d$pathogen <- "Phytophthora infestans"
  d$disease <- "potato late blight"
  d$record_id <- as.integer(d$record_id)
  d$treatment <- as.character(d$treatment)
  d$disease_severity <- as.character(d$disease_severity)

	carobiner::write_files(path, meta, d)
}



