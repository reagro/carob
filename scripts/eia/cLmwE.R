# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
SOME DESCRIPTION GOES HERE...
"

#### Identifiers
	uri <- "0000000000"
	group <- "eia"

#### Download data 
	ff <- list.files(paste0(getwd(), '/data/raw/', group, '/', uri), full.names = TRUE)

##### dataset level metadata 
	dset <- data.frame(
	  # Need to fill-in metadata...
	  # carobiner::read_metadata(uri, path, group, major=2, minor=0),
	  uri = uri,
	  dataset_id = uri,
	  authors = 'Some names here...',
	  title = 'Rwanda RAB Rice Data',
	  description = 'Some description here...',
	  group = group,
	  license = 'Some license here...',
	  carob_contributor = 'Someone...',
	  data_citation = '...',
	  project = 'Excellence in Agronomy',
	  data_type = "Experiment", # or, e.g. "on-farm experiment", "survey", "compilation"
	  carob_date="2024-04-22"
	)
	
##### PROCESS data records

# read data 

	f1 <- ff[basename(ff) == "AfricaRice_2015.csv"]
	r1 <- read.csv(f1)
	f2 <- ff[basename(ff) == "OFRA.csv"]
	r2 <- read.csv(f2)
	f3 <- ff[basename(ff) == "Rice_2022A.csv"]
	r3 <- read.csv(f3)
	f4 <- ff[basename(ff) == "SAnDMan_Rice_fieldData.RDS"]
	r4 <- readRDS(f4)

## process file(s)

## use a subset
	d1 <- data.frame(trial_id = r1$ID,
	                 country = r1$Country,
	                 location = r1$Village,
	                 site = r1$SiteName,
	                 longitude = as.numeric(r1$Longitude),
	                 latitude = as.numeric(r1$Latitude),
	                 planting_date = as.character(as.Date(r1$DateSeeding, "%m/%d/%Y")),
	                 harvest_date = as.character(as.Date(r1$HarvestingDate, "%m/%d/%Y")),
	                 crop = tolower(r1$Crop),
	                 variety = r1$Variety,
	                 N_fertilizer = r1$Nrate,
	                 P_fertilizer = r1$Prate,
	                 K_fertilizer = r1$Krate,
	                 yield = r1$Yield * 1000,
	                 land_prep_method = ifelse(r1$Tillage == "CT", "conventional", NA))
	
	d1 <- d1[d1$country == "Rwanda",]
	
	d2 <- data.frame(country = "Rwanda",
	                 trial_id = r2$TLID,
	                 longitude = as.numeric(r2$lon),
	                 latitude = as.numeric(r2$lat),
	                 planting_date = as.character(substr(r2$season, start = 1, stop = 4)),
	                 season = trimws(substr(r2$season, start = 5, stop = nchar(r2$season))),
	                 crop = "rice",
	                 N_fertilizer = as.numeric(r2$N),
	                 P_fertilizer = as.numeric(r2$P),
	                 K_fertilizer = as.numeric(r2$K),
	                 yield = r2$TY * 1000)
	
	d3 <- data.frame(country = "Rwanda",
	                 trial_id = rep(1:(nrow(r3)/12), each = 12),
	                 longitude = as.numeric(r3$lon),
	                 latitude = as.numeric(r3$lat),
	                 planting_date = as.character(as.Date(r3$PlantingDate, "%d/%m/%Y")),
	                 harvest_date = as.character(as.Date(r3$HarvestDate, "%d/%m/%Y")),
	                 crop = "rice",
	                 N_fertilizer = as.numeric(r3$N),
	                 P_fertilizer = as.numeric(r3$P),
	                 K_fertilizer = as.numeric(r3$K),
	                 lime = as.numeric(r3$Lime),
	                 yield = (r3$FreshWeightPrimary)/(r3$NetplotArea/10000), # Assuming that the measurement is in kg/m2
	                 dmy_residue = (r3$dryWeightsecondaryProduct)/(r3$NetplotArea/10000))
	
	d4 <- data.frame(country = "Rwanda",
	                 trial_id = r4$TLID2,
	                 longitude = as.numeric(r4$lon),
	                 latitude = as.numeric(r4$lat),
	                 planting_date = as.character(as.Date(r4$plantingDate, "%Y-%m-%d")),
	                 harvest_date = as.character(as.Date(r4$harvestDate, "%Y-%m-%d")),
	                 crop = "rice",
	                 # N_fertilizer = r4$Nrate,
	                 # P_fertilizer = r4$Prate,
	                 # K_fertilizer = r4$Krate,
	                 yield = r4$grainsFW/(r4$plotSize/10000) # Assuming that the measurement is in kg/m2
	                 )
	
	d <- carobiner::bindr(d1,d2,d3,d4)
	
	d$yield_part <- "grain"
	d$latitude <- ifelse(d$latitude > 0, d$latitude*-1, d$latitude)
	d$dataset_id <- uri
	
# all scripts must end like this
	carobiner::write_files(path, dset, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

