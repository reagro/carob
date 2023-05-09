# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:

    A set of 87 bibliographic references was collected in order to compare yields in “CT” (conventional tillage) and in “CA” (conservation agriculture) in a meta-analysis, from experiments in Sub-Saharan Africa. Data were collected from 1974 to 2017. We had 1071 observations in total in our file. Six of the crops in the file were subject of the meta-analysis: cotton, cowpea, maize, rice, sorghum and soybean. (2020-06-12)

"

	uri <- "doi:10.18167/DVN1/DLTQWR"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   project=NA,
	   uri=uri,
	   publication= "doi:10.1038_s43016-020-0114-x",
	   data_institutions = "CIRAD",
	   carob_contributor="Eduardo Garcia Bendito",
	   experiment_type="meta-analysis",
	   has_weather=TRUE,
	   has_soil=TRUE,
	   has_management=TRUE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)
	dset$license <- carobiner::get_license(js)

	f <- ff[basename(ff) == "Donnees_meta-analyse_2020.txt"]

	# Handling unknown-8bit charset
	r <- read.table(f, sep = "\t", quote = "")
	colnames(r) <- r[1,]
	r <- r[2:nrow(r),]
  # Reshape dataset to long
	rr <- reshape(r, direction='long', 
	              varying=c('Grain_yield_CT', 'Grain_yield_CA'), 
	              timevar='tillage',
	              times=c('CT', 'CA'),
	              v.names=c('yield'),
	              idvar='observation')

## process file(s)

#### about the data #####

	d <- data.frame("on_farm" = ifelse(rr$Study_type == "on-farm", TRUE, FALSE))
	d$dataset_id <- dataset_id
	d$trial_id <- paste0(d$dataset_id, "_", rr$ref)
## the treatment code	
	d$treatment <- paste0("N", ifelse(as.numeric(gsub(",", ".", rr$Applied_N)) > 0, paste0("N", floor(as.numeric(gsub(",", ".", rr$Applied_N)))), 0),
	                      "P", ifelse(as.numeric(gsub(",", ".", rr$Applied_P)) > 0, paste0("P", floor(as.numeric(gsub(",", ".", rr$Applied_P)))), 0),
	                      "K0")


##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- gsub("^.*\\.", "", gsub(".*,","", rr$Site))
	d$site <- gsub("<92>", "'", gsub("(.*),.*", "\\1", rr$Site))
	d$elevation <- round(as.numeric(rr$Altitude), 0)
## each site must have corresponding longitude and latitude
	# Process coordinates
	rr$Coordinates <- gsub("  ", " ", gsub('"', " ", gsub("<b4>", "'", gsub("\\?.*", "'", gsub("<92>", "'", gsub("<a0>", "\\1", gsub("o", "°", gsub(" °", "°", gsub("<b0>", "°", rr$Coordinates)))))))))
	rr$Lon <- gsub(".*\\;", "\\1", gsub(".*\\,", "\\1", rr$Coordinates))
	rr$Lat <- gsub("^(.*?);.*", "\\1", gsub("^(.*?),.*", "\\1", rr$Coordinates))
	rr$Lon.deg <- as.integer(gsub("^(.*?)°.*", "\\1", rr$Lon))
	rr$Lon.deg <- ifelse(grepl("W", rr$Lon), as.integer(paste0("-", rr$Lon.deg)), as.integer(rr$Lon.deg))
	rr$Lon.min <- as.integer(gsub("(.*)'.*", "\\1", gsub(".*°", "" ,rr$Lon)))
	rr$Lon.min <- ifelse(grepl("W", rr$Lon), as.integer(paste0("-", rr$Lon.min)), as.integer(rr$Lon.min))
	rr$Lon.min <- ifelse(!is.na(rr$Lon.min), rr$Lon.min, 0)
	rr$Lon.sec <- as.integer(gsub("[^0-9.-]", '', gsub(".*'", "\\1", rr$Lon)))
	rr$Lon.sec <- ifelse(grepl("W", rr$Lon), as.integer(paste0("-", rr$Lon.sec)), as.integer(rr$Lon.sec))
	rr$Lon.sec <- ifelse(!is.na(rr$Lon.sec), rr$Lon.sec, 0)
	rr$Lat.deg <- as.integer(gsub("^(.*?)°.*", "\\1", rr$Lat))
	rr$Lat.deg <- ifelse(grepl("S", rr$Lat), as.integer(paste0("-", rr$Lat.deg)), as.integer(rr$Lat.deg))
	rr$Lat.min <- as.integer(gsub("(.*)'.*", "\\1", gsub(".*°", "" ,rr$Lat)))
	rr$Lat.min <- ifelse(grepl("S", rr$Lat), as.integer(paste0("-", rr$Lat.min)), as.integer(rr$Lat.min))
	rr$Lat.min <- ifelse(!is.na(rr$Lat.min), rr$Lat.min, 0)
	rr$Lat.sec <- as.integer(gsub("[^0-9.-]", '', gsub(".*'", "\\1", rr$Lat)))
	rr$Lat.sec <- ifelse(grepl("S", rr$Lat), as.integer(paste0("-", rr$Lat.sec)), as.integer(rr$Lat.sec))
	rr$Lat.sec <- ifelse(!is.na(rr$Lat.sec), rr$Lat.sec, 0)
	d$longitude <- round(rr$Lon.deg + (rr$Lon.min/60) + (rr$Lon.sec/60), 3)
	d$latitude <- round(rr$Lat.deg + (rr$Lat.min/60) + (rr$Lat.sec/60), 3)
	# Certain locations need to be geocoded manually 
	d$longitude[grep("Holeta", rr$Site, fixed = T)] <- 38.504
	d$latitude[grep("Holeta", rr$Site, fixed = T)] <- 9.067
	d$longitude[grep("Chuka", rr$Site, fixed = T)] <- 37.652
	d$longitude[grep("Bugesera", rr$Site, fixed = T)] <- 30.250
	d$longitude[grep("Bertoua", rr$Site, fixed = T)] <- 13.678165
	d$longitude[grep("Meki", rr$Site, fixed = T)] <- 38.839832
	d$longitude[grep("Ambohitsilaozana", rr$Site, fixed = T)] <- 48.471
	d$latitude[grep("Ambohitsilaozana", rr$Site, fixed = T)] <- -17.699
	d$longitude[grep("Murewha", rr$Site, fixed = T)] <- 31.763
	d$latitude[grep("Murewha", rr$Site, fixed = T)] <- -17.637
	d$longitude[grep("Andranomanelatra", rr$Site, fixed = T)] <- 47.105
	d$latitude[grep("Andranomanelatra", rr$Site, fixed = T)] <- -19.779



##### Crop #####
## normalize variety names
	d$crop <- tolower(rr$Crop)
	d$intercrops <- NA
	d$intercrops[grep("hanfets (wheat+barley)", tolower(rr$Crop), fixed = T)] <- "barley"
	d$crop[grep("hanfets (wheat+barley)", tolower(rr$Crop), fixed = T)] <- "wheat"
	d$crop[grep("tef", tolower(rr$Crop), fixed = T)] <- "teff"
	d$crop[grep("millet", tolower(rr$Crop), fixed = T)] <- "pearl millet"
	d$crop[grep("bean", tolower(rr$Crop), fixed = T)] <- "common bean"
	
## EGB
# crop_rotation and intercrops are included but only as binary legume...
	# d$intercrops <- ifelse(rr$Legume_intercropping == "Y", "common bean", NA)
	# d$crop_rotation <- ifelse(rr$Legume_rotation == "Y", "common bean", NA)

##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	year <- substr(gsub("[^0-9.-]", "", rr$Year), 1, 4)
	d$start_date <- as.character(format(as.Date(strptime(ifelse(year == "", NA, year), "%Y")), "%Y"))
	d$season <- ifelse(grepl("LR", rr$Year), "1",
	                   ifelse(grepl("SR", rr$Year), "2",
	                          ifelse(grepl("M", rr$Year), "1",
	                                 ifelse(grepl("V", rr$Year), "2",
	                                        ifelse(grepl("masika", rr$Year), "1",
	                                               ifelse(grepl("vuli", rr$Year), "2", NA))))))

##### Fertilizers #####
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- as.numeric(gsub(",", ".", rr$Applied_P))
   d$K_fertilizer <- 0
   d$N_fertilizer <- as.numeric(gsub(",", ".", rr$Applied_N))
## normalize names 
   d$fertilizer_type <- "unknown"
   d$inoculated <- FALSE
   

##### in general, add comments to your script if computations are
##### based in information gleaned from metadata, publication, 
##### or not immediately obvious for other reasons

##### Yield #####
   d$yield <- as.numeric(gsub(",", ".", rr$yield))
   
##### Soil #####
   d$soil_type <- tolower(trimws(rr$Soil_NRCS))
   d$soil_SOC <- as.numeric(gsub(",", ".", ifelse(rr$Initial_soil_C == "?", NA, rr$Initial_soil_C)))/10 # g/kg -> %
   
##### Tillage #####   
  d$tillage <- ifelse(rr$tillage == "CT", rr$CT_type, rr$CA_type1)

# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)
}
