# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
This experiments were established with different rates of nitrogen in order to generate a wide range of values for NDVI and grain yield in order to develop a calibration model for the GreenSeeker in Yaqui Valley. (2022-10-17)
"

#### Identifiers
	uri <- "hdl:11529/10548811"
	group <- "wheat_trials"

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)

##### dataset level metadata 
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=2, minor=0),
		data_institute = "CIMMYT",
		publication = NA,
		project = "GREENSEEKER CALIBRATION",
		data_type = "experiment",
		treatment_vars = "N_fertilizer", 
		carob_contributor = "Blessing Dzuda",
		carob_date = "2024-06-25"
	)
	
##### PROCESS data records

# read data 
	sort_data <- function(sheetname) {
	  
	f <- ff[basename(ff) == "GreenSeeker Sonora 2000-2014.xlsx"]
  r <- carobiner::read.excel(f, sheet = sheetname,skip = 1, col_names = TRUE)
  names(r)[13] <- 'Rate N (kg/ha)' #changing format of column names
  names(r)[34] <- 'Yield at 12% hum' #changing format of column names

	d1 <- data.frame(
	     trial_id="1",
	     country="Mexico",
		   adm2=r$Municipality,
		   site=r$Locality,
		   longitude="-102.5528",
		   latitude="23.6345",
		   crop="wheat",
		   planting_date=r$`Planting Date`,
		   land_prep_method=r$Tillage,
		   variety=r$Hibrid,
		   planting_method=r$`Planting method`,
		   #plant_density=r$`planting density (Kg/ha)`-need for conversion from kg/ha to plants/ha
		   rep=r$REP,
		    treatment=r$`Number of Treatment`,
		    Zn_fertilizer=r$`Rate Zn`,
 		   N_fertilizer=r$`Rate N (kg/ha)`,
		   yield_part="grain",
		   yield=r$`Yield at 12% hum`
	)

	}
	d1 <- sort_data("2000-2001")
	d1$planting_date <- as.numeric(d1$planting_date)
	d1$planting_date <- as.Date(d1$planting_date,origin = "1904-01-01" )
	d2 <- sort_data("2001-2002")
	d3 <- sort_data("2004-2005")
	d4 <- sort_data("2005-2006")
	d5 <- sort_data("2006-2007")
	d6 <- sort_data("2007-2008")
	d7 <- sort_data("2008-2009")
	d8 <- sort_data("2009-2010")
	d9 <- sort_data("2013-2014")
	
	
	r0 <- carobiner::read.excel(f, sheet = "2010-2011",skip = 1, col_names = TRUE)
	names(r0)[13] <- 'Rate N (kg/ha)' #changing format of column names
	names(r0)[32] <- 'Yield at 12% hum' #changing format of column names
	
	d10 <- data.frame(
	  trial_id="1",
	  country="Mexico",
	  adm2=r0$Municipality,
	  site=r0$Locality,
	  longitude="-102.5528",
	  latitude="23.6345",
	  crop="wheat",
	  planting_date=r0$`Planting Date`,
	  land_prep_method=r0$Tillage,
	  variety=r0$Hibrid,
	  planting_method=r0$`Planting method`,
	  #plant_density=r$`planting density (Kg/ha)`-need for conversion from kg/ha to plants/ha
	  rep=r0$REP,
	  treatment=r0$`Number of Treatment`,
	  Zn_fertilizer=r0$`Rate Zn`,
	  N_fertilizer=r0$`Rate N (kg/ha)`,
	  yield_part="grain",
	  yield=r0$`Yield at 12% hum`
	)
	
	d <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10)
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- TRUE

  d$land_prep_method <- gsub("Conservation", "reduced tillage",d$land_prep_method)
  d$land_prep_method <- gsub("Conventional", "conventional",d$land_prep_method)
  d$land_prep_method <- gsub("-", "unknown",d$land_prep_method)
  d$planting_method <- gsub("-", "unknown",d$planting_method)
  d$planting_method <- gsub("Basin irrigation", "basin",d$planting_method)
  d$planting_method <- gsub(" Bed", "raised beds",d$planting_method)
  d$planting_method <- gsub("Bed (burn straw)","raised beds",d$land_prep_method)
  d$planting_method <- gsub("Bed (incorporated straw wheat + maize)", "raised beds",d$land_prep_method)
  d$planting_method <- gsub("Bed (remove straw)", "conventional tilled beds",d$planting_method) 
  d$planting_method <- gsub("Bed (retain straw)", "raised beds", d$planting_method)
  d$planting_method <- gsub("Basin irrigation", "basin",d$planting_method)
  d$longitude <- as.numeric(d$longitude)
  d$latitude <- as.numeric(d$latitude)
  d$planting_date <- as.character(d$planting_date)
  d$rep <- as.integer(d$rep)
  d$Zn_fertilizer <- as.numeric(d$Zn_fertilizer)
  d$N_fertilizer <- as.numeric(d$N_fertilizer)
  
  # all scripts must end like this
	carobiner::write_files(path, dset, d)
	
}
