# R script for "carob"

# 
carob_script <- function(path) {
  
  "Description:

    Data from crop-cuts as part of the Agronomy Panel Survey (APS) implemented in Oromia in three Zones (East Wollega, West Showa and Jimma). The APS included 16 peasant associations and 76 communities across the western part of Ethiopia in 56 randomly selected 1 X 1 km areas from eight 10km x 10 km sampling grids. Replicated crop cuts were made on farmers maize fields and yield measured. Soil samples were also collected but have not been analysed yet.

"
  
  uri <- "hdl:11529/11013"
  group <- "crop_cuts"
  ff <- carobiner::get_data(uri, path, group)
  ## dataset level data 
  dset <- data.frame(
  	carobiner::read_metadata(uri, path, group, major=2, minor=3),
    project="TAMASA",
    ## if there is a paper, include the paper's doi here
    ## also add a RIS file in references folder (with matching doi)
    publication= NA,
    data_institutions = "CIMMYT",
    data_type="survey",
    carob_contributor="Fredy Chimire",
    carob_date="2023-08-17"
  )
  
  
  
  
  f <- ff[basename(ff) == "TAMASA_ET_CC_2016F.xlsx"]
  
  # Select sheeet with revised data from the excel file 
  r <- carobiner::read.excel(f, sheet = "Revised_Data")
  
  #### about the data #####
    
    #d$trial_id <- d$`HH-ID`
	d <- data.frame(adm1=r$Zone, adm2=r$Districts, location=r$Kebele, site=r$Community)
	d$country <- "Ethiopia"
	
	d$on_farm <- TRUE
    d$is_survey <- TRUE
    #d$is_experiment <- FALSE
    d$irrigated <- FALSE
    ## the treatment code	
    #d$treatment <- 
    #d$rep
    
    ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
    d$on_farm <- TRUE
    d$row_spacing <- d$`Row Distance (cm)`
    d$plant_spacing <- d$`Plant Distance (cm)`
    d$yield_part <- "grain"
    d$crop <- "maize"
    d$rep <- d$`Quadrant no.`
    d$yield <- d$`Grain yield (kg/ha)`

    
	d$date=as.character(as.Date(r$Date, "%d/%m/%Y"))
        
    # Pick unique coordinates of the location
	## should use this:
    locs <- unique(d[,c("country","adm2", "location", "site")]) 
   
   
#    locs <- unique(d[,c("country","adm2")]) # adm2 is the district
#    locs <- na.omit(locs)# remove null values
    # Get the geo coordinates of the location
    # we use district level as the location
## RH that is not OK. Use the best information you have

## RH do not run carobiner::geocode in the script. 
## Run it once and store the output in the script
#    geocodes <- carobiner::geocode(country=locs$country,location=locs$adm2) 
#    geocodes1 <- geocodes$df
    
    # use this link to get gps coordinates https://www.gps-coordinates.net/
#    geocodes1$lon[geocodes1$location=="Gobu sayo"] <- 35
#    geocodes1$lat[geocodes1$location=="Gobu sayo"] <- 8.83333
    
#    geocodes1$lon[geocodes1$location=="Tiro Afeta"] <- 37.33333
#    geocodes1$lat[geocodes1$location=="Tiro Afeta"] <- 7.91667
    
#    geocodes1 <- change_names(geocodes1, c("lon", "lat"), c("longitude", "latitude")

    # Merge dataframes
#    mergeddf <- merge(d, geocodes2, by=c("country","adm2"),all.x=TRUE)

    
   carobiner::write_files(dset, d, path=path)
}

