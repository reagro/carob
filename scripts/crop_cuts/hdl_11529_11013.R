# R script for "carob"

# ## ISSUES
# ....

carob_script <- function(path) {
  
  "Description:

    Data from crop-cuts as part of the Agronomy Panel Survey (APS) implemented in Oromia in three Zones (East Wollega, West Showa and Jimma). The APS included 16 peasant associations and 76 communities across the western part of Ethiopia in 56 randomly selected 1 X 1 km areas from eight 10km x 10 km sampling grids. Replicated crop cuts were made on farmers maize fields and yield measured. Soil samples were also collected but have not been analysed yet.

"
  
  uri <- "hdl:11529/11013"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "crop_cuts"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project="TAMASA",
    uri=uri,
    data_citation="T Balemi (CIMMYT),M Kebede (CIMMYT) - ORCID: 0000-0003-4953-1585,J Chamberlin (CIMMYT) - ORCID: 0000-0001-9522-3001,B Assefa (CIMMYT),K Workneh (CIMMYT),T Abera (EIAR) - ORCID: 0000-0002-9335-9906,T Tufa (EIAR),G Hailu (EIAR),G Chala (EIAR),G Gurumu (EIAR)",
    ## if there is a paper, include the paper's doi here
    ## also add a RIS file in references folder (with matching doi)
    publication= NA,
    data_institutions = "CIMMYT",
    data_type="survey",
    carob_contributor="Fredy chimire" 
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=3)
  dset$license <- carobiner::get_license(js)
  
  
  f <- ff[basename(ff) == "TAMASA_ET_CC_2016F.xlsx"]
  
  # Select sheeet with revised data from the excel file 
  r <- carobiner::read.excel(f, sheet = "Revised_Data")
  
  #### about the data #####

    # selecting columns of interest which match the carob standard format#
#    d <- d[,c("dataset_id","rep","date","on_farm","adm1","adm2","is_survey","is_experiment","irrigated","yield","crop","location","site","country","yield_part","row_spacing","plant_spacing")]

    
    #d$trial_id <- d$`HH-ID`
	d <- data.frame(adm1=r$Zone, adm2=r$Districts, location=r$Kebele, site=r$Community)
	d$country <- "Ethiopia"
	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
    d$is_survey <- TRUE
    d$is_experiment <- FALSE
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
    d$crop <-"maize"
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

    
    # all scripts must end like this
   carobiner::write_files(dset, d, path=path)
}

