# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
  Description:
    
    TAMASA Ethiopia. Yield, soil and agronomy data from 70 farmers’ maize fields in Bako, Ethiopia, 2015 season.
  Version 1.1
  
  
  "

  uri<- "hdl :11529/11020"
  dataset_id <- carobiner::simple_uri(uri)
  group <-  "crop_cuts"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project= "CIMMYT",
    uri=uri,
    data_citation= NA,
    ## if there is a paper, include the paper's doi here
    ## also add a RIS file in references folder (with matching doi)
    publication= NA,
    data_institutions = "CIMMYT",
    data_type="survey",
    carob_contributor="Shumirai Manzvera"  
  )
  
  ## download and read data 
 
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  dset$license <- carobiner::get_license(js)
  
  
 
  ##TAMASA_ET_CC_2015_BakoF <- read_excel("scripts/crop_cuts/data/raw/crop_cuts/hdl_11529_10548392/TAMASA_ET_CC_2015_BakoF.xlsx", 
  #                                   +     sheet = "Raw_Data")
  r<- TAMASA_ET_CC_2015_BakoF
  #	r <- readxl::read_excel(f)
  #	r <- readxl::read_excel(f) |> as.data.frame()
  
  
  ## process file(s)
  
  ## use a subset
  d <- r
  
  
  #### about the data #####
  ## (TRUE/FALSE)
  
  d$dataset_id <- dataset_id
  d$on_farm <- FALSE
  d$is_survey <- TRUE
  d$is_experiment <- FALSE
  d$irrigated <- FALSE
  
  
  ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  d$country <-"Ethiopia"
  d$adm1 <- "Oromia"
  d$adm2 <- "West Showa"
  d$adm3 <- "Bako"
  d$adm4 <- d$`Name of the Village`
  d$elevation <- NA
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
  d$longitude <- 35.52-42.12
  
  d$latitude <-  6.58-11.77
  
  ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  d$crop <- "maize"
  #variety
  d$variety<-d$`Type of Variety`
  #farming type 
  d$intercrops <- d$`Cropping System`
  d$intercrops<-d$`Intercropping with legume`
  d$croprotation<-d$`Crop Rotation with`
  d$crop_rotation<-d$croprotation
  d$previous_crop<-d$`Previous/precursor crop`
  
  ##### Fertilizers #####
  
  ## normalize names 
  d$fertlizer_type <- d$`Type of Inorganic Fertilizer`
  d$inoculated <- FALSE
  d$OM_used<-d$`Apply Organic Fertilizer ?`
  d$OM_type<-d$`Type of Organic Fertilizer applied`
  d$soil_type<-d$`Soil type`
  d$soil_pH<-d$pH
  d$soil_K<-d$`K (mg kg-1)`
  d$soil_Mg<-d$`Mg (mg kg-1)`
  d$soil_Ca<-d$`Ca (mg kg-1)`
  
  
  ##### Yield #####
  d$biomass_total <- 
    
    d$yield <- d$`Average yield kg/ha or (Q1+Q2)/2`
  #what plant part does yield refer to?
  d$yield_part <-  d$`Dry wt of cobs in (4mX4m) Q1`+d$`Dry wt of cobs /Q2`
  
  
  # all scripts must end like this
  carobiner::write_files(dset, d, path=path)
  
}

  
  