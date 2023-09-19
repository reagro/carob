# R script for "carob"

# ## ISSUES
"Grain yield not specified if it is kg/ha,
 CP2 is not explicitly defined under column treatment in carob

"

path <- getwd()
carob_script <- function(path) {
  
  "Description:

Grain yield data collected from Conservation Agriculture (CA) systems across experiments of varying experimental duration, established in trial locations of Malawi, Mozambique, Zambia, and Zimbabwe under an increasingly variable climate. Data contains different agro-environmental yield response moderators such as type of crop diversifcation and amount of rainfall and aims to identify cropping systems that may provide both short-term gains and longer-term sustainability.
"
  
  uri <- "hdl:11529/10548832"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "maize_trials"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    data_citation="Thierfelder, Christian (CIMMYT) - ORCID: 0000-0002-6306-7670,Mhlanga, Blessing (CIMMYT) - ORCID: 0000-0003-4587-795X",
    ## if there is a paper, include the paper's doi here
    ## also add a RIS file in references folder (with matching doi)
    publication= NA,
    data_institutions = "CIMMYT",
    data_type="experiment",
    carob_contributor="Fredy chimire" 
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)
  dset$license <- carobiner::get_license(js)
  
  
  f <- ff[basename(ff) == "DATA SA 2005 to 2019.xls"]
  
  # Select sheeet with revised data from the excel file 
  d <- carobiner::read.excel(f, sheet = "Raw Data")
  
  ## process file(s)
  
  # selecting columns of interest which match the carob standard format
  d <- d[,c(	"Location","Season","System","Rep","Clay","Sand","OrgC","Biomass","Grain" )]
  
  d$dataset_id <- dataset_id
  d$on_farm <- FALSE
  d$is_survey <- FALSE
  d$is_experiment <- TRUE
  d$irrigated <- FALSE
  ## the treatment code	
  #d$treatment <- 
  #d$rep
  
# Assign country names based on location on which experiment was made
  
  country_mapping <- c("CRS" = "Malawi", "DTC" = "Zimbabwe", "HRS" = "Zimbabwe",
                       "MFTC" = "Zambia","MRS" = "Zambia","SRS" = "Mozambique")
  
  d$country <- country_mapping[d$Location]
  
  d$date <- d$Season
  
  d$rep <- d$Rep
  d$soil_clay <- d$Clay
  d$soil_sand <- d$Sand
  d$soil_SOC <- d$OrgC
  d$yield_part <- "grain"
  d$yield <- d$Grain 
  d$rain <- d$Rainfall
  
  # Get gps coordinates
   lattitude_mapping <- list("CRS"=c(-13.97738,33.64887) ,"DTC"=c(-17.6091, 31.1377),
                             "HRS"=c(-17.5585947, 30.9814704),"MFTC"=c(-16.2402, 27.44145),
                             "MRS"=c(-13.645, 32.5585),"SRS"=c(-19.4112,33.2947))
   
   "Sources for gps coordinates"
   
   #https://malawi.worldplaces.me/places-in-chitedze/56585145-chitedze-research-station.html
   #https://glten.org/experiments/300)
   #https://vymaps.com/ZW/Henderson-Research-Station-394000277403139/#google_vignette
   #https://glten.org/experiments/14
   #https://glten.org/experiments/311
   #http://wheatatlas.org/station/MOZ/10706\
   
   # Map geo coordinates to dataframe
   d$latitude <- unlist(lapply(d$Location, function(loc) lattitude_mapping[[loc]][1]))
   d$longitude <- unlist(lapply(d$Location, function(loc) lattitude_mapping[[loc]][2]))
   
   # Rename Location codes with actual names
   
   location_names <- c("CRS" = "Chitedze Research Station", "DTC" = "Domboshawa Training Centre",
                       "HRS" = "Hennderson Research Station", "MFTC" = "Monze Farmer Training Centre",
                       "MRS" = "Msekera Research Station","SRS" = "Sussundenga Research Station")
   
   d$location <- location_names[d$Location] #carob standard name
   
   # We need to replace treatment codes with actual names
   treat_names <- c("BA"="basins","CP"= "conventional ploughing","DiS"="dibble stick","DiS-MC"="dibble stick and maize-cowpea rotation",
                   "DiS-M+C"="dibble stick and maize-cowpea intercrop", "DiS-M+Mp"="dibble stick and maize-velvet bean intercrop","DiS-M+Pp",
                   "DS"= "dibble stick and maize-pigeonpea intercrop","DS-MG"="direct seeding and maize-groundnut rotation","DS-MSf"="direct seeding and maize-sunflower-cotton rotation", 
                   "DS-M+C"="direct seeding and maize-cowpea intercrop","DS-MBio"="direct seeding and maize and biochar","RI"="ripping","RI-M+C"="ripping and maize-cowpea intercrop",
                   "DS-MCt"= "direct seeding and maize-cotton rotation", "DS-MCtS"="direct seeding and maize-cotton-sunhemp rotation" ,"CP-MCt"="conventional ploughing and maize-cotton rotation",
                   "DS-MC"="direct seeding and maize-cowpea rotation","DS-MSy"="direct seeding and maize-soyean rotation","DS-MSfC"= "direct seeding and maize-sunflower-cotton rotation",
                   "DS-M+Pp"="irect seeding and maize-pigeonpea intercrop",  "JP"="jab planter","CP2"="CP2")
   
   d$treatment <- treat_names[d$System]
   
 
  
  # all scripts must end like this
  carobiner::write_files(dset, d, path=path)
}



