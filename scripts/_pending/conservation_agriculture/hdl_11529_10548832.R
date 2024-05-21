

# ## ISSUES
# units for grain yield not specified if it is kg/ha,
# CP2 is not explicitly defined under column treatment in carob


carob_script <- function(path) {
  
    "
  Grain yield data collected from Conservation Agriculture (CA) systems across experiments of varying
  experimental duration, established in trial locations of Malawi, Mozambique, Zambia, and Zimbabwe under 
  an increasingly variable climate. Data contains different agro-environmental yield response moderators such 
  as type of crop diversifcation and amount of rainfall and aims to identify cropping systems that may provide 
  both short-term gains and longer-term sustainability.
  "
  
  uri <- "hdl:11529/10548832"
  group <- "conservation_agriculture"
  ff <- carobiner::get_data(uri, path, group)
 
  dset <- data.frame(
  	carobiner::read_metadata(uri, path, group, major=1, minor=1),
    project=NA,
    publication= NA,
    data_institute = "CIMMYT",
    data_type="experiment",
    carob_contributor="Fredy Chimire", 
    carob_date="2023-08-21"
  )
  
  
  
  
  f <- ff[basename(ff) == "DATA SA 2005 to 2019.xls"]
  r <- carobiner::read.excel(f, sheet = "Raw Data")
  
  ## process file(s)
  
  # selecting columns of interest which match the carob standard format
  # d <- d[,c(	"Location","Season","System","Rep","Clay","Sand","OrgC","Biomass","Grain" )]
  
  # efyrouwa: you can use carobiner::change_names, 
  # look at the documentation on the function above to understand better.
  d <- carobiner::change_names(r,c("Location","Season","Rep","Clay","Sand","OrgC","Biomass","Grain","System","Nitrogen","Phosphorus","Potassium"),c("location","planting_date","rep","soil_clay","soil_sand","soil_SOC","dmy_total","yield","treatment", "soil_N", "soil_P_total","soil_K"))
  
  d$on_farm <- FALSE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  d$yield_part <- "grain"
  # efyrouwa : could the units be in tons/ha?, if so then convert 
  d$yield <- d$yield * 1000 
  d$dmy_total <- d$dmy_total * 1000 
  
  # Assign country names based on location on which experiment was made
  
  # country_mapping <- c("CRS" = "Malawi", "DTC" = "Zimbabwe", "HRS" = "Zimbabwe",
                       # "MFTC" = "Zambia","MRS" = "Zambia","SRS" = "Mozambique")
  # d$country <- country_mapping[d$Location
  
  d$country <- ifelse(d$location == "CRS", "Malawi",
                      ifelse(d$location == "DTC", "Zimbabwe",
                             ifelse(d$location == "HRS", "Zimbabwe",
                                    ifelse(d$location == "MFTC","Zambia",
                                           ifelse(d$location == "MRS","Zambia","Mozambique")))))

  # "Sources for gps coordinates"
   
   #https://malawi.worldplaces.me/places-in-chitedze/56585145-chitedze-research-station.html
   #https://glten.org/experiments/300)
   #https://vymaps.com/ZW/Henderson-Research-Station-394000277403139/#google_vignette
   #https://glten.org/experiments/14
   #https://glten.org/experiments/311
   #http://wheatatlas.org/station/MOZ/10706\
  
  d$latitude <- ifelse(d$location == "CRS", -13.97738,
                       ifelse(d$location == "DTC", -17.6091,
                              ifelse(d$location == "HRS", -17.5585947,
                                     ifelse(d$location == "MFTC",-16.2402,
                                            ifelse(d$location == "MRS",-13.645,-19.4112)))))
  
    
  d$longitude <- ifelse(d$location == "CRS", 33.64887,
                       ifelse(d$location == "DTC",31.1377,
                              ifelse(d$location == "HRS", 30.9814704,
                                     ifelse(d$location == "MFTC",27.44145,
                                            ifelse(d$location == "MRS",32.5585,33.2947)))))
  
 
  # # Get gps coordinates
  #  lattitude_mapping <- list("CRS"=c(-13.97738,33.64887) ,"DTC"=c(-17.6091, 31.1377),
  #                            "HRS"=c(-17.5585947, 30.9814704),"MFTC"=c(-16.2402, 27.44145),
  #                            "MRS"=c(-13.645, 32.5585),"SRS"=c(-19.4112,33.2947))
  #  
   
   
   # # Map geo coordinates to dataframe
   # d$latitude <- unlist(lapply(d$Location, function(loc) lattitude_mapping[[loc]][1]))
   # d$longitude <- unlist(lapply(d$Location, function(loc) lattitude_mapping[[loc]][2]))
   # 
  
   # We need to replace treatment codes with actual names
   # treat_names <- c("BA"="basins","CP"= "conventional ploughing","DiS"="dibble stick","DiS-MC"="dibble stick and maize-cowpea rotation",
                   # "DiS-M+C"="dibble stick and maize-cowpea intercrop", "DiS-M+Mp"="dibble stick and maize-velvet bean intercrop","DiS-M+Pp",
                   # "DS"= "dibble stick and maize-pigeonpea intercrop","DS-MG"="direct seeding and maize-groundnut rotation","DS-MSf"="direct seeding and maize-sunflower-cotton rotation", 
                   # "DS-M+C"="direct seeding and maize-cowpea intercrop","DS-MBio"="direct seeding and maize and biochar","RI"="ripping","RI-M+C"="ripping and maize-cowpea intercrop",
                   # "DS-MCt"= "direct seeding and maize-cotton rotation", "DS-MCtS"="direct seeding and maize-cotton-sunhemp rotation" ,"CP-MCt"="conventional ploughing and maize-cotton rotation",
                   # "DS-MC"="direct seeding and maize-cowpea rotation","DS-MSy"="direct seeding and maize-soyean rotation","DS-MSfC"= "direct seeding and maize-sunflower-cotton rotation",
                   # "DS-M+Pp"="irect seeding and maize-pigeonpea intercrop",  "JP"="jab planter","CP2"="CP2")
                   # 
   # efyrouwa: use gsub (),it's easier to keep track also avoid spaces and hyphens by placing underscores
   # first run unique(d$treatment)
   g <- d$treatment
   g <- gsub("-","",g)
   g <- gsub("\\+", "2",g)# remove the plus 
   g <- gsub("BA" ,"basins",g)
   g <- gsub("CP" ,"conventional_ploughing",g)
   g <- gsub("DiS" ,"dibble_stick",g)
   g <- gsub("DiSMC" ,"dibble_stick_maize_cowpea_rotation",g) 
   g <- gsub("DiSM2C" , "dibble_stick_maize_cowpea_intercrop",g)
   g <- gsub("DiSM2Mp", "dibble_stick_maize_velve_bean_intercrop",g)
   g <- gsub("DiSM2Pp", "direct_seeding_maize_pigeonpea_intercrop",g)
   g <- gsub("DS", "direct_seeding_sole_maize",g)
   g <- gsub("DSMG", "direct_seeding_maize_groundnut_rotation",g)
   g <- gsub("DSMSf","direct_seeding_maize_sunflower_rotation",g)
   g <- gsub("DSM2C", "direct_seeding_maize_cowpea_intercrop",g)
   g <- gsub("DSMBio","direct_seeding_maize_biochar",g)
   g <- gsub("RI", "ripping", g)
   g <- gsub("RIM2C", "ripping_maize_cowpea_intercrop",g)
   g <- gsub("DSMCt", "direct_seeding_maize_cotton_rotation",g)
   g <- gsub("DSMCtS", "direct_seeding_maize_cotton_sunhemp_rotation",g)
   g <- gsub("CPMCt" , "conventional_ploughing_maize_cotton_rotation",g)
   g <- gsub("DSMC","direct_seeding_maize_cowpea_rotation",g)
   g <- gsub("DSMSy" ,"direct_seeding_maize_soyean_rotation",g)
   g <- gsub("DSMSfC", "direct_seeding_maize_sunflower_cotton_rotation",g)
   g <- gsub("DSM2Pp", "direct_seeding_maize_pigeonpea_intercrop",g)
   g <- gsub("JP", "jab_planter",g)
   # CP2 not defined
   d$treatment <- g
   d$trial_id <- paste(1:nrow(d),d$treatment, sep = "_") #efyrouwa: trial_id not there therefore create,for uniqueness add numbers
   d$crop <- "maize"
   d$rep <- as.integer(d$rep)
   d$planting_date <- as.character(d$planting_date)
   d <- d[,c(-5,-6)] # remove columns 5 and 6
  
  carobiner::write_files(dset, d, path=path)
}



