# R script for "carob"

# ## ISSUES
"
Some sites have missing geo coordinates 
"

path <- getwd()
carob_script <- function(path) {
  
  "Description:
  The file contains on-farm and on-station agronomy data sets generated in Eastern and Southern Africa between 2010 and 2016 under the Sustainable Intensification of Maize-Legume systems in Eastern and Southern Africa (SIMLESA) project. The five countries involved were Ethiopia, Kenya, Tanzania, Malawi and Mozambique. The experimental trials tested the performance of various Conservation Agriculture based maize-legume intercrops or rotations relative to the commonly practiced conventional till farmer practice systems. In most of the tested cropping systems, the conventional farmer practice involved tillage using the moldboard plough, the manual hand hoe or in some cases the ridge and furrow system. Conservation Agriculture involved reduced soil disturbance, provision of soil cover and the use of leguminous crop rotations or intercrops. Crop establishment techniques involved hoe prepared planting basins or stations, ripping using animal traction, direct seeding using the dibble stick, jab planters, or animal traction direct seeding equipment. Crop management in terms of planting date, plant populations, fertilization, pest and weed control, were similar across all tested cropping systems. Experiments were mostly run in the same locations over the seven year period in order to also get an understanding of how the systems performance changed over time. On-station data has 43 variables and 1114 observations On-farm data has 43 variables and 5242 observations (2017-12-17)

  "
  
  uri <- "hdl:11529/2223085"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "maize_trials"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    data_citation="Nyagumbo, Isaiah (CIMMYT) - ORCID: 0000-0003-0180-234X
                    Rusinamhodzi, Leonard (CIMMYT) - ORCID: 0000-0002-5576-2040
                    Mupangwa, W (CIMMYT) - ORCID: 0000-0001-5672-1331
                    Njeru, John (CIMMYT)
                    Craufurd, Peter (CIMMYT) - ORCID: 0000-0001-8559-0174
                    Nhambeni, B (CIMMYT)
                    Dias, Domingos (IIAM, Mozambique) - ORCID: 0000-0002-0333-2241
                    Kamalongo, Donwell (DARS, Malawi) - ORCID: 0000-0002-8286-1036
                    Siyeni, Dyton (DARS, Malawi)
                    Ngwira, Amos (DARS, Malawi)
                    Sariah, John (DRD, Tanzania)
                    Ngatoluwa, Rama (DRD, Tanzania)
                    Makoko, B (DRD, Tanzania)
                    Ayaga, George (KALRO, Kenya)
                    Micheni, Alfred (KALRO, Kenya)
                    Nkonge, Charles (KALRO, Kenya)
                    Atomsa, TB (EIAR, Ethiopia) - ORCID: 0000-0001-5199-7195
                    Bedru, Beshir (EIAR, Ethiopia)
                    Kanampiu, Fred (IITA, Kenya)",
    publication= NA,
    data_institutions = "CIMMYT",
    data_type="experiment",
    carob_contributor="Fredy chimire" 
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=3, minor=2)
  dset$license <- carobiner::get_license(js)
  
  
  f <- ff[basename(ff) == "Agronomy full data set 13 dec 2019 with nonames.xlsx"]
  
  # start with reading On-farm data sheet
  d <- carobiner::read.excel(f, sheet = "onfarmwith nonames")
  
  # Reading On-station data sheet
  d1 <- carobiner::read.excel(f, sheet = "On-station",skip= 1) 
  
  # remove first row 
  d1 <- d1[-1,]
  
  # Drop the last 6 rows
  d1 <- d1[1:(nrow(d1) - 6), ] 
  

  
  # selecting columns of interest which match the carob standard format from onfarm sheet
  d <- d[,c("country","rainfall", "original_treatment","Clay 0_20_cm","sand_0_20_cm",
            "site","Latitude","Longitude","Basal.fertilizer.type","Maize harvesting dat","Maize Planting Date",
            "maize_grain")]
  
  # selecting columns of interest which match the carob standard format from onfarm sheet
  d1 <- d1[,c("Country name","SIMLESA Site name", "Total rainfall","Clay 0-20 cm","Sand 0-20 cm"
              ,"Silt 0-20 cm","Planting date...37","Date harvesting...39","Replicate",
            "Treatment name/code","Maize grain yield")]
  
  
  
  
  #  put dates in the correct format for the onfarm sheet
  
  
  d$`Maize Planting Date`<- as.Date("1899-12-30") + as.numeric(d$`Maize Planting Date`)
  
  d$`Maize harvesting dat`<- as.Date("1899-12-30") + as.numeric(d$`Maize harvesting dat`)
  
  # for onfarm data
  d$dataset_id <- dataset_id
  d$on_farm <- FALSE
  d$is_survey <- FALSE
  d$is_experiment <- TRUE
  d$irrigated <- FALSE
  
  # for onstation data
  
  d1$dataset_id <- dataset_id
  d1$on_farm <- FALSE
  d1$is_survey <- FALSE
  d1$is_experiment <- TRUE
  d1$irrigated <- FALSE
  
 # standardise columns for the sheet named onfarm
  d$crop <- "maize"
  d$treatment <- d$original_treatment
  d$harvest_date <- d$`Maize harvesting dat`
  d$planting_date <-  d$`Maize Planting Date`
  d$soil_clay <- d$`Clay 0_20_cm`
  d$soil_sand <- d$sand_0_20_cm
  d$yield_part <- "grain"
  d$yield <- d$maize_grain
  d$fertilizer_type <- d$Basal.fertilizer.type
  d$latitude <- d$Latitude
  d$longitude <- d$Longitude
  
  
  # standardise columns for the sheet named onstation
  d1$country <- d1$`Country name`
  d1$crop <- "maize"
  d1$treatment <- d1$`Treatment name/code`
  d1$harvest_date <- d1$`Date harvesting...39`
  d1$planting_date <-  d1$`Planting date...37`
  d1$rainfall <- d1$`Total rainfall`
  d1$rep <- d1$Replicate
  d1$soil_clay <- d1$`Clay 0-20 cm`
  d1$soil_sand <- d1$`Sand 0-20 cm`
  d1$soil_silt <- d1$`Silt 0-20 cm`
  
  d1$yield_part <- "grain"
  d1$yield <- d1$`Maize grain yield`
  d1$site <- d1$`SIMLESA Site name`
  
  # find geo coordinates for onstation
  locs <- unique(d1[,c("country","site")]) 
  locs <- na.omit(locs) # remove null values
  
  geocodes <- carobiner::geocode(country=locs$'country',location=locs$'site')
  geocodes1 <- geocodes$df
 
  install.packages("dplyr")
  library(dplyr)
  
  # renaming columns
  geocodes1 <- geocodes1 %>%
    rename(longitude=lon,latitude = lat,site=location)
  # Assign geo cordinates which were not found using carobiner function 
  lattitude_mapping <- list("ISPM Chimoio"=c(-19.08114,33.39414) ,"Melkassa"=c(8.4, 39.33333),
                            "ARI-Ilonga"=c(-9.06667,36.85),"SARI"=c(-6,35))
  
  # sources for gps coordinates 
  #https://mozambique.worldplaces.me/view-place/61801532-ispm-santo-antonio-chimoio.html
  #https://www.gps-coordinates.net/
  
  # Map geo coordinates to dataframe
  geocodes1$latitude[is.na(geocodes1$latitude)] <- unlist(lapply(geocodes1$site, function(loc) lattitude_mapping[[loc]][1]))
  geocodes1$longitude[is.na(geocodes1$longitude)] <- unlist(lapply(geocodes1$site, function(loc) lattitude_mapping[[loc]][2]))
  
  # merge dataset to get geocoordinates
  #d1 <- merge(d1,geocodes1,by=c("country","site"))
  d1<- left_join(d1,geocodes1,by=c("country","site"))
  
  
 
  
  
  # all scripts must end like this
  carobiner::write_files(dset, d, path=path)
  carobiner::write_files(dset, d1, path=path)
  #carob_script(path)
}


