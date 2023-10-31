



carob_script <- function(path) {
  
  "
  Rice crop cuts conducted in Odisha (Mayurbhanj, Balasore, Keonjhar, Bhadrak, Khorda, Puri districts)

"
  
  uri <- "hdl:11529/11111"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "crop_cuts"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    data_citation="Wasim Iftikar; Nabakishore Parida; Vivek Kumar; Narayan Chandra Banik; Amit Mishra, 2017, Odisha Rice Crop Cut Data 2013 - 2016, https://hdl.handle.net/11529/11111, CIMMYT Research Data & Software Repository Network, V2",
    publication= NA,
    data_institutions = "CIMMYT",
    data_type="experiment", # or, e.g. "on-farm experiment", "survey", "compilation"
    carob_contributor="Effie Ochieng'",
    carob_date="2023-09-25"
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=0)
  dset$license <- "CIMMYT license"
  
  
  f <- ff[basename(ff) == "CSISA_OD_RiceCropCut_AllYearRawDataFinal.csv"]
  d <- read.csv(f)
  
  d$trial_id <- paste(1:nrow(d),d$FID, sep = "_")
  d <- carobiner::change_names(d,c("Year","Season","District_Name","CEM","VAR","DOS","DOT","DOH","Latitude","Longitude"),c("year","season","adm2","treatment","variety","planting_date","transplanting_date","harvest_date","latitude","longitude"))
  d$country <- "India"
  d$adm1 <- "Odisha"
  d$crop <- "rice"
  d$yield_part <- "grain"
  
  t <- d[is.na(d$longitude),] #extract coordinates for those that do not have
  
  # g <- carobiner::geocode(country = t$country, adm1 = t$adm1, location = t$adm2, service = "nominatim") #to extract lat and lon
  # dput(g) to see the extracted data frame
  
  x <- data.frame(country = "India",  adm1 = "Odisha", 
		adm2 = c("Bhadrak", "Bhadrak", "Bhadrak", "Bhadrak", 
               "Bhadrak", "Bhadrak", "Bhadrak", "Bhadrak", "Bhadrak", "Bhadrak", 
               "Bhadrak", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Puri", "Puri", "Puri", "Puri", 
               "Puri", "Puri", "Puri", "Puri", "Puri", "Puri", "Puri", "Puri", 
               "Puri", "Puri", "Puri", "Puri", "Puri", "Puri", "Puri", "Puri", 
               "Puri", "Puri", "Puri", "Puri", "Puri", "Puri", "Puri", "Puri", 
               "Puri", "Puri", "Puri", "Puri", "Puri", "Puri", "Puri", "Puri", 
               "Puri", "Puri", "Puri", "Puri", "Puri", "Puri", "Puri", "Puri", 
               "Puri", "Puri", "Puri", "Puri", "Puri", "Puri", "Puri", "Puri", 
               "Puri", "Puri", "Puri", "Puri", "Puri", "Puri", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Bhadrak", 
               "Bhadrak", "Bhadrak", "Bhadrak", "Bhadrak", "Bhadrak", "Bhadrak", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", "Mayurbhanj", 
               "Mayurbhanj", "Mayurbhanj", "Puri"),
  longitude = c(86.498, 86.498, 86.498, 86.498, 86.498, 86.498, 86.498, 86.498, 86.498, 86.498, 
          86.498, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 
          86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 
          86.3957, 86.3957, 86.3957, 85.8391, 85.8391, 85.8391, 85.8391, 
          85.8391, 85.8391, 85.8391, 85.8391, NA, 85.8391, 85.8391, 
          85.8391, 85.8391, NA, 85.8391, 85.8391, 85.8391, 85.8391, 
          85.8391, 85.8391, 85.8391, 85.8391, 85.8391, 85.8391, 85.8391, 
          85.8391, 85.8391, 85.8391, 85.8391, 85.8391, 85.8391, 85.8391, 
          85.8391, 85.8391, 85.8391, 85.8391, 85.8391, NA, 85.8391, 
          85.8391, 85.8391, 85.8391, 85.8391, 85.8391, 85.8391, 85.8391, 
          85.8391, 85.8391, 85.8391, NA, 85.8391, 85.8391, 85.8391, 
          85.8391, 85.8391, 85.8391, 85.8391, NA, 86.3957, NA, 86.3957, 
          86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 
          86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 
          86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 
          86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 
          86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, NA, 
          86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 
          NA, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 
          86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 
          86.3957, 86.498, 86.498, 86.498, 86.498, 86.498, 86.498, 
          86.498, 86.3957, 86.3957, NA, 86.3957, 86.3957, 86.3957, 
          86.3957, NA, 86.3957, 86.3957, 86.3957, 86.3957, NA, 86.3957, 
          86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 
          86.3957, NA, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 
          86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 
          86.3957, 86.3957, 86.3957, NA, 86.3957, 86.3957, 86.3957, 
          86.3957, 86.3957, 86.3957, 86.3957, 86.3957, 85.8391), 
  latitude = c(21.0666,21.0666, 21.0666, 21.0666, 21.0666, 21.0666, 21.0666, 21.0666, 
           21.0666, 21.0666, 21.0666, 21.9156, 21.9156, 21.9156, 21.9156, 
           21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 
           21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 19.8068, 19.8068, 
           19.8068, 19.8068, 19.8068, 19.8068, 19.8068, 19.8068, NA, 
           19.8068, 19.8068, 19.8068, 19.8068, NA, 19.8068, 19.8068, 
           19.8068, 19.8068, 19.8068, 19.8068, 19.8068, 19.8068, 19.8068, 
           19.8068, 19.8068, 19.8068, 19.8068, 19.8068, 19.8068, 19.8068, 
           19.8068, 19.8068, 19.8068, 19.8068, 19.8068, 19.8068, 19.8068, 
           NA, 19.8068, 19.8068, 19.8068, 19.8068, 19.8068, 19.8068, 
           19.8068, 19.8068, 19.8068, 19.8068, 19.8068, NA, 19.8068, 
           19.8068, 19.8068, 19.8068, 19.8068, 19.8068, 19.8068, NA, 
           21.9156, NA, 21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 
           21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 
           21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 
           21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 
           21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 
           21.9156, 21.9156, NA, 21.9156, 21.9156, 21.9156, 21.9156, 
           21.9156, 21.9156, 21.9156, NA, 21.9156, 21.9156, 21.9156, 
           21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 
           21.9156, 21.9156, 21.9156, 21.9156, 21.0666, 21.0666, 21.0666, 
           21.0666, 21.0666, 21.0666, 21.0666, 21.9156, 21.9156, NA, 
           21.9156, 21.9156, 21.9156, 21.9156, NA, 21.9156, 21.9156, 
           21.9156, 21.9156, NA, 21.9156, 21.9156, 21.9156, 21.9156, 
           21.9156, 21.9156, 21.9156, 21.9156, 21.9156, NA, 21.9156, 
           21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 
           21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 
           NA, 21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 21.9156, 
           21.9156, 21.9156, 19.8068))   
  
  d$longitude <- ifelse(is.na(d$longitude), x$longitude[match(d$adm2, x$adm2)], d$longitude)
  d$latitude <- as.numeric(ifelse(is.na(d$latitude) | d$latitude == "-"  | d$latitude == "" | d$latitude == 19.58| d$latitude== 86.34, 
  x$latitude[match(d$adm2, x$adm2)], d$latitude)) 
  
  d$dataset_id <- dataset_id
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$planting_date <- as.character(format(as.Date(d$planting_date, format = "%d-%b-%y"), format = "%Y-%m-%d")) 
  d$harvest_date <- as.character(format(as.Date(d$harvest_date, format = "%d-%b-%y"), format = "%Y-%m-%d")) 
  d$transplanting_date <- as.character(format(as.Date(d$transplanting_date, format = "%d-%b-%y"), format = "%Y-%m-%d")) 
  d$fertilizer_type <- "none"
  d$fertilizer_type[!is.na(d$DAP_Total) & d$DAP_Total > 0] <- "DAP"
  d$fertilizer_type[!is.na(d$MoP_Total) & d$MoP_Total > 0] <- "KCl"
  d$fertilizer_type[!is.na(d$Urea_Total) & d$Urea_Total > 0] <- "urea"
  d$P_fertilizer <- ifelse(!is.na(d$DAP_Total), d$DAP_Total * 2.4711 * .201, 0) # to convert to kg/ha then work out the P using percentages in terms fertilizer.csv 
  d$K_fertilizer <- ifelse(!is.na(d$MoP_Total), d$MoP_Total * 2.4711 * .498, 0)
  d$N_fertilizer <- ifelse(!is.na(d$DAP_Total), d$DAP_Total * 2.4711 * 0.180, 
                           ifelse(!is.na(d$Urea_Total), d$Urea_Total * 2.4711 * 0.460, 0))
  
  d <- reshape(
    data = d,
    direction = "long",
    varying = list(
      c("TAGB_A", "TAGB_B", "TAGB_C"),
      c("Yield_A", "Yield_B", "Yield_C")
    ),
    v.names = c("biomass_total", "yield"),
    idvar = "trial_id",
    timevar = "spot"
  )
  d$biomass_total[d$biomass_total== "-"] <- 0
  d$yield[d$yield=="-"] <- 0
  
  # efyrouwa : from dataset description
  #Grain weight Year 2013 - 5X5 sq meter in Kg 
  #Year 2014 - 2X2.5 sq meter in Kg 
  #Year 2015 - 1X1 sq meter in gm 
  #Year 2016 - 2X2 sq meter in gm
  d$yield <- ifelse(d$year == 2013, as.numeric(d$yield) * 400,
             ifelse(d$year == 2014, as.numeric(d$yield) * 2000,
             ifelse(d$year == 2015, as.numeric(d$yield) * 10, as.numeric(d$yield) * 2.5))) # to calculate in kg/ha
  
  
  #Total above ground weight Year 2013 - 5X5 sq meter in Kg Year 
  #2014 - 2X2.5 sq meter in Kg 
  #Year 2015 - 1X1 sq meter in gm 
  #Year 2016 - 2X2 sq meter in Kg
  d$biomass_total <- ifelse(d$year == 2013, as.numeric(d$biomass_total) * 400,
                     ifelse(d$year == 2014, as.numeric(d$biomass_total) * 2000,
                     ifelse(d$year == 2015, as.numeric(d$biomass_total) * 10, as.numeric(d$biomass_total) * 2500)))# to calculate in kg/ha
  
  
  
  d <- d[, c("trial_id","dataset_id","country","adm1","adm2","latitude","longitude","crop","variety","planting_date","transplanting_date","harvest_date","treatment","yield_part","on_farm","is_survey","fertilizer_type", "N_fertilizer","P_fertilizer","K_fertilizer","biomass_total","yield")]
    # all scripts must end like this
    carobiner::write_files(dset, d, path=path)
}


