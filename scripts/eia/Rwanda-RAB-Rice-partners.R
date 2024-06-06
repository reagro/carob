# R script for EiA version of"carob"

## ISSUES
# 1. DOI and much of the metadata is missing
# 2. Data is compiled from multiple sources (AfricaRice, IFDC, etc)
# 3. Some of the rates nutrient rates in "SAnDMan_Rice_fieldData.RDS" are not indicated, and only treatment is provided
# 4. License is missing (CC-BY)?
# 5. ...


carob_script <- function(path) {
   
   "
SOME DESCRIPTION GOES HERE...
"
   
   #### Identifiers
   uri <- "doi:Rwanda-RAB-Rice-partners"
   group <- "fertilizer"
   
   #### Download data 
   ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("/home/jovyan/carob-eia/data/raw/eia/Rwanda-RAB-Rice-partners/", full.names = T))
   # ff <- list.files(paste0(getwd(), '/data/raw/', group, '/', uri), full.names = TRUE)
   
   ##### dataset level metadata 
   dset <- data.frame(
      uri = carobiner::simple_uri(uri),
      dataset_id = uri,
      authors = 'Some names here...',
      title = 'Rwanda Rice Data',
      description = 'Some description here...',
      data_institutions = "CGIAR - CIP",
      group = group,
      license = 'Some license here...',
      carob_contributor = 'Eduardo Garcia Bendito',
      data_citation = '...',
      project = 'Excellence in Agronomy - Rwanda RAB',
      data_type = "compilation", # or, e.g. "on-farm experiment", "survey", "compilation"
      carob_date="2024-04-25"
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
   ### CN
   #the treatment used and fertilizer applied are a bit confusing 
   ## what is the unit of Nrate, Prate and Krate?
   d1 <- data.frame(trial_id = paste0("AfricaRice", "-", as.integer(factor(paste0(r1$Longitude, "-", r1$Latitude)))),
                    country = r1$Country,
                    location = r1$Village,
                    site = r1$SiteName,
                    longitude = as.numeric(r1$Longitude),
                    latitude = as.numeric(r1$Latitude),
                    treatment = r1$Treatment,
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
   
   ##CN
   ## Contain of "Diag" not specified
   r2$N <- gsub("Diag",NA,r2$N) ## change "Diag" to NA 
   r2$P <- gsub("Diag",NA,r2$P) ## change "Diag" to NA 
   r2$K <- gsub("Diag",NA,r2$K) ## change "Diag" to NA 
   
   d2 <- data.frame(country = "Rwanda",
                    trial_id = paste0("IFDC", "-", as.integer(factor(paste0(r2$lon, "-", r2$lat)))),
                    longitude = as.numeric(r2$lon),
                    latitude = as.numeric(r2$lat),
                    treatment = r2$Treat,
                    planting_date = as.character(substr(r2$season, start = 1, stop = 4)),
                    season = trimws(substr(r2$season, start = 5, stop = nchar(r2$season))),
                    crop = "rice",
                    N_fertilizer = as.numeric(r2$N),
                    P_fertilizer = as.numeric(r2$P),
                    K_fertilizer = as.numeric(r2$K),
                    yield = r2$TY * 1000)
   
   d3 <- data.frame(country = "Rwanda",
                    trial_id = paste0("EiARAB", "-", as.integer(factor(paste0(r3$lon, "-", r3$lat)))),
                    longitude = as.numeric(r3$lon),
                    latitude = as.numeric(r3$lat),
                    treatment = r3$Treatment,
                    planting_date = as.character(as.Date(r3$PlantingDate, "%d/%m/%Y")),
                    harvest_date = as.character(as.Date(r3$HarvestDate, "%d/%m/%Y")),
                    season = "B",
                    crop = "rice",
                    N_fertilizer = as.numeric(r3$N),
                    P_fertilizer = as.numeric(r3$P),
                    K_fertilizer = as.numeric(r3$K),
                    lime = as.numeric(r3$Lime),
                    yield = (r3$FreshWeightPrimary)/(r3$NetplotArea/10000), # Assuming that the measurement is in kg/m2
                    dmy_residue = (r3$dryWeightsecondaryProduct)/(r3$NetplotArea/10000))
   
   d4 <- data.frame(country = "Rwanda",
                    trial_id = paste0("ScalingAKILIMO", "-", as.integer(factor(paste0(r4$lon, "-", r4$lat)))),
                    longitude = as.numeric(r4$lon),
                    latitude = as.numeric(r4$lat),
                    treatment = r4$treat,
                    planting_date = as.character(as.Date(r4$plantingDate, "%Y-%m-%d")),
                    harvest_date = as.character(as.Date(r4$harvestDate, "%Y-%m-%d")),
                    season = "A",
                    crop = "rice",
                    plot_area=r4$plotSize,
                    plot_width= as.numeric(r4$plotWidthGrainYield),
                    plot_lenght= as.numeric(r4$plotLengthGrainYield),
                    
                    # SAnDMan treatment levels not specified
                    # N_fertilizer = r4$Nrate,
                    # P_fertilizer = r4$Prate,
                    # K_fertilizer = r4$Krate,
                    yield = r4$grainsFW/(r4$plotSize/10000) # Assuming that the measurement is in kg/m2
                    
   )
   
   d <- carobiner::bindr(d1,d2,d3,d4)
   ## Fix whitespace in treatment
   d$treatment <- gsub("", NA, d$treatment)
   
   d$yield_part <- "grain"
   d$latitude <- ifelse(d$latitude > 0, d$latitude*-1, d$latitude)
   d$dataset_id <- uri
   
   # all scripts must end like this
   carobiner::write_files(path, dset, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)
