# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
    "In 2019-2020 season, three clones with high levels of resistance to late blight were evaluated in adaptation and efficiency trials for tuber yield, dry matter content, frying and baking quality throughout Peru in six locations, compared to two varieties planted by farmers. and very well accepted by final consumers, Canchan and Unica, these are currently also used for frying in sticks, but without stability in all crops due to the genotype x environment interaction. The randomized complete block design was used with three replications of 150 plants, the fertilization dose was 200-220-180 Kg of NPK, using potassium sulfate as a source of potassium to improve frying quality. At harvest, samples were taken to determine dry matter, reducing sugar content, traditional and blanched frying color, and baking quality. The clone was equal to or superior to the controls for the yield of tubers, it presented good quality of frying color in all localities compared to the control varieties that did not present good quality of frying color in all localities, It is expected to complete all the documents requested by the Peruvian Seed Authority (SENASA) to be registered as a new potato variety with resistance to late blight and quality for frying and / or baking."
  
  uri <- "doi:10.21223/KNC22E"
  group <- "varieties"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- data.frame(
      carobiner::read_metadata(uri, path, group, major=1, minor=3),
      data_institute = "CIP",
      publication = NA,
      project = NA,
      data_type = "experiment",
      treatment_vars = "variety",
      response_vars = "yield;yield_marketable", 
      carob_contributor = "Henry Juarez",
      carob_date = "2024-09-13",
      notes = NA
  )
  
  process <- function(filename){
      
      r <- carobiner::read.excel(filename, sheet="Fieldbook")               
      minimal <- carobiner::read.excel(filename, sheet="Minimal")
      installation <- carobiner::read.excel(filename, sheet="Installation")
      
      m <- as.list(minimal$Value)
      names(m) <- minimal$Factor
      
      n <- as.list(installation$Value)
      names(n) <- installation$Factor
      
      if((!"yield"%in%colnames(r)) && ("TTWP" %in% colnames(r))){
          
          TTYNA = (as.numeric(r$TTWP) / as.numeric(n$`Plot_size_(m2)`)) * 10
          
          r$yield = TTYNA * 1000
          
      } 
      
      if((!"yield_marketable"%in%colnames(r)) && ("MTWP" %in% colnames(r))){
          
          MTYNA = (as.numeric(r$MTWP) / as.numeric(n$`Plot_size_(m2)`)) * 10
          r$yield_marketable = MTYNA * 1000
          
      }
      
      df <- data.frame(
          rep = as.integer(r$REP),
          variety = r$INSTN,
          yield = if('yield' %in% colnames(r)) {
              r$yield
          } else {
              as.numeric(NA)
          },
          yield_marketable = if('yield_marketable' %in% colnames(r)) {
              r$yield_marketable
          } else {
              as.numeric(NA)
          },
          AUDPC = if('AUDPC' %in% colnames(r)) {
              as.numeric(r$AUDPC) / 100
          } else {
              as.numeric(NA)
          },
          rAUDPC = if('rAUDPC' %in% colnames(r)) {
              as.numeric(r$rAUDPC)
          } else {
              as.numeric(NA)
          },
          country = m$Country,
          adm1 = m$Admin1,
          adm2 = m$Admin2,
          adm3 = m$Admin3,
          location = m$Locality,
          longitude = as.numeric(m$Longitude),
          latitude = as.numeric(m$Latitude),
          elevation = as.numeric(m$Elevation),
          planting_date = m$Begin_date |> 
              sub(".*-(.*)", "\\1", x=_) |> 
              as.Date(format = "%d/%m/%Y") |> 
              as.character(),
          harvest_date = m$End_date |> 
              sub(".*-(.*)", "\\1", x=_) |> 
              as.Date(format = "%d/%m/%Y") |> 
              as.character(),
          trial_id = gsub(".xls", "", basename(filename))
      )
  }
  
  f <- ff[grep("PTYield1", basename(ff))]
  d <- lapply(f, process)
  d <- do.call(rbind, d)
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  d$crop <- "potato"
  d$pathogen <- "Phytophthora infestans"
  d$yield_part <- "tubers"
  d$geo_from_source <- TRUE
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
  
  carobiner::write_files(path = path,
                         metadata = meta,
                         records = d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)
