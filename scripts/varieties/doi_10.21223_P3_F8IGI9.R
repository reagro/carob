# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
    "The Mother and Baby (M&B) trial methodology was adapted by CIP for Participatory Variety Selection (PVS) through decentralized evaluation networks and multi-year evaluations in potato growing areas in the Andean region. The M&B trial design encourages active participation of farmers through the application of treatments through systematic evaluations and selections of treatments in their own plots called \"Baby trials\" (i.e. farmer managed trials) and in fields with an experimental design called \"Mother trials\" (i.e. researcher managed trials).\r\nObjective: Analyze characteristics, attributes and preferences that men and women have when selecting a new potato variety at the phase of flowering and harvesting.\r\nA M&B trial was performed to evaluate 20 clones of the population B3C2 with late blight resistance at the locality of Chaquicocha, Pataz province in La Libertad department, Peru. The trial design was a Randomized Complete Block Design (RCBD) with 3 replicates during 2009-2010. In this experiment, characteristics of plant (size, type of foliage) and resistance to diseases during flowering and harvesting were evaluated."
  
  uri <- "doi:10.21223/P3/F8IGI9"
  group <- "varieties"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- data.frame(
      carobiner::read_metadata(uri, path, group, major=3, minor=0),
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
      
      r <- carobiner::read.excel(filename, sheet="F4_ Harvest_Mother")               
      minimal <- carobiner::read.excel(filename, sheet="Minimal")
      installation <- carobiner::read.excel(filename, sheet="Installation")
      
      m <- as.list(minimal$Value)
      names(m) <- minimal$Factor
      
      n <- as.list(installation$Mother)
      names(n) <- installation$Factor
      
      if((!"yield"%in%colnames(r)) && ("TTWP" %in% colnames(r))){
          
          TTYNA = (as.numeric(r$TTWP) / as.numeric(n$`Plot size (m2)`)) * 10
          
          r$yield = TTYNA * 1000
          
      } 
      
      if((!"yield_marketable"%in%colnames(r)) && ("MTWP" %in% colnames(r))){
          
          MTYNA = (as.numeric(r$MTWP) / as.numeric(n$`Plot size (m2)`)) * 10
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
          planting_date = m$`Begin date`,
          harvest_date = m$`End date`,
          trial_id = gsub(".xls", "", basename(filename))
      )
      
      return(df)
      
  }
  
  f <- ff[grep("PTPV", basename(ff))]
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
