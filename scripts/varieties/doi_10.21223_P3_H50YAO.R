# R script for "carob"


carob_script <- function(path) {
  
"In anticipation of the effects of global warming on potato cultivation in both tropical and subtropical environments. Since 2004, efforts have turned to the development of a new group in Population B with improved adaptation to warm environments, resistance to late blight and virus, mid-season maturity (90 day growing period under short day length conditions), adaptation to mid elevations, low glycoalkaloids content, along with economically important traits such as high tuber yield, quality for table and industry. And so group LBHT of population B was developed, denominated LBHT because of its late blight and heat tolerance.  \r\nAll trials were conducted in randomized complete block design (RCBD) with 3-4 replicates or in simple lattice design, using Désirée as a heat tolerant control and Amarilis as a non-tolerant to heat control at La Molina-Peru, in spring-summer season, located at 300 masl in Lima, an arid environment in lowland tropics."
  
  uri <- "doi:10.21223/P3/H50YAO"
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
  
  process <- function(filename) {
      
      r <- carobiner::read.excel(filename, sheet="Fieldbook")               
      minimal <- carobiner::read.excel(filename, sheet="Minimal")
      installation <- carobiner::read.excel(filename, sheet="Installation")
      
      m <- as.list(minimal$Value)
      names(m) <- minimal$Factor
      
      n <- as.list(installation$Value)
      names(n) <- installation$Factor

	  plot_adj <- 10000 / as.numeric(n$`Plot size (m2)`)

      data.frame(
          rep = as.integer(r$REP),
          variety = r$INSTN,
          yield = as.numeric(r$TTWP) * plot_adj,
          yield_marketable = as.numeric(r$MTWP) * plot_adj,
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
  }
  
  f <- ff[grep("CIPHQ_LBHT", basename(ff))]
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
  
  
  carobiner::write_files(path = path, metadata = meta, records = d)
}

