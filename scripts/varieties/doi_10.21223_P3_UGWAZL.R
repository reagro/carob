# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
    "In anticipation of the effects of global warming on potato cultivation in both tropical and subtropical environments. Since 2004, efforts have turned to the development of a new group in Population B with improved adaptation to warm environments, resistance to late blight and virus, mid-season maturity (90 day growing period under short-day length conditions), adaptation to  mid elevations, low glycoalkaloids content, along with economically important traits such as high tuber yield, quality for table and industry. And so group LBHT of population B was developed, denominated LBHT because of its late blight and heat tolerance. \r\nAll trials were conducted in  randomized complete block design (RCBD) with 2-4  replicates or in simple lattice design at Oxapampa, located at 1810 masl in Pasco in the Eastern mountain ranges facing the Amazon. The trials were established at Oxapampa due to high disease pressure of late blight, disease endemic, highly favorable for disease development and severity during the rainy periods  in these areas from 2005 to 2008."
  
  uri <- "doi:10.21223/P3/UGWAZL"
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
      
      
  }
  
  f <- ff[grep("_OXAPMP", basename(ff))]
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
  
  
  
  files <- list.files(file.path(paste0(path,"/data/raw/varieties/",dataset_id,"/")), pattern = "_OXAPMP", full.names = T)
  cat("These are the available files for the doi link:\n\n")
  print(files)
  
  
  filter_dfs <- function(file, variables=c("REP","INSTN","AUDPC","rAUDPC","TTYNA","MTYNA")){
      
      df <- carobiner::read.excel(f = file, sheet="Fieldbook")
      
      available_vars = which(colnames(df) %in% variables)
      
      missing_vars = which(!variables %in% colnames(df))
      
      
      df <- df[,available_vars]
      
      for(missing_var in variables[missing_vars]){
          
          df[[missing_var]] = NA
          
      }
      
      
      idx <- which(colnames(df) %in% c("REP","INSTN","TTYNA","MTYNA"))
      
      colnames(df)[idx] <- c("rep","variety","yield","yield_marketable") 
      
      
      tryCatch({
          df$rep <- df$rep |> as.integer()
          df$yield <- df$yield * 1000
          df$yield_marketable <- df$yield_marketable * 1000
          df$AUDPC <- df$AUDPC / 100
      },error = function(e){
          print(paste0("Error: ", e$message))
          Sys.sleep(1.5)
      })
      
      
      return(df)
      
  }
  
  ###
  
  extract_metadata <- function(file, data){
      
      metadata <- carobiner::read.excel(f = file, sheet="Minimal")
      
      data$country <- metadata[which(metadata$Factor=="Country"),]$Value
      data$adm1 <- metadata[which(metadata$Factor=="Admin1"),]$Value
      data$adm2 <- metadata[which(metadata$Factor=="Admin2"),]$Value
      data$adm3 <- metadata[which(metadata$Factor=="Admin3"),]$Value
      data$longitude <- metadata[which(metadata$Factor=="Longitude"),]$Value |> 
          as.numeric()
      data$latitude <- metadata[which(metadata$Factor=="Latitude"),]$Value |> 
          as.numeric()
      data$elevation <- metadata[which(metadata$Factor=="Elevation"),]$Value |> 
          as.numeric()
      data$planting_date <- metadata[which(metadata$Factor=="Begin date"),]$Value
      data$harvest_date <- metadata[which(metadata$Factor=="End date"),]$Value
      
      return(data)
      
  }
  
  ###
  
  df_list <- list()
  
  trial_id_counter = 1
  
  for(dataframe in files){
      
      d <- filter_dfs(dataframe)
      
      df_name <- sub(".*\\/(.*)\\.xls$", "\\1", dataframe)
      
      d <- extract_metadata(dataframe, d)
      
      d$on_farm <- TRUE
      d$is_survey <- FALSE
      d$irrigated <- FALSE
      d$treatment = "varieties"
      d$crop <- "potato"
      d$pathogen <- "Phytophthora infestans"
      d$yield_part <- "tubers"
      # d$is_experiment <- 'yes'
      # d$sciname <- 'Solanum tuberosum'
      d$trial_id = (trial_id_counter) |> 
          as.character()
      
      trial_id_counter = trial_id_counter + 1
      
      
      df_list[[df_name]] <- d
      
  }
  
  d <- do.call(rbind, df_list)
  
  d$record_id <- as.integer(1:nrow(d))
  
  ## required variabeles for metadata
  
  metadata_requirements <- read.csv("https://raw.githubusercontent.com/reagro/terminag/main/variables/variables_metadata.csv", sep = ",")
  metadata_requirements <- metadata_requirements[which(metadata_requirements$required=="yes"),]
  
  
  ## required variables for records
  
  crop_requirements <- read.csv("https://raw.githubusercontent.com/reagro/terminag/main/variables/variables_crop.csv", sep = ",")
  crop_requirements <- crop_requirements[which(crop_requirements$required=="yes"),]
  
  ## filling metadata 
  
  missing_metadata_idx <- which(!metadata_requirements$name %in% colnames(js))
  
  missing_metadata_vars <- metadata_requirements[missing_metadata_idx,]$name
  
  print(missing_metadata_vars)
  
  fill_metadata <- c(
      "Henry Juarez",
      "experiment",
      "treatment",
      "AUDPC, rAUDPC, yield, yield_marketable",
      NA,
      "CIP",
      NA,
      Sys.Date() |> as.character()
  )
  
  data_vector <- setNames(fill_metadata, missing_metadata_vars)
  
  js <- cbind(js, data_vector |> 
                  as.data.frame() |> 
                  t())
  
  ###
  
  missing_data_idx <-  which(!crop_requirements$name %in% colnames(d))
  
  missing_data_vars <- crop_requirements[missing_data_idx,]$name
  
  print(missing_data_vars)
  
  
  # Data
  
  # d$trial_id = js$dataset_id
  d$geo_from_source = TRUE
  d$N_fertilizer = NA
  d$P_fertilizer = NA
  d$K_fertilizer = NA
  
  
  carobiner::write_files(path = path,
                         metadata = js,
                         records = d)
  
  
  
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)
