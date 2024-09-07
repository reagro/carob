# R script for "carob"

carob_script <- function(path) {
  
  "Description:

    Group B1, cycle B1C5 of Population B (fifth cycle of recombination of the pure native Andigena group B1), is the result of a new population improvement strategy in the absence of R genes started at CIP in 1990. The group B1 derives from the primitive cultivars of Solanum tuberosum ssp. andigena, known to be free of R-genes. \r\n\r\nThese clones were planted in a randomized complete block design (RCBD) with 2-4  replicates at Oxapampa,  located at 1810 masl in Pasco-Peru in the Eastern mountain ranges facing the Amazon. The trials were established at Oxapampa due to the high disease pressure of late blight in these areas from 2005 to 2006."
  
  # Set doi, dataset id and group
    
  uri <- "doi:10.21223/P3/VYCLVX"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "varieties"
  
  
  ## Download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  metadata <- carobiner::read_metadata(uri, path, group, major=1, minor=3)
  
  ## Fill missing metadata
  
  metadata_requirements <- carobiner::accepted_variables(type = 'metadata')
  metadata_requirements <- metadata_requirements[which(metadata_requirements$required=="yes"),]
  
  ### Helper code to see missing metadata
  
  missing_metadata_idx <- which(!metadata_requirements$name %in% colnames(metadata))
  
  missing_metadata_vars <- metadata_requirements[missing_metadata_idx,]$name
  
  # print(missing_metadata_vars)
  
  ### Fill missing metadata
  
  fill_metadata <- c(
      "Henry Juarez",
      "experiment",
      "treatment",
      "AUDPC, rAUDPC, yield, yield_marketable",
      NA,
      "CIP",
      NA,
      as.character(Sys.Date())
  )
  
  data_vector <- setNames(fill_metadata, missing_metadata_vars)
  
  metadata <- cbind(metadata, t(as.data.frame(data_vector)))
  
  ## process file(s)
  
  ### Retrieve files with records based on a pattern
  
  files <- list.files(file.path(paste0(path,"/data/raw/varieties/",dataset_id,"/")), pattern = "OXAPMP", full.names = T)
  # cat("These are the available files for the doi link:\n\n")
  # print(files)
  
  ## Helper functions to process the data of multiple sheets of excel files
  
  filter_dfs <- function(file, variables=c("REP","INSTN","AUDPC","rAUDPC","TTYNA","MTYNA")){
      
      df <- carobiner::read.excel(f = file, sheet="Fieldbook")
      
      available_vars = which(colnames(df) %in% variables)
      
      missing_vars = which(!variables %in% colnames(df))
      
      
      df <- df[,available_vars]
      
      for(missing_var in variables[missing_vars]){
          
          df[[missing_var]] = NA
          
      }
      
      df = carobiner::change_names(df, from=c("REP","INSTN","TTYNA","MTYNA"), to = c("rep","variety","yield","yield_marketable") )
      
      
      tryCatch({
          df$rep <- as.integer(df$rep)
          df$yield <- df$yield * 1000
          df$yield_marketable <- df$yield_marketable * 1000
          df$AUDPC <- df$AUDPC / 100
      },error = function(e){
          print(paste0("Error: ", e$message))
          Sys.sleep(1.5)
      })
      
      
      return(df)
      
  }
  
  extract_metadata <- function(file, data){
      
      metadata <- carobiner::read.excel(f = file, sheet="Minimal")
      
      data$country <- metadata[which(metadata$Factor=="Country"),]$Value
      data$adm1 <- metadata[which(metadata$Factor=="Admin1"),]$Value
      data$adm2 <- metadata[which(metadata$Factor=="Admin2"),]$Value
      data$adm3 <- metadata[which(metadata$Factor=="Admin3"),]$Value
      data$longitude <- as.numeric(metadata[which(metadata$Factor=="Longitude"),]$Value)
      data$latitude <- as.numeric(metadata[which(metadata$Factor=="Latitude"),]$Value)
      data$elevation <- as.numeric(metadata[which(metadata$Factor=="Elevation"),]$Value)
      data$planting_date <- metadata[which(metadata$Factor=="Begin date"),]$Value
      data$harvest_date <- metadata[which(metadata$Factor=="End date"),]$Value
      
      return(data)
      
  }
  
  ## Fill missing records with a for loop
  
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
      d$trial_id = as.character(trial_id_counter) 
      
      trial_id_counter = trial_id_counter + 1
      
      
      df_list[[df_name]] <- d
      
  }
  
  ## Join records
  
  d <- do.call(rbind, df_list)
  
  d$record_id <- as.integer(1:nrow(d))
  
  
  ## Helper code to check the required variables for records

  crop_requirements <- carobiner::accepted_variables(type = 'records', group = 'varieties')
  crop_requirements <- crop_requirements[which(crop_requirements$required=="yes"),]

  missing_data_idx <-  which(!crop_requirements$name %in% colnames(d))
  missing_data_vars <- crop_requirements[missing_data_idx,]$name
  
  # print(missing_data_vars)
  
  
  ## Fill missing records 
  
  d$geo_from_source = TRUE
  d$N_fertilizer = NA
  d$P_fertilizer = NA
  d$K_fertilizer = NA
  
  
  carobiner::write_files(path = path,
                         metadata = metadata,
                         records = d)
  
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)
