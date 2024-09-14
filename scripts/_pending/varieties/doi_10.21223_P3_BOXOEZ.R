# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "Description:

    [copy the abstract from the repo]"
  
  uri <- "doi:10.21223/P3/BOXOEZ"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "varieties"
  
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::read_metadata(uri, path, group, major=1, minor=3)
  
  # f <- ff[basename(ff) == "16470_PTYield082019_Parental_Value_B3C3_CIPSRM19_02.xlsx"]
  
  # r <- read.csv(f, sep = ";")
  
  ## process file(s)
  
  files <- list.files(file.path(paste0(path,"/data/raw/varieties/",dataset_id,"/")), pattern = "OXAPMP", full.names = T)
  cat("These are the available files for the doi link:\n\n")
  print(files)
  
  
  filter_dfs <- function(file, variables=c("REP","INSTN","AUDPC","rAUDPC","TTYNA","MTYNA")){
      
      df <- carobiner::read.excel(f = file, sheet="Fieldbook")
      
      available_vars = which(colnames(df) %in% variables)
      
      missing_vars = which(!variables %in% colnames(df))
      
      
      df <- df[,available_vars]
      
      df_fill <- carobiner::read.excel(f = file, sheet="Fieldbook")
      df_installation <- carobiner::read.excel(f = file, sheet="Installation")
      
      for(missing_var in variables[missing_vars]){
          
          if(missing_var=="TTYNA" && ("TTWP" %in% colnames(df_fill))){
              
              idx_1 = which(df_installation$Factor == "Planting density (plants/Ha)")
              idx_2 = which(df_installation$Factor == "Number of plants planted per plot")
              
              TTYNA = df_fill$TTWP * ((df_installation[idx_1, ]$Value |> as.numeric()) /
                                          (df_installation[idx_2, ]$Value |> as.numeric()))
              
              df$TTYNA = TTYNA
              
          } else if(missing_var=="MTYNA" && ("MTWP" %in% colnames(df_fill))){
              
              idx_1 = which(df_installation$Factor == "Planting density (plants/Ha)")
              idx_2 = which(df_installation$Factor == "Number of plants planted per plot")
              
              MTYNA = df_fill$MTWP * ((df_installation[idx_1, ]$Value |> as.numeric()) /
                                          (df_installation[idx_2, ]$Value |> as.numeric()))
              
              df$MTYNA = MTYNA
              
          } else {
              
              df[[missing_var]] = NA
              
          }
          
          
      }
      
      
      # idx <- which(colnames(df) %in% c("REP","INSTN","TTYNA","MTYNA"))
      # 
      # colnames(df)[idx] <- c("rep","variety","yield","yield_marketable") 
      
      df = carobiner::change_names(df, from=c("REP","INSTN","TTYNA","MTYNA"), to = c("rep","variety","yield","yield_marketable") )
      
      
      tryCatch({
          df$rep <- df$rep |> as.integer()
          df$yield <- df$yield |> as.numeric() * 1000
          df$yield_marketable <- df$yield_marketable |> as.numeric() * 1000
          df$AUDPC <- df$AUDPC |> as.numeric() / 100
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
