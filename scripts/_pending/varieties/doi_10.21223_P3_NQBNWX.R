# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "Description:

    [copy the abstract from the repo]"
  
  uri <- "doi:10.21223/P3/NQBNWX"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "varieties"
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::read_metadata(uri, path, group, major=1, minor=3)
  # dset$license <- js$license
  # dset
  
  
  # f <- ff[basename(ff) == "16470_PTYield082019_Parental_Value_B3C3_CIPSRM19_02.xlsx"]
  
  # r <- read.csv(f, sep = ";")
  
  ## process file(s)
  
  files <- list.files(file.path(paste0(path,"/data/raw/varieties/",dataset_id,"/")), pattern = "VIENA", full.names = T)
  cat("These are the available files for the doi link:\n\n")
  print(files)
 
  
  ####
  
  filter_dfs <- function(file, variables=c("REP","INSTN","AUDPC","rAUDPC","TTYNA","MTYNA")){
    
    df <- carobiner::read.excel(f = file, sheet="Fieldbook")
    
    df <- df[,variables]
    
    idx <- which(colnames(df) %in% c("REP","INSTN","TTYNA","MTYNA"))
    
    colnames(df)[idx] <- c("rep","variety","yield","yield_marketable") 
    
    df$rep <- df$rep |> as.integer()
    
    df$yield <- df$yield * 1000
    df$yield_marketable <- df$yield_marketable * 1000
    df$AUDPC <- df$AUDPC / 100
    
    return(df)
    
  }
  
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
  
  
  df_list <- list()
  
  
  for(dataframe in files){
    
    d <- filter_dfs(dataframe)
    
    df_name <- sub(".*\\/(.*)\\.xls$", "\\1", dataframe)
    
    d <- extract_metadata(dataframe, d)
    
    d$on_farm <- TRUE
    d$is_survey <- FALSE
    d$irrigated <- FALSE
    d$treatment <- "Phytophthora infestans"
    d$crop <- "potato"
    d$pathogen <- "Phytophthora infestans"
    d$yield_part <- "tubers"
    # d$is_experiment <- 'yes'
    # d$sciname <- 'Solanum tuberosum'
    
    df_list[[df_name]] <- d
    
  }
  
  d <- do.call(rbind, df_list)
  
  d$record_id <- as.integer(1:nrow(d))
  
  ### filling metadata
  
  # Metadata
  
  js$carob_contributor = "Henry Juarez"
  js$data_type = "experiment"
  js$treatment_vars = "treatment"
  js$response_vars = "AUDPC,rAUDPC,yield,yield_marketable"
  js$publication = NA
  js$data_institute = "CIP"
  js$project = NA
  js$carob_date = Sys.Date() |> 
    as.character()
  
  # Data
  
  # d$trial_id = js$dataset_id
  d$geo_from_source = TRUE
  d$N_fertilizer = NA
  d$P_fertilizer = NA
  d$K_fertilizer = NA
  d$trial_id = js$dataset_id
  
  
  carobiner::write_files(path = path,
                         metadata = js,
                         records = d)
  
  
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)
