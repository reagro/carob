# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "Description:

    [copy the abstract from the repo]"
  
  uri <- "doi:10.21223/HMMWZH"
  group <- "pest_disease"
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(uri, path, group, major=2, minor=0)
  
  # f <- ff[basename(ff) == "16470_PTYield082019_Parental_Value_B3C3_CIPSRM19_02.xlsx"]
  
  # r <- read.csv(f, sep = ";")
  
  ## process file(s)
  
  files <- list.files(file.path(sprintf(
      "%s/data/raw/%s/%s/", path, group, dataset_id
  )),
  pattern = "0[0-9]_",
  full.names = T)
  
  cat("These are the available files for the doi link:\n\n")
  print(files)
  
  ####
  d_severity <- carobiner::read.excel(files[grep("01_",files)])
  idx <- grep("LB",colnames(d_severity))
  d_severity <- reshape(d_severity, idvar = "id", varying = list(colnames(d_severity)[idx]), direction = 'long', v.names = "disease_severity", timevar = "LBs")
  d_severity$LBs <- paste0("LB",d_severity$LBs)
  
  dates_severity <- carobiner::read.excel(files[grep("02_",files)])
  dates_severity <- dates_severity[-1,c(1,2)]
  
  d1 <- merge(d_severity, dates_severity, by.x="LBs", by.y = "Abbreviation", all.x = TRUE)
  
  idx <- which(names(d1) == "Evaluation_date")
  
  names(d1)[idx] <- "date"
  
  ####
  
  d_canopy <- carobiner::read.excel(files[grep("03_",files)])
  idx <- grep("CC_G",colnames(d_canopy))
  d_canopy <- reshape(d_canopy, idvar = "id", varying = list(colnames(d_canopy)[idx]), direction = "long", v.names = "LAI", timevar = "Canopy")
  d_canopy$CC_Gs <- paste0("CC_G_Et_%_",d_canopy$Canopy)
  
  
  dates_canopy <- carobiner::read.excel(files[grep("04_",files)])
  dates_canopy <- dates_canopy[,c(2,4)]
  
  d2 <- merge(d_canopy, dates_canopy, by.x = "CC_Gs", by.y = "Abbreviation", all.x = TRUE)
  
  idx <- which(names(d2) == "Evaluation_date_CCG")
  
  names(d2)[idx] <- "date"
  
  ####
  
  # d_fungicide = carobiner::read.excel(files[grep("ungicide",files)])
  # d_fungicide = d_fungicide[3:nrow(d_fungicide),8:9]
  # 
  # colnames(d_fungicide) = paste0("every_",d_fungicide[1,]," days")
  # 
  # d_fungicide = d_fungicide[4:nrow(d_fungicide),] 
  # 
  # d_fungicide =  d_fungicide |>
  #     lapply(\(x) as.Date(as.numeric(x), origin = "1899-12-30")) |>
  #     as.data.frame()
  # 
  # d_fungicide_long <- reshape(d_fungicide, 
  #                             varying = list(colnames(d_fungicide)), 
  #                             v.names = "Fungicide Date", 
  #                             timevar = "Number of days", 
  #                             times = colnames(d_fungicide), 
  #                             direction = "long")
  # 
  # d_fungicide_long <- d_fungicide_long[, c("Number of days", "Fungicide Date")]
  # 
  # 
  # d2$Fungicide_num <- as.numeric(gsub("F", "", d2$Fungicide_rate))
  # Days_num <- gsub("every_", "", d_fungicide_long$`Number of days`)
  # Days_num = gsub(".days", "", Days_num)
  # d_fungicide_long$Days_num <- as.numeric(Days_num)
  # 
  # final_df <- merge(d2, d_fungicide_long, by.x = "Fungicide_num", by.y = "Days_num", all.x = TRUE)
  # d2 <- final_df[, !colnames(final_df) %in% c("Fungicide_num", "Number of Days")]
  
 
  ####
  
  d_yield <- carobiner::read.excel(files[grep("05_",files)])
  idx <- grep("TBDMC", colnames(d_yield))
  d_yield <- reshape(d_yield, idvar = "id", varying = list(colnames(d_yield)[idx]), direction = "long", v.names = "dmy_total", timevar = "TBDMCs")
  d_yield$TBDMCs <- paste0("TBDMC_Cp_gplant_",d_yield$TBDMCs)
  
  
  dates_yield <- carobiner::read.excel(files[grep("06_",files)])
  dates_yield <- dates_yield[,c(2,3)]
  
  d3 <- merge(d_yield, dates_yield, by.x = "TBDMCs", by.y = "Abbreviation", all.x = TRUE)
  
  idx <- which(names(d3) == "Evaluation_date_TBDMC")
  
  names(d3)[idx] <- "date"
  
  ####
  
  d_int <- merge(d1,d2, by = c('id','Variety','Nitrogen_rate',
                               'Fungicide_rate','Treatment','Repetition','date'),all.x = T, all.y = T)
  
  
  d <- merge(d_int,d3, by = c('id','Variety','Nitrogen_rate',
                              'Fungicide_rate','Treatment','Repetition','date'), all.x = T, all.y = T)
  ## required variabeles for metadata
  
  d$fungicide_times = d$Fungicide_rate |> 
      gsub('F','',x=_) |> 
      as.integer()
  
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
  
  #####
  
  
  d$country <- "Peru"
  d$adm1 <- "Cerro de Pasco"
  d$adm2 <- "Oxapampa"
  d$adm3 <- "Miraflores"
  d$longitude = -75.383342
  d$latitude = -10.591977
  d$planting_date <- "1999-11-27" 
  d$harvest_date<- "2000-04-05"
  d$elevation <- 1813
  
  ####
  
  d$record_id <- as.integer(1:nrow(d))
  
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  ## the treatment code
  d$treatment <- "Variety x Nitrogen_rate x Fungicide_rate"
  d$trial_id <- "1"
  d$crop <- "potato"
  d$pathogen <- "Phytophthora infestans"
  d$yield_part <- "tubers"
  # d$is_experiment <- 'yes'
  # d$sciname <- 'Solanum tuberosum'
  d$geo_from_source = TRUE
  
  d = carobiner::change_names(x = d, 
                              from = c('Variety', 'Repetition'),
                              to = c('variety', 'rep')
                             )
  
  d$yield = NA
  d$N_fertilizer = as.numeric(gsub("N", "", d$Nitrogen_rate))
  d$P_fertilizer = NA
  d$K_fertilizer = NA
  
  d$rep = d$rep |> as.integer()
  
  remove_vars = c('id', 'Nitrogen_rate', 'Fungicide_rate', 'Treatment', 'LBs', 'CC_Gs', 'TBDMCs', 'Canopy')
  
  idx = which(!colnames(d) %in% remove_vars)
  
  d = d[,idx]
  
  d$date = as.character(d$date)
  d$yield = NULL
  
  
  
  # d = d[!is.na(d$date),]
  
  d$disease_severity = d$disease_severity |> as.character()
  
  weather = carobiner::read.excel(f = files[8])
  colnames(weather) = weather[2,]
  colnames(weather)[1] = 'date'
  weather = weather[3:nrow(weather),]
  weather$date = weather$date |> 
      as.character()
  
  weather = weather[,-c(2,8,9,10,11)]
  
  colnames(weather) = c('date','temp','tmax','tmin','rhum','prec')
  
  for(i in 2:ncol(weather)){
      
      weather[[i]] = as.numeric(weather[[i]]) 
      
  }
  
  
  carobiner::write_files(path = path, metadata = js,wide=d, wth = weather)
  
  
}

