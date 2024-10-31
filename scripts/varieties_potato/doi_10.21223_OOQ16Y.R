# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
    "During the period 2020-2021, experiments were planted to study the phenotypic stability of tuber yield in thirty advanced clones of the B3C3 population, using the Row-Column statistical design with three replications of ten plants in each experiment. Amarilis, Canchan and Chucmarina varieties were used as controls, the fertilization dose was 200-180-160 kg NPK per hectare and pest control was carried out as in a normal potato crop; no fungicides were used for late blight control because the clones are resistant to the disease. The experiment was conducted in the Chinchao-Huanuco locality. Harvesting took place 120 days after planting."
  
  uri <- "doi:10.21223/OOQ16Y"
  group <- "varieties_potato"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- data.frame(
      carobiner::read_metadata(uri, path, group, major=2, minor=0),
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
  
  f <- ff[grep("PT", basename(ff))]
  
  r <- carobiner::read.excel(f[1], sheet="Fieldbook")  
  installation <- carobiner::read.excel(f[4], sheet="Installation")
  
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
      country = 'Peru',
      adm1 = 'Huanuco',
      longitude = -76.331377,
      latitude = -9.894659,
      planting_date = "2020-11-07" ,
      harvest_date = "2021-04-11",
      trial_id = gsub(".xls", "", basename(f[1]))
  )
  
  df$on_farm <- TRUE
  df$is_survey <- FALSE
  df$irrigated <- FALSE
  df$treatment = "varieties"
  df$crop <- "potato"
  df$pathogen <- "Phytophthora infestans"
  df$yield_part <- "tubers"
  df$geo_from_source = FALSE
  df$N_fertilizer = 200
  df$P_fertilizer = 180
  df$K_fertilizer = 160 
  
  
  carobiner::write_files(path = path,
                         metadata = meta,
                         records = df)
  
 
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)
