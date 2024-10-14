# R script for "carob"



carob_script <- function(path) {
   
"Small land holdings are among the main constraints for smallholders to produce enough food and feed to meet household demands. During the main cropping season, when all the land is covered by stable crops, feed is critically in short supply. Africa RISING’s diagnostic surveys showed that during such times weeds growing with faba bean crop are used as important feed resource by farmers in Ethiopia. Farmers leave the weed to grow with the faba bean until a certain stage, which is against the recommended agronomic practice through the extension system. A series of experiments were conducted to explore the rationale behind farmers preference, involving three faba bean production practices: 1) the traditional management practice (where weeds are used as forage), 2) improved practice (where weeds are frequently removed from faba bean plots) and 3) intercropping faba bean with fodder oat. The dataset contains the grain yield, straw yield, forage yield and gross income from the different practices and faba bean varieties in the Lemo district of Africa RISING site."
  
   uri <- "doi:10.7910/DVN/JAOXFJ"
   group <- "agronomy" 
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- data.frame(
      carobiner::read_metadata(uri, path, group, major=3, minor=1), 
      data_institute = "CIAT", 
      publication = NA,
      project ="Africa RISING", 
      data_type = "experiment",
      response_vars = "yield;fwy_total",
      treatment_vars = "variety;intercrops;weeding_times", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-10-14"
   )
   
   f <- ff[basename(ff)=="data.xlsx"]
   
   ## processing data
   r <- carobiner::read.excel(f)
   ### faba bean yield
   d1<- data.frame(
      treatment= ifelse(grepl("Traditional", r$`Management practice`), "one late weeding", 
                 ifelse(grepl("Improved", r$`Management practice`), "two late weeding", "two late weeding + intercropped")),
      variety= r$`Faba bean Variety`,
      planting_date= ifelse(r$Year=="1","2015",
                            ifelse(r$Year=="2", "2016","2017")),
      farmer_gender= r$`Gender of HH`,
      yield= r$`Grain Yield (t/ha)`*1000,
      fwy_residue= r$`Straw Yield (t/ha)`*1000,
      crop_price= r$`Gross Return (ETB/ha)`,
      currency= "ETB",
      country= "Ethiopia",
      crop="faba bean",
      intercrops= ifelse(grepl("Intercropped",r$`Management practice`), "oats", "none"),
      weeding_times= as.integer(ifelse(grepl("Traditional", r$`Management practice`), 1, 2)),
      weeding_done= TRUE 
   )
   ### processing oats yield 
   d2 <- data.frame(
      treatment= ifelse(grepl("Traditional", r$`Management practice`), "one late weeding", 
                        ifelse(grepl("Improved", r$`Management practice`), "two late weeding", "two late weeding + intercropped")),
      planting_date= ifelse(r$Year=="1","2015",
                            ifelse(r$Year=="2", "2016","2017")),
      farmer_gender= r$`Gender of HH`,
      yield= r$`Forage Yield (t/ha)`*1000,
      country= "Ethiopia",
      crop="oats",
      intercrops= ifelse(grepl("Intercropped",r$`Management practice`), "faba bean", "none"),
      weeding_times= as.integer(ifelse(grepl("Traditional", r$`Management practice`), 1, 3)),
      weeding_done= TRUE 
   )
   
   d <- carobiner::bindr(d1, d2)
   d$location <- "Lemo"
   d$trial_id <- paste0(d$planting_date, "_", "Lemo")
   d$row_spacing <- 40
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$irrigated <- NA
   d$yield_part <- "grain"
   d$latitude <- 5.432054 
   d$longitude <- 37.8433
   d$geo_from_source <- FALSE
   
   ### Adding fertilizer
   d$fertilizer_type <- "DAP"  ## DAP: 18% N, 20.1% P (DAP= 100kg/ha)
   d$fertilizer_amount <- 100
   d$N_fertilizer <- 18
   d$P_fertilizer <- 20.1
   d$K_fertilizer <- 0
   ## fixing crop price 
   ## Assuming that all grain are marketable.
   d$crop_price <- d$crop_price/d$yield ### ETB/kg
   ### Removing duplicate (probably appear during the data collection process)
   d <- unique(d)
  
   carobiner::write_files (path, meta, d)
}


