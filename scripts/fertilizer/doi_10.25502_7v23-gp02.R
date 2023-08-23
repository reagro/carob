# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
	Description:

    N2Africa is to contribute to increasing biological nitrogen fixation 
    and productivity of grain legumes among African smallholder farmers 
    which will contribute to enhancing soil fertility, improving household
    nutrition and increasing income levels of smallholder farmers. 
    As a vision of success, N2Africa will build sustainable, long-term 
    partnerships to enable African smallholder farmers to benefit from 
    symbiotic N2-fixation by grain legumes through effective production 
    technologies including inoculants and fertilizers adapted to local settings. 
    A strong national expertise in grain legume production and N2-fixation research 
    and development will be the legacy of the project.
      
"
  uri <- "doi.org/10.25502/7v23-gp02"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
 
   ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project="N2Africa",
    uri=uri,
    publication=NA,
    data_citation = 'Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, P., Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira, L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller, K., Ampadu-Boakye, T., & Heerwaarden, J. van. (2020). N2Africa agronomy trials - Uganda, 2016, I [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/6H5E-Q472',
    data_institutions = "IITA",
    carob_contributor="Eduardo Garcia Bendito",
    data_type="experiment"
  )
  
  ## download and read data 
  ff <- carobiner::get_data(uri, path, group)
  js<-carobiner::get_metadata(dataset_id,path,group,major=1,minor = 0)
  dset$license <-carobiner::get_license (js)
 
  #read the data file
  f1 <- ff[basename(ff) == "general.csv"]
  # f2 <- ff[basename(ff) == "production.csv"] 
  f3 <- ff[basename(ff) == "experiment.csv"] 
  # read the dataset
  d1 <- data.frame(read.csv(f1))
  # d2 <- data.frame(read.csv(f2))
  d3 <- data.frame(read.csv(f3))
  
  # Subset d1
  colnames(d1)[c(3,4,5,6,8,9)] <- c("trial_id", "obs_day", "obs_month", "obs_year", "adm1", "adm2")
  colnames(d1)[c(59,60,61)] <- c("harvest_date_day", "harvest_date_month", "harvest_date_year")
  d1 <- d1[,c("trial_id", "obs_day", "obs_month", "obs_year", "adm1", "adm2","harvest_date_day", "harvest_date_month", "harvest_date_year")]
  # # Subset d2
  # d2 <- d2[,c(3:8,13:16,49:52,65:81,133:138)]
  # colnames(d2) <- c("trial_id", "area_field1", "area_field2", "area_field2", "area_field3", "area_field4", "area_field_unit",
  #                   "main_crop_field1", "main_crop_field2", "main_crop_field3", "main_crop_field4",
  #                   "variety_field1", "variety_field2", "variety_field3", "variety_field4",
  #                   "amount_mineral_fertilizer_field1", "amount_mineral_fertilizer_field2", "amount_mineral_fertilizer_field3", "amount_mineral_fertilizer_field4","amount_fertilizer_unit",
  #                   "fertilizer_type_field1", "fertilizer_type_field2", "fertilizer_type_field3", "fertilizer_type_field4",
  #                   "OM_used_field1", "OM_used_field2", "OM_used_field3", "OM_used_field4",
  #                   "innocuated_field1", "innocuated_field2", "innocuated_field3", "innocuated_field4",
  #                   "50pct_flowering_day", "50pct_flowering_month", "50pct_maturity_day", "50pct_maturity_month")
  # Subset d3
  d3 <- d3[,c(3,5:12,86:297)]
  d3 <- cbind(d3[,c(1:7)],
              d3[,c(grep(paste0("plot_",1:6, collapse = "|"), colnames(d3[10:ncol(d3)]), value = T))],
              d3[,c(grep("experimental_treatments_", colnames(d3[10:ncol(d3)]), value = T))])
  # reshape d3
  rr <- reshape(d3,
              direction='long', 
              varying=list(names(d3)[8:ncol(d3)]),
              v.names = "value",
              idvar = "farm_id",
              timevar = "var",
              times = colnames(d3)[8:ncol(d3)])
  rownames(rr) <- 1:nrow(rr)
  for(i in 1:6){
    rrr <- reshape(rr[rr$var %in% grep(paste0(paste0("plot_",i), "|experimental_treatments_"), rr$var, value = T),
                      c("farm_id", paste0("name_treatment_",i), "var", "value")],
           idvar = c("farm_id", paste0("name_treatment_",i)), timevar = "var",
           direction='wide')
    rrr$plot <- i
    colnames(rrr) <- gsub("value.", "", colnames(rrr))
    colnames(rrr) <- gsub(paste0("_plot_",i), "", colnames(rrr))
    colnames(rrr)[1:2] <- c("trial_id", "treatment")
    colnames(rrr) <- gsub("\\..*", "", colnames(rrr))
    colnames(rrr)[c(3,4,6,7,8)] <- c("plot_width", "plot_length", "yield", "residue", "biomass_total")
    if(i == 1){d <- rrr}
    else{d <- rbind(d,rrr)}
  }
  
  # Merge site info and agronomy info
  d <- merge(d, d1, by = "trial_id")
  
  # Standardization
  d$trial_id <- paste(d$trial_id, d$plot, sep = "_")
  d$country <- "Tanzania"
  d$date <- paste(as.integer(d$obs_year),
                  ifelse(d$obs_month == 'December', as.integer(12), as.integer(11)),
                  sprintf('%02d', d$obs_day),
                  sep = "-")
  # # EGB:
  # # This is not included because how can harvest date be after the survey?
  # d$harvest_date <- paste(as.integer(d$harvest_date_year),
  #                         ifelse(d$harvest_date_month == 'January', '01', '02'),
  #                         sprintf('%02d', d$harvest_date_day),
  #                         sep = "-")
  d$on_farm <- TRUE
  d$is_survey <- TRUE
  # d$treatment <- 
  d$crop <- ifelse(d$experimental_treatments_crop_1 == trimws("bush bean"), "common bean", NA)
  d$variety <- trimws(d$experimental_treatments_variety_crop_1)
  
  # Fertilizer part
  d$fertilizer_type <- 'none'
  d$fertilizer_type[grepl('\\+', d$treatment)] <- "PK"
  d$fertilizer_type[grep('pk', d$treatment)] <- "PK"
  d$fertilizer_type[grep('npk', d$treatment)] <- "NPK"
  d$fertilizer_type[grepl('mpal', d$treatment)] <- "sympal"
  d$N_fertilizer <- 0
  d$N_fertilizer <- ifelse(d$fertilizer_type == "NPK",
                           ((as.numeric(d$fert_1_kg_plot) / (as.numeric(d$plot_width)*as.numeric(d$plot_length))) * 10000) * 0.1, # Assumed to be NPK (10:18:24)
                           d$N_fertilizer)
  d$N_fertilizer <- ifelse(d$fertilizer_type == "PK",
                           ((as.numeric(d$fert_2_kg_plot) / (as.numeric(d$plot_width)*as.numeric(d$plot_length))) * 10000) * 0.1, # Assumed to be NPK (10:18:24)
                           d$N_fertilizer)
  d$N_fertilizer <- ifelse(d$fertilizer_type == "sympal",
                           ((as.numeric(d$fert_3_kg_plot) / (as.numeric(d$plot_width)*as.numeric(d$plot_length))) * 10000) * 0, # Sympal has 0 % N
                           d$N_fertilizer)
  d$P_fertilizer <- 0
  d$P_fertilizer <- ifelse(d$fertilizer_type == "NPK",
                           ((as.numeric(d$fert_1_kg_plot) / (as.numeric(d$plot_width)*as.numeric(d$plot_length))) * 10000) * 0.18, # Assumed to be NPK (10:18:24)
                           d$P_fertilizer)
  d$P_fertilizer <- ifelse(d$fertilizer_type == "PK",
                           ((as.numeric(d$fert_2_kg_plot) / (as.numeric(d$plot_width)*as.numeric(d$plot_length))) * 10000) * 0.18, # Assumed to be NPK (10:18:24)
                           d$P_fertilizer)
  d$P_fertilizer <- ifelse(d$fertilizer_type == "sympal",
                           ((as.numeric(d$fert_3_kg_plot) / (as.numeric(d$plot_width)*as.numeric(d$plot_length))) * 10000) * 0.23, # Sympal has 0 % N
                           d$P_fertilizer)
  d$K_fertilizer <- 0
  d$K_fertilizer <- ifelse(d$fertilizer_type == "NPK",
                           ((as.numeric(d$fert_1_kg_plot) / (as.numeric(d$plot_width)*as.numeric(d$plot_length))) * 10000) * 0.24, # Assumed to be NPK (10:18:24)
                           d$K_fertilizer)
  d$K_fertilizer <- ifelse(d$fertilizer_type == "PK",
                           ((as.numeric(d$fert_2_kg_plot) / (as.numeric(d$plot_width)*as.numeric(d$plot_length))) * 10000) * 0.24, # Assumed to be NPK (10:18:24)
                           d$K_fertilizer)
  d$K_fertilizer <- ifelse(d$fertilizer_type == "sympal",
                           ((as.numeric(d$fert_3_kg_plot) / (as.numeric(d$plot_width)*as.numeric(d$plot_length))) * 10000) * 0.15, # Sympal has 0 % N
                           d$K_fertilizer)
  d$OM_used <- NA
  d$OM_used[grepl('\\+', d$treatment)] <- TRUE
  d$OM_type <- ifelse(d$OM_used == TRUE, "farmyard manure", NA)
  d$OM_applied <- ifelse(d$OM_used == TRUE,
                         ((as.numeric(d$manure_kg_plot) / (as.numeric(d$plot_width)*as.numeric(d$plot_length))) * 10000),
                         NA)
  # Yield
  d$yield <- (as.numeric(d$yield) / (as.numeric(d$plot_width)*as.numeric(d$plot_length))) * 10000 # kg/ha
  d$yield_part <- "grain"
  d$residue_yield <- (as.numeric(d$residue) / (as.numeric(d$plot_width)*as.numeric(d$plot_length))) * 10000 # kg/ha
  d$biomass_total <- (as.numeric(d$biomass_total) / (as.numeric(d$plot_width)*as.numeric(d$plot_length))) * 10000 # kg/ha
  
  # Other
  d$irrigated <- FALSE
  d$row_spacing <- as.numeric(d$experimental_treatments_density_1_row_spacing)
  d$plant_spacing <- as.numeric(d$experimental_treatments_density_1_plant_spacing)
  d$plant_density <- (as.numeric(d$plot_width)/(as.numeric(d$row_spacing)/100)) * (as.numeric(d$plot_length)/(as.numeric(d$plant_spacing)/100)) # plants/plot
  d$plant_density <- (as.numeric(d$plant_density) / (as.numeric(d$plot_width)*as.numeric(d$plot_length)) * 10000) # plants/ha
  
  # # EGB:
  # # There is info on herbicide
  
  # Subset final
  d <- d[,c('trial_id','treatment','country','date','on_farm','is_survey','crop','variety','fertilizer_type','N_fertilizer','P_fertilizer','K_fertilizer',
            'OM_used', 'OM_type', 'OM_applied', 'yield', 'yield_part', 'residue_yield', 'biomass_total', 'irrigated', 'row_spacing', 'plant_spacing', 'plant_density')]
  d$dataset_id <- dataset_id
  
  carobiner::write_files (dset, d, path=path)
  
}

# # EGB: Georeferencing
# s <- unique(d3[,c("country", "adm1", "adm2", "adm4")])
# s$latitude <- NA
# s$longitude <- NA
# for (i in 1:nrow(s)) {
#   if(is.na(s$latitude[i]) | is.na(s$longitude[i])){
#     ll <- carobiner::geocode(country = s$country[i], adm1 = s$adm1[i], adm2 = s$adm2[i], location = s$adm4[i], service = "geonames", username = "efyrouwa")
#     ii <- unlist(jsonlite::fromJSON(ll))
#     c <- as.integer(ii["totalResultsCount"][[1]])
#     s$latitude[i] <- as.numeric(ifelse(c == 1, ii["geonames.lat"][1], ii["geonames.lat1"][1]))
#     s$longitude[i] <- as.numeric(ifelse(c == 1, ii["geonames.lng"][1], ii["geonames.lng1"][1]))
#   }
# }
# s <- dput(s)
