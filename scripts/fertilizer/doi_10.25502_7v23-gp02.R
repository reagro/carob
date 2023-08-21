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
    
    %%%%%%%%%%The dataset is N2Africa agronomy trials - Uganda, 2016, I%%%%%%%%%%%%
    Crop: Climbing bean
    Crop system: intercropped with banana vs. sole
  
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
    carob_contributor="Samar Attaher",
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
              d3[,c(grep(paste0("plot_",1:6, collapse = "|"), colnames(d3[10:ncol(d3)]), value = T))])
  # reshape d3
  rr <- reshape(d3, direction='long', 
              varying=grep("plot_1", colnames(d3[8:ncol(d3)]), value = T),
              times=)
  
  
  carobiner::write_files (dset, d5, path=path)
  
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
