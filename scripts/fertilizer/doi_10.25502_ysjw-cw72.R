


carob_script <- function(path) {
  
  "Description:

  N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, 
  improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder
  farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise 
  in grain legume production and N2-fixation research and development will be the legacy of the project.
The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.

"
  
  uri <- "doi:10.25502/ysjw-cw72"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication= NA, 
    data_citation = "Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, P., Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira, L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller, K., Ampadu-Boakye, T., & Heerwaarden, J. van. (2020). N2Africa farm monitoring - Zimbabwe, 2011 - 2012 [dataset]. International Institute of Tropical Agriculture (IITA).
    https://doi.org/10.25502/YSJW-CW72" ,
    data_institutions = "IITA",
    carob_contributor="Cedric Ngakou",
    carob_date="2023-09-18",
    data_type="farm monitoring",
    project=NA 
  )
  
  ## download and read data 
  
  ff <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  bn <- basename(ff)
  
  # read the dataset
  
  r1 <- read.csv(ff[bn == "a_general.csv"])
  r2 <- read.csv(ff[bn == "a_inputs.csv"])
  r3 <- read.csv(ff[bn == "c_use_of_package_1.csv"])
  r4 <- read.csv(ff[bn == "c_use_of_package_2.csv"])
  r5 <- read.csv(ff[bn == "c_use_of_package_4.csv"])
  r6 <- read.csv(ff[bn == "d_cropping_calendar.csv"])
  r7 <- read.csv(ff[bn == "e_harvest.csv"])
  ## process file(s)
  ### Process General file containing locations information
  d <- r1[, c( "farm_id","country","season","district","vilage","gps_longitude","gps_latitude","farm_size")] 
       colnames(d) <- c("trial_id", "country","season","adm2","adm3","longitude","latitude","farm_size")
  
  ############
### process file with crop and management information
  
  d1 <- r4[, c("farm_id","plot_no", "crop_1","variety_1","inoculant_used","organic_fert_type","organic_fert_amount","mineral_fert_type","mineral_fert_amount")] 
        colnames(d1) <- c("trial_id","plot_no", "crop","variety","inoculated","OM_type","OM_applied","fertilizer_type","fertilizer_amount")
  
### merge d and d1
    d <- merge(d,d1,by="trial_id",all.x = T)    
        
 ###### process file with management information
  d2 <- r5[, c("farm_id","plot_no","crop_1_spacing_row_to_row","crop_1_spacing_plant_to_plant")] 
  colnames(d2) <- c("trial_id","plot_no", "row_spacing","plant_spacing")
 
  ### merge d and d2
  d <- merge(d,d2,by=c("trial_id","plot_no"),all.x = T)
  
  ######### process file with date  
  
  d3 <- r6[, c("farm_id","date_planting_dd","date_planting_mm","date_planting_yyyy")] 
  d3$planting_date <- paste(d3$date_planting_yyyy,d3$date_planting_mm,d3$date_planting_dd,sep = "-")
  d3 <- d3[,c("farm_id","planting_date")]
  colnames(d3) <- c("trial_id","planting_date")
  ## merge d and d3
  d <- merge(d,d3,by=("trial_id"),all.x = T)
  
  ### process file with yield data
  
  d4 <- r7[,c("farm_id","crop_1_area_harvested","crop_1_weight_grain","plot_no")]
  colnames(d4) <- c("trial_id","area_harvested","weight_grain","plot_no")
  
  ### merge d and d4
  d <- merge(d,d4,by=c("trial_id","plot_no"),all.x = T)
  
  d$yield <- (d$weight_grain/d$area_harvested)*10000 ## convert yield to kg/ha
  
  ## Extract Relevant columns 
  
  d <- d[,c("trial_id", "country","season","adm2","adm3","longitude","latitude","crop","variety","yield","inoculated","OM_applied","fertilizer_type","fertilizer_amount","row_spacing","plant_spacing","planting_date")]
  
  # Add columns
  d$dataset_id <- dataset_id
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  # fix lon and Lat 
  d$adm2 <- gsub(" ","",d$adm2)
  
  Geo <- data.frame(adm2=c("Mudzi","Hwedza","Guruve","Goromonzi","Chegutu","Murehwa","Makoni","MAKONI","Murewa"),
                     lat=c(-17.0516554,-18.6257778,-16.3431777,-17.8182737,-18.1867654,-17.8044363,-18.3848893,-18.3848893,-17.646074),
                     lon=c(32.5494492,31.5769196,30.6290426,31.3723592,30.39756,31.8388002,32.1371586,32.1371586,31.777541))
  # merge geo data 
  
  d <- merge(d,Geo,by="adm2")
  
  d$longitude <- d$lon
  d$latitude <- d$lat
  d$lon <- d$lat <- NULL
  # Fix fertilizer_type
  p <- carobiner::fix_name(d$fertilizer_type)
  p <- gsub(" and ","; ",p)
  p <- gsub(",",";",p)
  p <- gsub(",",";",p)
  p <- gsub(",",";",p)
  p <- gsub("\\+|/| &|&|,", "; ", p)
  p <- gsub("\\-|/| &|&|,", "; ", p)
  p <- gsub("Gypsum","gypsum",p)
  p <- gsub("Gypum","gypsum",p)
  p <- gsub("SSp","SSP",p)
  p <- gsub("No leaf harvesting","unknown",p)
  p <- gsub("No Leaf harvesting" ,"unknown",p)
  p <- gsub("no Leaf harvesting","unknown",p)
  p <- gsub("no Leaf harvesting","unknown",p)
  p <- gsub("Noleaf harvesting","unknown",p)
  p <- gsub("Noleaf harvesting","unknown",p)
  p <- gsub("Leaf harvesting","unknown",p)
  p <- gsub("leaves harvested","unknown",p)
  p <- gsub("leaves not harvested","unknown",p)
  p <- gsub("SSP ; harvesting leaves for re","SSP; unknown",p)
  p <- gsub("SSP ; gypsum","SSP; gypsum",p)
  p <- gsub("No; unknown" ,"unknown" ,p)
  p <- gsub("SSP gypsum" ,"SSP; gypsum" ,p)
  p <- gsub("SSP;gypsum" ,"SSP; gypsum" ,p)
  p <- gsub("Harveng leaves of leaves for r" ,"unknown" ,p)
  p <- gsub("Leaves not harvested"  ,"unknown" ,p)
  p <- gsub("SSP; no leaf harvesting"  ,"SSP; unknown" ,p)
  p <- gsub("SSP no leaf harvesting"  ,"SSP; unknown" ,p)
  p <- gsub("no leaf harvesting"  ,"unknown" ,p)
  p <- gsub("Leaves harvested"   ,"unknown" ,p)
  p <- gsub("leaf harvesting"   ,"unknown" ,p)
  p <- gsub("Leaves harvested; no SSP"   ,"SSP; unknown" ,p)
  p <- gsub("SSP ; leaf harvesting"   ,"SSP; unknown" ,p)
  p <- gsub("SSP; leaf harvesting"       ,"SSP; unknown" ,p)
  p <- gsub("SSP leaf harvesting"       ,"SSP; unknown" ,p)
  p <- gsub("SSP leaf harvesting"        ,"SSP; unknown" ,p)
  p <- gsub("SSP leaf harvesting"        ,"SSP; unknown" ,p)
  p <- gsub("2.5"         ,"unknown" ,p)
  p <- gsub("2"         ,"unknown" ,p)
  p <- gsub( "5"          ,"unknown" ,p)
  p <- gsub( "SSP unknown"           ,"SSP; unknown"  ,p)
  p <- gsub( "SSP ;  harvesting leaves for re"           ,"SSP; unknown"  ,p)
  p <- gsub( "SSP ;  gypsum"   ,"SSP; gypsum"   ,p)
  p <- gsub( "SSP ;  unknown"  ,"SSP; unknown"   ,p)
  p <- gsub( "unknown; no SSP"  ,"SSP; unknown"   ,p)
  p <- gsub( "Lime"  ,"lime"   ,p)
  d$fertilizer_type <- p
  
  #add fertilizer
  d$N_fertilizer <- 0
  d$P_fertilizer <- 0
  d$K_fertilizer <- 0
  i <- grepl("SSP",d$fertilizer_type ) |grepl("SSP; unknown",d$fertilizer_type ) |grepl("lime; SSP",d$fertilizer_type ) |grepl("SSP; gypsum",d$fertilizer_type )
  d$P_fertilizer[i] <- d$fertilizer_amount[i]
  
  
  d$fertilizer_amount <- NULL
  #fix country name
  dd <- carobiner::fix_name(d$country,"title")
  d$country <- dd
  
  # fix crop name 
  p <- carobiner::fix_name(d$crop,"lower")
  p <- gsub("climbing bean","common bean",p)
  p <- gsub( "bush bean" ,"common bean",p)
  d$crop <- p
  # remove NA in Crop 
  d <- d[!is.na(d$crop),]
  ## data type
  d$inoculated[d$inoculated=="N" ] <- FALSE
  d$inoculated[is.na(d$inoculated)] <- FALSE
  d$inoculated[d$inoculated=="Y"] <- TRUE
  d$inoculated <- as.logical(d$inoculated)
  d$planting_date <- as.character(as.Date(d$planting_date, format = "%Y-%m-%d"))
  ### fix crop yield
  d$k <- d$yield
  d$yield[d$crop=="common bean" & d$k>9000] <- NA
  d$yield[d$crop=="groundnut" & d$k>8500] <- NA
  d$yield[d$crop=="cowpea" & d$k>5000] <- NA
  d$k <- NULL
  
  # fix whitespace in variable
  d$fertilizer_type[d$fertilizer_type==""] <- NA
  d$adm2[d$adm2==""] <- NA
  d$adm3[d$adm3==""] <- NA
  d$inoculated[d$inoculated==""] <- NA
  d$variety[d$variety==""] <- NA
  
  d$yield_part <- "seed"
  d$yield_part[d$crop=="groundnut"] <- "grain"
  # all scripts must end like this
  carobiner::write_files(dset, d, path=path)
}
