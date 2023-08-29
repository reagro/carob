


carob_script <- function(path) {
  
  "Description:

  N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, 
  improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder
  farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise 
  in grain legume production and N2-fixation research and development will be the legacy of the project.
The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.

"
  
  uri <- "doi:10.25502/K1TM-5012"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication= NA, 
    data_citation = "Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, P., Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira, L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller, K., Ampadu-Boakye, T., & Heerwaarden, J. van. (2020). N2Africa impact survey - Rwanda, 2013 [dataset]. International Institute of Tropical Agriculture (IITA).
    https://doi.org/10.25502/K1TM-5012" ,
    data_institutions = "IITA",
    carob_contributor="Cedric Ngakou",
    data_type="survey",
    project=NA 
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  
  f <- ff[basename(ff) == "a_general_1.csv"] 
  f6 <- ff[basename(ff) == "c_land_holding_management.csv"]
  f7 <- ff[basename(ff) == "d_crop_production_use.csv"]
  f10 <- ff[basename(ff) == "e_changes_legume_haulm_process.csv"]
  f12 <- ff[basename(ff) == "e_changes_production_use_2.csv"]
  # read the dataset
  r <- read.csv(f)
  r6 <- read.csv(f6)
  r7 <- read.csv(f7)
  r10 <- read.csv(f10)
  r12 <- read.csv(f12)
  
  
  ## process file(s)
  d<- r[, c("farm_id","country","sector_state","action_site","village","gps_latitude","gps_longitude","gps_latitude_dec","gps_longitude_dec")] 
  
  colnames(d) <- c("trial_id", "country","location","adm1","adm2","latitude1","longitude1","latitude2","longitude2")
  
  # fix long and lat
  i<-is.na(d$latitude1)
  d$latitude1[i]<-d$latitude2[i]
  
  i<-is.na(d$longitude1)
  d$longitude1[i]<-d$longitude2[i]
  d$longitude<-d$longitude1
  d$latitude<- d$latitude1
  
  d1 <- r6[, c("farm_id","farm_size_ha","inoculant_type","other_min_fert_type")] 
  
  colnames(d1) <- c("trial_id","farm_size","inoculation_type","fertilizer_type")
  
  d2 <- r7[, c("farm_id", "crop","total_production_farm","weight_unit")] 
  
  colnames(d2) <- c("trial_id", "crop","yield1","yield_unit")
  
  #merge d1 and d
  d2<-merge(d1,d2,by="trial_id")
  # remove word in yield value
  d2<-d2[d2$yield1!="NOTYET" & d2$yield1!="LOSS" & d2$yield1!="DAMAGEDBYFLOOD" & d2$yield1!="STILLINFIELD" & d2$yield1!="STILLINTHEFIELD" & d2$yield1!="NOTYETHARVESTED", ]
  # process 
  d3<-r12[,c("farm_id","legume_area_now_ha","crop","yield_amount_now","yield_unit_now")]
  colnames(d3)<- c("trial_id","farm_size","crop","yield1","yield_unit")
  d3$inoculation_type<- NA
  d3$fertilizer_type<- NA
  # append d3 and d2
  d3 <- rbind(d3,d2)
  d3$yield1 <- as.numeric(d3$yield1)
  d3$yield <- ifelse((d3$yield_unit=="kg") & d3$farm_size >0,d3$yield1/d3$farm_size,
                     ifelse(d3$yield_unit=="Kg/0.15ha" & d3$farm_size >0,d3$yield1*0.15,d3$yield1))
                            
  d3<-d3[d3$yield_unit!=""  & d3$yield_unit!="na" & d3$yield_unit!="pineaple",]
  
  # merge d and d3
  d <- merge(d,d3,bx="trial_id", all.y = T)
  d$inoculated<-FALSE
  d$inoculated[!is.na(d$inoculation_type)| d$inoculation_type !=""]<- TRUE
  d <- d[, c("country", "trial_id", "location","adm1","adm2","longitude", "latitude","crop", "yield","fertilizer_type","inoculated")]
  #process
  d4<-r10[,c("farm_id","haulm_use_now")]
  colnames(d4)<-c("trial_id","OM_type")
  #merge d and d4
  d<-merge(d,d4,by="trial_id",all.x = T)
  # Add columns
  d$dataset_id <- dataset_id
  d$on_farm <- FALSE
  d$is_survey <- TRUE
  d$irrigated <- FALSE
  
  # DAP content: 18% of N  and 46% P205 
  # P apply is 30kg/ha in rwanda 
  #Urea was applied at a rate of 60 kg N/ha in Kenya and Rwanda trials
  #add fertilizer
  d$N_fertilizer<-0
  d$P_fertilizer<- 0
  d$K_fertilizer<- 0
  d$N_fertilizer[d$fertilizer_type=="Urea"] <- 60*0.46
  d$N_fertilizer[d$fertilizer_type=="DAP"|d$fertilizer_type=="DAP+manure"] <- (30*0.18)/0.46
  d$P_fertilizer[d$fertilizer_type=="DAP"|d$fertilizer_type=="DAP+manure"] <- 30
  #fix country name
  dd<-carobiner::fix_name(d$country,"title")
  d$country<-dd
  d$country[is.na(d$country)]<-"Rwanda"
  # fix crop name 
  p<- carobiner::fix_name(d$crop,"lower")
  p<-gsub("soybeans","soybean",p)
  p<-gsub("Sweet potatoes","sweetpotato",p)
  p<-gsub("sweet potatoes","sweetpotato",p)
  p<-gsub("sweet potatoes","sweetpotato",p)
  p<-gsub("irish potatoes","potato",p)
  p<-gsub("potatoes","potato",p)
  p<-gsub("peas","pea",p)
  p<-gsub("climbing beans","common bean",p)
  p<-gsub("common beans","common bean",p)
  p<-gsub("beans","common bean",p)
  p<-gsub("tomatoes","tomato",p)
  p<-gsub("onions","onion",p)
  p<-gsub("cocoyam","taro",p)
  p<-gsub("amaranths","amaranth",p)
  p<-gsub("groundnuts","groundnut",p)
  p<-gsub("kitchen garden","vegetables",p)
  p<-gsub("cofffee","coffee",p)
  p<-gsub("red onions","onion",p)
  p<-gsub("red onion","onion",p)
  p<-gsub("Banana","banana",p)
  p<-gsub("peanuts","groundnut",p)
  p<-gsub("irish potaotes","potato",p)
  d$crop<-p
  d$crop[is.na(d$crop)]<- "no crop"
  # Fix fertilizer_type
  p<- carobiner::fix_name(d$fertilizer_type)
  p<-gsub("Urea","urea",p)
  d$fertilizer_type<- p
  d$fertilizer_type[d$fertilizer_type=="DAP+manure"]<- "DAP"
  
  #fix crop yield limit by crop
  d$yield[d$crop=="common bean" & d$yield>9000]<- NA
  d$yield[d$crop=="groundnut" & d$yield>8500]<- NA
  d$yield[d$crop=="soybean" & d$yield>15000]<- NA
  # remove crop with very low yield value after divided by the plot area
  d<-d[d$crop!="coffee"&d$crop!="no crop",]
  
  # fix whitespace in variable
  d$fertilizer_type[d$fertilizer_type==""]<-NA
  d$location[d$location==""]<-NA
  d$adm1[d$adm1==""]<-NA
  d$adm2[d$adm2==""]<-NA
  d$OM_type[d$OM_type==""]<-NA
  # add column
  d$OM_used<- FALSE
  d$OM_used[!is.na(d$OM_type)]<-TRUE

  d$yield_part<-"seed"
  # all scripts must end like this
  carobiner::write_files(dset, d, path=path)
}
