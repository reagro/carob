carob_script <- function(path) {
  
  "Description:

  N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, 
  improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder
  farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise 
  in grain legume production and N2-fixation research and development will be the legacy of the project.
The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.

"
  
  uri <- "doi:10.25502/Y18Z-6T60"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication= NA, 
    data_citation = "Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, P., Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira, L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller, K., Ampadu-Boakye, T., & Heerwaarden, J. van. (2020). N2Africa impact survey - Democratic Republic of Congo, 2013 [Data set]. International Institute of Tropical Agriculture (IITA). 
    https://doi.org/10.25502/Y18Z-6T60" ,
    data_institutions = "IITA",
    carob_contributor="Cedric Ngakou",
    data_type="experiment",
    project=NA 
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  
  f <- ff[basename(ff) == "a_general_1.csv"] 
  f1 <- ff[basename(ff) == "a_general_2.csv"] 
  f2 <- ff[basename(ff) == "a_general_3.csv"] 
  f3 <- ff[basename(ff) == "a_general_4.csv"] 
  f4 <- ff[basename(ff) == "b_inputs_1.csv"] 
  f5 <- ff[basename(ff) == "b_inputs_2.csv"] 
  f6 <- ff[basename(ff) == "c_land_holding_management.csv"]
  f7 <- ff[basename(ff) == "d_crop_production_use.csv"]
  f8 <- ff[basename(ff) == "e_changes_area_in_last_4_years.csv"]
  f9 <- ff[basename(ff) == "e_changes_legume_grain_process.csv"]
  f10 <- ff[basename(ff) == "e_changes_legume_haulm_process.csv"]
  f11 <- ff[basename(ff) == "e_changes_production_use.csv"]
  f12 <- ff[basename(ff) == "e_changes_production_use_2.csv"]
  f13 <- ff[basename(ff) == "e_legumes_produced_4_years_ago.csv"]
  f14 <- ff[basename(ff) == "f_nutrition_1.csv"]
  
  # read the dataset
  r <- read.csv(f)
  r1 <- read.csv(f1)
  r2 <- read.csv(f2)
  r3 <- read.csv(f3)
  r4 <- read.csv(f4)
  r5 <- read.csv(f5)
  r6 <- read.csv(f6)
  r7 <- read.csv(f7)
  r8 <- read.csv(f8)
  r9 <- read.csv(f9)
  r10 <- read.csv(f10)
  r11 <- read.csv(f11)
  r12 <- read.csv(f12)
  r13 <- read.csv(f13)
  r14 <- read.csv(f14)
  
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
  
  colnames(d1) <- c("trial_id","farm_size","inoculation_type","treatment")
  
  d2 <- r7[, c("farm_id", "crop","total_production_farm","weight_unit")] 
  
  colnames(d2) <- c("trial_id", "crop","yield1","yield_unit")
  
  #merge d1 and d
  d2<-merge(d1,d2,by="trial_id")
  d2$yield1[d2$yield1=="12O"]<-"120"
  d2$yield1[grepl("UNKNOWN",d2$yield1) | d2$yield1==""]<-NA
  d2$yield1<- as.numeric(d2$yield1)
  
  # process 
  d3<-r12[,c("farm_id","legume_area_now_ha","crop","yield_amount_now","yield_unit_now")]
  colnames(d3)<- c("trial_id","farm_size","crop","yield1","yield_unit")
  d3$inoculation_type<- NA
  d3$treatment<- NA
  # append d3 and d2
  d3 <- rbind(d3,d2)
  
  d3$yield <- ifelse((d3$yield_unit=="kg"| d3$yield_unit=="Kg" | d3$yield_unit=="kgs" | d3$yield_unit=="KG" | d3$yield_unit==" kg") & d3$farm_size >0,d3$yield1/d3$farm_size,d3$yield1)
  
  # merge d and d3
  d <- merge(d,d3,bx="trial_id", all.y = T)
  d$inoculated<-FALSE
  d$inoculated[!is.na(d$inoculation_type)| d$inoculation_type !=""]<- TRUE
  d <- d[, c("country", "trial_id", "location","adm1","adm2","longitude", "latitude","crop", "yield","treatment","inoculated")]
  #process
  d4<-r10[,c("farm_id","haulm_use_now")]
  colnames(d4)<-c("trial_id","OM_type")
  #merge d and d4
  d<-merge(d,d4,by="trial_id",all.x = T)
  # Add columns
  d$dataset_id <- dataset_id
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  
  #add fertilizer
  d$N_fertilizer<-0
  d$P_fertilizer<- 0
  d$K_fertilizer<- 0
  d$N_fertilizer[d$treatment=="NPK 17-17-17"] <- 17
  d$P_fertilizer[d$treatment=="NPK 17-17-17"] <- 17
  d$K_fertilizer[d$treatment=="NPK 17-17-17"] <- 17
  
  #fix country name
  dd<-carobiner::fix_name(d$country,"title")
  d$country<-dd
  d$country[d$country=="D.R. Congo"]<-"Democratic Republic of the Congo"
  # fix crop name 
  p<- carobiner::fix_name(d$crop,"lower")
  p<-gsub("soyabean","soybean",p)
  p<-gsub("sweet potato","sweetpotato",p)
  p<-gsub("banana trees","banana",p)
  p<-gsub("seed patato","potato",p)
  p<-gsub("sweet-potato","sweetpotato",p)
  p<-gsub("trees","no crop",p)
  p<-gsub("sugar canne","sugarcane",p)
  p<-gsub("soya","soybean",p)
  p<-gsub("soybeans","soybean",p)
  p<-gsub("banana tree","banana",p)
  p<-gsub("groundnuts","groundnut",p)
  p<-gsub("aubergine","eggplant",p)
  p<-gsub("soybeanbean","soybean",p)
  p<-gsub("sugar cane","sugarcane",p)
  p<-gsub("cabbages","eggplant",p)
  p<-gsub("soy","soybean",p)
  p<-gsub("sweet patato","sweetpotato",p)
  p<-gsub("legumes","legume",p)
  p<-gsub("soya beans","soybean",p)
  p<-gsub("soja","soybean",p)
  p<-gsub("oignon","onion",p)
  p<-gsub("soyabeans","soybean",p)
  p<-gsub("sweet potatoes","sweetpotato",p)
  p<-gsub("potatoes","potato",p)
  p<-gsub("patate douce","sweetpotato",p)
  p<-gsub("patate","sweetpotato",p)
  p<-gsub("colocasia","taro",p)
  p<-gsub("cassava bean","cassava",p)
  p<-gsub("peanut","groundnut",p)
  p<-gsub("manioc","cassava",p)
  p<-gsub("mais","maize",p)
  p<-gsub("green pea","pea",p)
  p<-gsub("goundnut","groundnut",p)
  p<-gsub("sweet poato","sweetpotato",p)
  p<-gsub("colocase","taro",p)
  p<-gsub("groungnut","groundnut",p)
  p<-gsub("banania","banana",p)
  p<-gsub("sobean","soybean",p)
  p<-gsub("soybeanbean beans","soybean",p)
  p<-gsub("sean","common bean",p)
  p<-gsub("ben","common bean",p)
  p<-gsub("haricot","common bean",p)
  p<-gsub("beans","common bean",p)
  p<-gsub("soybeanbean","soybean",p)
  
  d$crop<-p
  d$crop[d$crop=="bean"]<- "common bean"
 d$adm2[d$adm1=="MUMOSHO"]<- "MUMOSHO"
 
  # fix long and lat coordinate
  geo<- data.frame(adm2=c("Lulanga","lukunga","kalirine","kasheke","bishwira","bushwira","nfuzi","ikoma","ishungu","buderhe","Buyenga","mufa","IGOBEGOBE","CHIMBI 2","MULAMBI","BURHEMBO","MUMOSHO","CHANIA","BWIREMBE","BUREMBO","Shebeye","KALIRINA","BURHWAGA","CAGOMBE","Chirheja","CIRHOGOLE","cirimba","ibamba","Irambi","IRAMBO","KABIBANGA", "Kabiganda","kalunga","Kananda","karhwa","Karhwa","KARWA","Kashisiraboba","LUGANDA","mazinzi","Mazinzi","MAZINZI","MULAMBA","mulambi","mumosho","Ndola","Nshebeyi","NYANGEZI"),
                   lat=c(-0.5220252, -4.371491,-2.7046167,-2.1518846,-2.44139,-2.4413,-2.7708003,-2.5557499,-2.300289,-2.34904,-2.6279646,-2.6279646,-3.2968958,-2.6279646,-2.7708003,-2.6279646,-2.6279646,-3.2968958,-3.2968958,-2.6560833,-2.3883942,-2.259,-2.7708003,-3.2968958,-2.2509531,-2.3883942,-2.6560833,-1.4715551,-2.1065462,-2.1065462,-2.6560833,-2.6560833,-5.5390095,-4.23383,-2.7008167,-2.7008167,-2.7008167,-2.6560833,-2.4991556,-2.6560833,-2.6560833,-2.6560833,-2.259,-2.8146579,-2.6279646,-2.7065333,-2.3883942,-2.6560833),
                   long=c(24.5609263,15.2279106,28.5622667,28.8560076,28.73278,28.73278,28.6000504,28.7399014,28.9415696,28.89343,28.8819501,28.8819501,28.1674008,28.8819501,28.6000504,28.8819501,28.8819501,28.1674008,28.1674008,28.86975,28.7938066,29.0447778,28.6000504,28.1674008,28.8140382,28.7938066,28.86975,18.7718785,28.9186227,28.9186227,28.86975,28.86975,19.0386855,28.91839,28.5701333,28.5701333,28.5701333,28.86975,28.8343339,28.86975,28.86975,28.86975,29.0447778,28.6950944,28.8819501,28.5760167,28.7938066,28.86975))
   
  d<-merge(d,geo,by="adm2",all.x = T)
  i<-!is.na(d$lat)
  d$latitude[i]<- d$lat[i]
  d$longitude[i]<- d$long[i]
  d$lat<- d$long<- NULL
  d$country[d$location=="south kivu"]<-"Democratic Republic of the Congo"
  d$longitude[d$location=="sud kivu" ]<- 27.8705717
  d$latitude[d$location=="sud kivu"]<- -3.2653155
  #fix crop yield limit by crop
  d$yield[d$crop=="common bean" & d$yield>9000]<- NA
  d$yield[d$crop=="groundnut" & d$yield>8500]<- NA
  d$yield[d$crop=="soybean" & d$yield>15000]<- NA
 # remove crop with very low yield value after divided by the plot area and it's not realistic
  d<-d[d$crop!="eggplant"&d$crop!="legume"&d$crop!="onion"&d$crop!="pea"&d$crop!="quinquina"&d$crop!="taro"&d$crop!="tomato"&d$crop!="yam"&d$crop!="cabbage"&d$crop!="amaranth"&d$crop!="no crop",]
  
  
  # fix whitespace in variable
  d$treatment[d$treatment==""]<-NA
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
