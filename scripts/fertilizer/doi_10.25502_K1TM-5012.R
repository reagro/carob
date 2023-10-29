


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
    carob_date="2023-08-20",
    data_type="survey",
    project=NA 
  )
  
  ## download and read data 
  
  ff <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  ff <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  f <- ff[basename(ff) == "a_general_1.csv"] 
  f1 <- ff[basename(ff) == "c_land_holding_management_2.csv"]
  f2 <- ff[basename(ff) == "d_crop_production_use.csv"]
  f4 <- ff[basename(ff) == "e_changes_production_use_2.csv"]
  f5 <- ff[basename(ff) == "c_land_holding_management.csv"]
  # read the dataset
  r <- read.csv(f)
  r1 <- read.csv(f1)
  r2 <- read.csv(f2)
  r4 <- read.csv(f4)
  r5 <- read.csv(f5)
  
  ## process file(s)
  d <- r[, c("farm_id","country","sector_state","action_site","village","gps_latitude","gps_longitude","gps_latitude_dec","gps_longitude_dec")] 
  
  colnames(d) <- c("trial_id", "country","location","adm2","adm3","latitude1","longitude1","latitude2","longitude2")
  
  # fix long and lat
  i <- is.na(d$latitude1)
  d$latitude1[i] <- d$latitude2[i]
  
  i <- is.na(d$longitude1)
  d$longitude1[i] <- d$longitude2[i]
  d$longitude <- d$longitude1
  d$latitude <- d$latitude1
  
  # process management file
  # use stringr:: if need be do not call library(stringr) or any other library
  # library("stringr")
  d1 <- r1[, c("farm_id","size_ha","crops_grown","varieties","min_fert_type","harvest_amount","inoculant_applied","weight_unit","min_fert_amount")] 
  colnames(d1) <- c("trial_id","farm_size","crop","variety","fertilizer_type","yield1","inoculation_type","yield_unit","fertilizer_amount")
  
  i <- c(50,40,60,90,70,30,20,35,80,10,5,45,15,55,6,65,100,1,2,3,4,7,8,43,13,88,0)
  p <- gsub("%", "", d1$crop)
  for(j in i){
    p <- gsub(j,"",p)
  }
  d1$crop <- p
  
  d1[c('crop', 'intercrops')] <- stringr::str_split_fixed(d1$crop, "[                                           ]", 2)
  
  d1[c('yield_unit', 'yield_unit1')] <- str_split_fixed(d1$yield_unit, "[                                                               ]", 2)
  
  d1 <- d1[,c("trial_id","farm_size","crop","variety","intercrops","fertilizer_type","yield1","inoculation_type","yield_unit","fertilizer_amount")]
  
  # merge d and d1
  d1 <- merge(d,d1,by="trial_id",All.X=T)
  
  ## fix crop name in d1
  p <- carobiner::fix_name(d1$crop,"lower")
  p <- gsub("sweet","sweetpotato",p)
  p <- gsub("soybeans","soybean",p)
  p <- gsub("groundnuts"  ,"groundnut" ,p)
  p <- gsub("cabbages"    ,"cabbage" ,p)
  p <- gsub("potatoes"    ,"potato" ,p)
  p <- gsub("irish"    ,"potato" ,p)
  p <- gsub("common"    ,"common bean" ,p)
  p <- gsub("climbing"    ,"common bean" ,p)
  p <- gsub("Beans"    ,"common bean" ,p)
  p <- gsub("onions"    ,"onion" ,p)
  p <- gsub("tomatoes"    ,"tomato" ,p)
  p <- gsub("pineaple"    ,"pineapple" ,p)
  d1$crop <- p
  
  # fix intercrops
  II <- carobiner::fix_name(d1$intercrops,"lower")
  II <- gsub("                        ","",II)
  II <- gsub("          ","",II)
  II <- gsub("                            ","",II)
  II <- gsub("                                  ","",II)
  II <- gsub(",  potatoes","potato",II)
  II <- gsub("irish potatoes" ,"potato",II)
  II <- gsub("sweet potatoes" ,"sweetpotato",II)
  II <- gsub("potaotes","potato",II)
  II <- gsub("groundnuts" ,"groundnut",II)
  II <- gsub("soybeans" ,"soybean",II)
  II <- gsub("climbing beans" ,"common bean",II)
  II <- gsub("crops" ,"no crop",II)
  II <- gsub("animal" ,"no crop",II)
  II <- gsub("garden" ,"no crop",II)
  II <- gsub("tree" ,"no crop",II)
  II <- gsub("9,    peas"  ,"pea" ,II)
  II <- gsub("peas"  ,"pea" ,II)
  II <- gsub("& spinach"   ,"spinach"  ,II)
  II <- gsub(",   beans"   ,"common bean"  ,II)
  II <- gsub("maize     vegetables"   ,"maize; vegetables"  ,II)
  II <- gsub("beans   pea"   ,"common bean; pea"  ,II)
  II <- gsub("potatoes     cassava"   ,"potato; cassava"  ,II)
  II <- gsub("beans  maize"   ,"common bean; maize"  ,II)
  II <- gsub("beans   maize"    ,"common bean; maize"  ,II)
  II <- gsub("beans   banana"     ,"common bean; banana"  ,II)
  II <- gsub("beans"      ,"common bean"  ,II)
  II <- gsub("potatoes","potato",II)
  d1$intercrops <- II
  d1$intercrops[d1$intercrops=="bean"] <- "common bean"
  d1$intercrops[d1$intercrops=="crop"] <- "no crop"
  # remove bad value of yield in the data
  d1 <- d1[d1$yield1 !="NOTYET" & d1$yield1 !="130NOTYETHARVESTED" & d1$yield1 !="67NA" & d1$yield1 !="1NA" & d1$yield1 !="NOTYET100" & d1$yield1 !="NOTYET75" & d1$yield1 !="100NOTYET"
          & d1$yield1 !="25NOTYET" & d1$yield1 !="STILLINTHEFIELD" & d1$yield1 !="40-6" & d1$yield1 !="80NOTYET" & d1$yield1 !="70NA" & d1$yield1 !="80NA" 
          & d1$yield1 !="20NOTYET" & d1$yield1 !="DAMAGEDBYFLOOD" & d1$yield1 !="NOTYETHARVESTED" & d1$yield1 !="20NOTYETHARVESTED" & d1$yield1 !="45NOTYETHARVESTED" & d1$yield1 !="NOTYETHARVESTED40"
          & d1$yield1 !="THEYARESTILLGROWING" & d1$yield1 !="STILLINFIELD3" & d1$yield1 !="70STILLINFIELD"& d1$yield1 !="95STILLINTHEFIELD" & d1$yield1 !="STILLINFIELD" 
          & d1$yield1 !="300STILLINFIELD" & d1$yield1 !="50STILLINFIELD" & d1$yield1 !="STILLINFIELD34" & d1$yield1 !="STILLINFIELD40" & d1$yield1 !="12NOTYET",]
  
  # remove unknown crops
  d1 <- d1[d1$crop!="fallow" & d1$crop!="urubingo" & d1$crop!="feeding" & d1$crop!="bamboo, fodder"& d1$crop!="eucalyptus" & d1$crop!="kitchen" 
         & d1$crop!="no" & d1$crop!="fodder crop" & d1$crop!="pineaple",]
  ##############################################
  # Process production data and  land management
  
  d2 <- r2[, c("farm_id", "crop","total_production_farm","weight_unit")] 
  colnames(d2) <- c("trial_id", "crop","yield1","yield_unit")
  d22 <- r5[,c("farm_id","farm_size_ha")]
  colnames(d22) <- c("trial_id","farm_size")
  #merge d2 and d22
  d2 <- merge(d2,d22,by="trial_id")
  
  # merge d2 and d (location data)
  d2 <- merge(d,d2,by="trial_id")
  # remove word in yield value
  d2 <- d2[d2$yield1!="NOTYET" & d2$yield1!="LOSS" & d2$yield1!="DAMAGEDBYFLOOD" & d2$yield1!="STILLINFIELD" & d2$yield1!="STILLINTHEFIELD" & d2$yield1!="NOTYETHARVESTED", ]
  d2$inoculation_type <- NA
  d2$fertilizer_type <- NA
  d2$fertilizer_amount <- NA
  d2$variety <- NA
  d2$intercrops <- "no crop"
  ################################################################
  # process second production_use_2
  d3 <- r4[,c("farm_id","legume_area_now_ha","crop","yield_amount_now","yield_unit_now")]
  colnames(d3) <- c("trial_id","farm_size","crop","yield1","yield_unit")
  d3$inoculation_type <- NA
  d3$fertilizer_type <- NA
  d3$fertilizer_amount <- NA
  d3$variety <- NA
  d3$intercrops <- "no crop"
  # merge d3 and d
  d3 <- merge(d,d3,by="trial_id")
  
  # Append All the data we process 
  ################################################
  d <- rbind(d1,d2,d3) 
  # remove bad value in the yield
  d$yield1 <- as.numeric(d$yield1)
  d$yield <- ifelse((d$yield_unit=="kg"| d$yield_unit=="Kg" | d$yield_unit=="kgs" | d$yield_unit=="KG" | d$yield_unit==" kg"| d$yield_unit=="kgs"| d$yield_unit=="kgS" | d$yield_unit=="KGS") & d$farm_size >0,d$yield1/d$farm_size,d$yield1)
  d$inoculated <- FALSE
  d$inoculated[!is.na(d$inoculation_type)| d$inoculation_type !=""] <- TRUE
  d <- d[, c("country", "trial_id", "location","adm2","adm3","longitude", "latitude","crop","intercrops", "yield","fertilizer_type","inoculated")]
  
  
  # Add columns
  d$dataset_id <- dataset_id
  d$on_farm <- FALSE
  d$is_survey <- TRUE
  d$irrigated <- FALSE
  
  # DAP content: 18% of N  and 46% P205 
  # P apply is 30kg/ha in rwanda 
  #Urea was applied at a rate of 60 kg N/ha in Kenya and Rwanda trials
  # NPK apply 17-17-17
  # Fix fertilizer_type
  p <- carobiner::fix_name(d$fertilizer_type)
  p <- gsub("Urea","urea",p)
  p <- gsub("DAP         urea","urea; DAP",p)
  p <- gsub("urea       DAP","urea; DAP",p)
  p <- gsub("DAP       urea","urea; DAP",p)
  p <- gsub("DAP      urea","urea; DAP",p)
  p <- gsub("DAP        urea","urea; DAP",p)
  p <- gsub("NPK       DAP","NPK; DAP",p)
  p <- gsub("DAP         urea","urea; DAP",p)
  p <- gsub("DAP       urea" ,"urea; DAP",p)
  p <- gsub("NPK17.17.17","NPK",p)
  p <- gsub("NPK       NPK","NPK",p)
  p <- gsub("NPk","NPK",p)
  d$fertilizer_type <- p
  d$fertilizer_type[d$fertilizer_type=="DAP+manure"] <- "DAP"
  
  #add fertilizer
  d$N_fertilizer <- 0
  d$P_fertilizer <- 0
  d$K_fertilizer <- 0
  d$N_fertilizer[d$fertilizer_type=="NPK"] <- 17
  d$N_fertilizer[d$fertilizer_type=="urea"] <- 60*0.46
  d$N_fertilizer[d$fertilizer_type=="urea; DAP"] <- 60*0.46+ (30*0.18)/0.46
  d$N_fertilizer[d$fertilizer_type=="NPK; DAP"] <- 17+ (30*0.18)/0.46
  d$N_fertilizer[d$fertilizer_type=="DAP"] <- (30*0.18)/0.46
  d$P_fertilizer[d$fertilizer_type=="DAP"|d$fertilizer_type=="urea; DAP"] <- 30
  d$P_fertilizer[d$fertilizer_type=="NPK; DAP"] <- 17/2.29+30
  d$K_fertilizer[d$fertilizer_type=="NPK"] <- 17/1.2051
  d$K_fertilizer[d$fertilizer_type=="NPK" |d$fertilizer_type=="NPK; DAP"] <- 17/1.2051
  #fix country name
  dd <- carobiner::fix_name(d$country,"title")
  d$country <- dd
  d$country[is.na(d$country)] <- "Rwanda"
  # fix crop name 
  p <- carobiner::fix_name(d$crop,"lower")
  p <- gsub("soybeans","soybean",p)
  p <- gsub("Sweet potatoes","sweetpotato",p)
  p <- gsub("sweet potatoes","sweetpotato",p)
  p <- gsub("sweet potatoes","sweetpotato",p)
  p <- gsub("irish potatoes","potato",p)
  p <- gsub("potatoes","potato",p)
  p <- gsub("peas","pea",p)
  p <- gsub("climbing beans","common bean",p)
  p <- gsub("common beans","common bean",p)
  p <- gsub("beans","common bean",p)
  p <- gsub("tomatoes","tomato",p)
  p <- gsub("onions","onion",p)
  p <- gsub("cocoyam","taro",p)
  p <- gsub("amaranths","amaranth",p)
  p <- gsub("groundnuts","groundnut",p)
  p <- gsub("kitchen garden","vegetables",p)
  p <- gsub("cofffee","coffee",p)
  p <- gsub("red onions","onion",p)
  p <- gsub("red onion","onion",p)
  p <- gsub("Banana","banana",p)
  p <- gsub("peanuts","groundnut",p)
  p <- gsub("irish potaotes","potato",p)
  p <- gsub("pineaple"    ,"pineapple" ,p)
  d$crop <- p
  d$crop[is.na(d$crop)] <- "no crop"
  
  #fix crop yield limit with respect to crop
  d$yield[d$crop=="common bean" & d$yield>9000] <- NA
  d$yield[d$crop=="banana" & d$yield>173000] <- NA
  d$yield[d$crop=="cassava" & d$yield>90000] <- NA
  d$yield[d$crop=="maize" & d$yield>41500] <- NA
  d$yield[d$crop=="sorghum" & d$yield>18000] <- NA
  d$yield[d$crop=="wheat" & d$yield>19000] <- NA
  d$yield[d$crop=="groundnut" & d$yield>8500] <- NA
  d$yield[d$crop=="soybean" & d$yield>15000] <- NA
  # remove crop with very low yield value after divided by the plot area
  d <- d[d$crop!="bamboo"&d$crop!="fodder"& d$crop!="fodder crop" & d$crop!="no crop",]
  
  # fix whitespace in variable
  d$fertilizer_type[d$fertilizer_type==""] <- NA
  d$location[d$location==""] <- NA
  d$adm2[d$adm2==""] <- NA
  d$adm3[d$adm3==""] <- NA
  d$intercrops[d$intercrops==""] <- NA
  d$yield_part <- "seed"
  # all scripts must end like this
  carobiner::write_files(dset, d, path=path)
}
