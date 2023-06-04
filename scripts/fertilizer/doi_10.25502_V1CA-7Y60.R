# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
N2Africa is to contribute to increasing biological nitrogen 
fixation and productivity of grain legumes among African smallholder 
farmers which will contribute to enhancing soil fertility, improving
household nutrition and increasing income levels of smallholder farmers.
As a vision of success, N2Africa will build sustainable, long-term partnership
s to enable African smallholder farmers to benefit from symbiotic N2-fixation by
grain legumes through effective production technologies including 
inoculants and fertilizers adapted to local settings.
   
"
  
  uri <- "doi.org/10.25502/V1CA-7Y60"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication=NA,
    data_citation ="Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, P., Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira, L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller, K., Ampadu-Boakye, T., & Heerwaarden, J. van. (2020). N2Africa diagnostic trial, 2012 [Data set]. International
    Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/V1CA-7Y60",
    data_institutions = "IITA",
    carob_contributor="Cedric Ngakou",
    experiment_type="experiment",
    has_weather=FALSE,
    has_management=TRUE
  )
  
  ## download and read data 
  ff <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  f <- ff[basename(ff) == "data_table.csv"] 
  
  # read the dataset
  r <- read.csv(f)
  
 d <- r[,c("country","id","lga_district_woreda","sector_ward","gps_homestead_device_latitude.decimal_degrees","gps_latitude_field.decimal_degrees","gps_homestead_device_longitude.decimal_degrees",
         "gps_field_device_longitude.decimal_degrees","date_of_planting_whole_n2africa_field.date","date_of_final_harvest_whole_n2a_field.date","legume_planted_in_the_n2africa_trial"
         ,"crop_1_previous_season","inoculation_n2africa_field")] 
 
 colnames(d) <- c("country","trial_id","location","site","latitude1","latitude2","longitude1","longitude2","start_date","end_date","crop","previous_crop","inoculated")
  
 # Fix long and lat columns 
 for (i in 1:length(d$latitude1)){
   if ( is.na(d$latitude1[i]) & !is.na(d$latitude2[i])){
     d$latitude1[i]<-d$latitude2[i]
   }
   if ( is.na(d$longitude1[i]) & !is.na(d$longitude2[i])){
     d$longitude1[i]<-d$longitude2[i]
   }
 }
 # keep the relevant column of long and lat
 d$latitude<-d$latitude1
 d$longitude<-d$longitude1
 
 
  #extract relevant columns for treatment1
   d1 <- r[,c("id","lga_district_woreda","country","treatment1","row_spacing_crop_1_plot_1.cm" ,"plant_spacing_crop_1_plot_1.cm","grain_weight_crop_1_plot_1.kg",
              "pod_weight_groundnut_crop_1_plot_1.kg","above_ground_biomass_weight_crop_1_plot_1.kg","width_of_harvested_plot_crop_1_plot_1.m",
              "no_plants_hole_crop_1_plot_1.nr","number_of_rows_crop_1_plot_1.nr")] 
  colnames(d1) <- c("trial_id","location","country","treatment","row_spacing","plant_spacing","yield1","yield2","residue_yield","width_size_plot","number_plant","number_row")
  
  #extract relevant columns for treatment2
  d2 <- r[, c("id","lga_district_woreda","country","treatment2","row_spacing_crop_1_plot_2.cm" ,"plant_spacing_crop_1_plot_2.cm","grain_weight_crop_1_plot_2.kg","pod_weight_groundnut_crop_1_plot_2.kg",
              "above_ground_biomass_weight_crop_1_plot_2.kg","width_of_harvested_plot_crop_1_plot_2.m",
              "no_plants_hole_crop_1_plot_2.nr","number_of_rows_crop_1_plot_2.nr")] 
  colnames(d2) <- c("trial_id","location","country","treatment","row_spacing","plant_spacing","yield1","yield2","residue_yield","width_size_plot","number_plant","number_row")
 
   #extract relevant columns for treatment3
  d3 <- r[, c("id","lga_district_woreda","country","treatment3","row_spacing_crop_1_plot_3.cm" ,"plant_spacing_crop_1_plot_3.cm","grain_weight_crop_1_plot_3.kg","pod_weight_groundnut_crop_1_plot_3.kg",
              "above_ground_biomass_weight_crop_1_plot_3.kg","width_of_harvested_plot_crop_1_plot_3.m",
              "no_plants_hole_crop_1_plot_3.nr","number_of_rows_crop_1_plot_.nr")] 
  colnames(d3) <- c("trial_id","location","country","treatment","row_spacing","plant_spacing","yield1","yield2","residue_yield","width_size_plot","number_plant","number_row")
  
  #extract relevant columns for treatment4
  d4 <- r[, c("id","lga_district_woreda","country","treatment4","row_spacing_crop_1_plot_4.cm" ,"plant_spacing_crop_1_plot_4.cm","grain_weight_crop_1_plot_4.kg","pod_weight_groundnut_crop_1_plot_4.kg",
              "above_ground_biomass_weight_crop_1_plot_4.kg","width_of_harvested_plot_crop_1_plot_4.m",
              "no_plants_hole_crop_1_plot_4.nr","number_of_rows_crop_1_plot_4.nr")] 
  colnames(d4) <- c("trial_id","location","country","treatment","row_spacing","plant_spacing","yield1","yield2","residue_yield","width_size_plot","number_plant","number_row")
  
  #extract relevant columns for treatment5
  d5 <- r[, c("id","lga_district_woreda","country","treatment5","row_spacing_crop_1_plot_5.cm" ,"plant_spacing_crop_1_plot_5.cm","grain_weight_crop_1_plot_5.kg","pod_weight_groundnut_crop_1_plot_5.kg",
              "above_ground_biomass_weight_crop_1_plot_5.kg","width_of_harvested_plot_crop_1_plot_5.m",
              "no_plants_hole_crop_1_plot_5.nr","number_of_rows_crop_1_plot_5.nr")] 
  colnames(d5) <- c("trial_id","location","country","treatment","row_spacing","plant_spacing","yield1","yield2","residue_yield","width_size_plot","number_plant","number_row")
  
  #extract relevant columns for treatment6 ## from d6 to d12 few rows contain data (treatment, yield) 
  d6 <- r[,c("id","lga_district_woreda","country","treatment6","row_spacing_crop_1_plot_6.cm" ,"plant_spacing_crop_1_plot_6.cm","grain_weight_crop_1_plot_6.kg","pod_weight_groundnut_crop_1_plot_6.kg",
             "above_ground_biomass_weight_crop_1_plot_6.kg","width_of_harvested_plot_crop_1_plot_6.m",
             "no_plants_hole_crop_1_plot_6.nr","number_of_rows_crop_1_plot_6.nr")] 
 colnames(d6) <- c("trial_id","location","country","treatment","row_spacing","plant_spacing","yield1","yield2","residue_yield","width_size_plot","number_plant","number_row")
  #remove rows with no treatment 
   d6<-d6[complete.cases(d6$treatment), ] 
   
   #extract relevant columns for treatment7
  d7 <- r[,c("id","lga_district_woreda","country","treatment7","row_spacing_crop_1_plot_7.cm" ,"plant_spacing_crop_1_plot_7.cm","grain_weight_crop_1_plot_7.kg","pod_weight_groundnut_crop_1_plot_7.kg",
             "above_ground_biomass_weight_crop_1_plot_7.kg","width_of_harvested_plot_crop_1_plot_7.m",
             "no_plants_hole_crop_1_plot_7.nr","number_of_rows_crop_1_plot_7.nr")] 
  colnames(d7) <- c("trial_id","location","country","treatment","row_spacing","plant_spacing","yield1","yield2","residue_yield","width_size_plot","number_plant","number_row")
  #remove rows with no treatment
  d7<-d7[complete.cases(d7$treatment), ] # remove all the rows where there is no treatment
  
  #extract relevant columns for treatment8
  d8 <- r[,c("id","lga_district_woreda","country","treatment8","row_spacing_crop_1_plot_8.cm" ,"plant_spacing_crop_1_plot_8.cm","grain_weight_crop_1_plot_8.kg","pod_weight_groundnut_crop_1_plot_8.kg",
             "above_ground_biomass_weight_crop_1_plot_8.kg","width_of_harvested_plot_crop_1_plot_8.m",
             "no_plants_hole_crop_1_plot_8.nr","number_of_rows_crop_1_plot_8.nr")] 
  colnames(d8) <- c("trial_id","location","country","treatment","row_spacing","plant_spacing","yield1","yield2","residue_yield","width_size_plot","number_plant","number_row")
  #remove rows with no treatment ##
  d8<-d8[complete.cases(d8$treatment), ] 
  
  #extract relevant columns for treatment9
  d9 <- r[,c("id","lga_district_woreda","country","treatment9","row_spacing_crop_1_plot_9.cm" ,"plant_spacing_crop_1_plot_9.cm","grain_weight_crop_1_plot_9.kg","pod_weight_groundnut_crop_1_plot_9.kg",
             "above_ground_biomass_weight_crop_1_plot_9.kg","width_of_harvested_plot_crop_1_plot_9.m",
             "no_plants_hole_crop_1_plot_9.nr","number_of_rows_crop_1_plot_9.nr")] 
  colnames(d9) <- c("trial_id","location","country","treatment","row_spacing","plant_spacing","yield1","yield2","residue_yield","width_size_plot","number_plant","number_row")
  #remove rows with no treatment
  d9<-d9[complete.cases(d9$treatment), ] 
  
  #extract relevant columns for treatment10
  d10 <- r[,c("id","lga_district_woreda","country","treatment10","row_spacing_crop_1_plot_10.cm" ,"plant_spacing_crop_1_plot_10.cm","grain_weight_crop_1_plot_10.kg","pod_weight_groundnut_crop_1_plot_10.kg",
              "above_ground_biomass_weight_crop_1_plot_10.kg","width_of_harvested_plot_crop_1_plot_10.m",
              "no_plants_hole_crop_1_plot_10.nr","number_of_rows_crop_1_plot_10.nr")] 
  colnames(d10) <-c("trial_id","location","country","treatment","row_spacing","plant_spacing","yield1","yield2","residue_yield","width_size_plot","number_plant","number_row")
  #remove rows with no treatment
  d10<-d10[complete.cases(d10$treatment), ] 
  
  #extract relevant columns for treatment11
  d11 <- r[,c("id","lga_district_woreda","country","treatment11","row_spacing_crop_1_plot_11.cm" ,"plant_spacing_crop_1_plot_11.cm","grain_weight_crop_1_plot_11.kg","pod_weight_groundnut_crop_1_plot_11.kg",
           "above_ground_biomass_weight_crop_1_plot_11.kg","width_of_harvested_plot_crop_1_plot_11.m",
           "no_plants_hole_crop_1_plot_11.nr","number_of_rows_crop_1_plot_11.nr")] 
  colnames(d11) <- c("trial_id","location","country","treatment","row_spacing","plant_spacing","yield1","yield2","residue_yield","width_size_plot","number_plant","number_row")
  #remove rows with no treatment
  d11<-d11[complete.cases(d11$treatment), ] 
  
  #extract relevant columns for treatment12
  d12 <- r[,c("id","lga_district_woreda","country","treatment12","row_spacing_crop_1_plot_12.cm" ,"plant_spacing_crop_1_plot_12.cm","grain_weight_crop_1_plot_12.kg","pod_weight_groundnut_crop_1_plot_12.kg",
           "above_ground_biomass_weight_crop_1_plot_12.kg","width_of_harvested_plot_crop_1_plot_12.m",
           "no_plants_hole_crop_1_plot_12.nr","number_of_rows_crop_1_plot_12.nr")] 
  colnames(d12) <-c("trial_id","location","country","treatment","row_spacing","plant_spacing","yield1","yield2","residue_yield","width_size_plot","number_plant","number_row")
  #remove rows with no treatment
  d12<-d12[complete.cases(d12$treatment), ] 
  # append all the treatment data
  dd<-carobiner::bindr(d1,d2,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12)
  
  
  
  # fix  yield column 
  for (i in 1:length(dd$yield1)){
    if ( is.na(dd$yield1[i]) & !is.na(dd$yield2[i])){
      dd$yield1[i]<-dd$yield2[i]
      
    }
  }
  
# calculate the yield value using area of different plot 
  dd$area<-((dd$width_size_plot*(dd$number_row-1)*dd$row_spacing)/100)/10000 # in ha
  dd$yield<-dd$yield1/dd$area
  dd$residue_yield<-dd$residue_yield/dd$area
  #plant density
  dd$plant_density<-(((dd$width_size_plot/dd$plant_spacing)+1)*dd$number_row)/dd$area # number of plan per ha
  
  
   # variety column
  variety1<- c("SAMNUT 24","TGX1951-3F","FTGX1955-4F","SAMNUT 22","TGX 1951-3F","Maksoy 3N","Lyamungu 90",
              "Nabe 12C","Local","Tumaini","TGX 1955-4F","Pendo","Songotra","Jenguma","Samnut23","Jenguma","TGX1955-4F")
  dd$variety<-NA
 for(i in 1:length(variety1)){
   for (j in 1:length(dd$treatment)){
     if(grepl(variety1[i],dd$treatment[j])==TRUE){
       dd$variety[j]<-variety1[i]
     }
   }
 }

# merge d and dd
d <-merge(d,dd,by=c("country","location","trial_id"))

d<-d[,c("country","trial_id","location","site","longitude","latitude","start_date"
        ,"end_date","crop","previous_crop","variety","inoculated","treatment","row_spacing","plant_spacing","yield","residue_yield","plant_density")]

# Add columns
  d$dataset_id <- dataset_id   
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  
  # Add fertilizer
  
  d$N_fertilizer <-0   # 
  d$K_fertilizer<- ifelse(grepl("K",d$treatment)==TRUE,20,0)
  d$P_fertilizer<- ifelse((grepl("P",d$treatment)==TRUE) & (d$treatment!="Pendo, FYM"|d$treatment!="Pendo, Minjingu" |d$treatment!="Pendo, Minjingu + Gypsum + FYM"|
                                                              d$treatment!="Pendo, Minjingu + FYM"|d$treatment!="Pendo, Control"|d$treatment!="Pendo, Gypsum"  ),30,0)
  d$Zn_fertilizer<- ifelse((grepl("Zn",d$treatment)==TRUE),2.5,0)
  
  d$S_fertilizer<-ifelse(d$treatment=="Maksoy 3N, Lime+P+K+S+Ca+Mg+Zn+Mo+Legume fix"|
                           d$treatment=="Maksoy 3N, Lime+P+K+S+Ca+Mg+Zn+Mo+Legume fix+Manure"|d$treatment=="Tumaini, + NPK+ MgS" |
                           d$treatment=="PK+CaMgS+Micronutrients" | d$treatment=="P+CaMgS+Micronutrients"|d$treatment=="PK+CaMgS"|
                           d$treatment=="PK+CaMgS+Micronutrients+I"|d$treatment=="Jenguma, P+ Ca + K + Mg + S + Zn + B"|d$treatment=="Jenguma, P+ Ca + K + Mg + S + Zn + B + Inoculant"|
                           d$treatment=="Jenguma, P+ K + Mg + S"|d$treatment=="Jenguma, P+ K + Mg + S + Zn + B"|d$treatment=="Jenguma, P+ Mg + S + Zn + B" |    
                           d$treatment=="Samnut23, P+ K + Mg + S + Zn + B"|d$treatment=="Samnut23, P+ K + Mg + S"|d$treatment=="Samnut23, P+ Ca + K + Mg + S + Zn + B"|
                           d$treatment=="Samnut23, P+ Mg + S + Zn + B" |d$treatment=="Samnut23, P+ Ca + K + Mg + S + Zn + B + Inoculant" | 
                           d$treatment=="Songotra, P+ K + Mg + S"|d$treatment=="Songotra, P+ Ca + K + Mg + S + Zn + B"|d$treatment=="Songotra, P+ Ca + K + Mg + S + Zn + B + Inoculant"|
                           d$treatment=="Songotra, P+ Mg + S + Zn + B" | d$treatment=="Songotra, P+ K + Mg + S + Zn + B",12,0)

 
 d$OM_used<- ifelse((grepl("OM",d$treatment)==TRUE)|(grepl("Manure",d$treatment)==TRUE),TRUE,FALSE)
 #fix country name 
  P<-carobiner::fix_name(d$country,"title")  
  d$country<-P
  # fix location
  P1<-carobiner::fix_name(d$location)
  P1<-gsub("tolon","Tolon",P1)
  d$location<-P1
# fix inoculated  column
  e<-carobiner::fix_name(d$inoculated)  
  e<-gsub("y",TRUE,e)
  e<-gsub("n",FALSE,e)
  d$inoculated<-e 
  # longitude and latitude fixe
  d$latitude[d$location=="Tolon"]  <- 9.432468
  d$longitude[d$location=="Tolon"]  <-  -1.0665974
  d$latitude[d$location=="karaga"]  <- 9.926780   
  d$longitude[d$location=="karaga"]  <- -0.4316607
  d$latitude[d$location=="wa-west"]  <- 9.898577
  d$longitude[d$location=="wa-west"]  <- -2.6832633
  d$latitude[d$location=="Ajingi"]  <- 11.967800
  d$longitude[d$location=="Ajingi"]  <- 9.0366500
  d$latitude[d$location=="Bugiri"]  <- 0.5637032
  d$longitude[d$location=="Bugiri"]  <-  33.76176
  d$latitude[d$location=="BIU"]  <- 10.614610
  d$longitude[d$location=="BIU"]  <- 12.1909612
  d$latitude[d$location=="kassena nankana"]  <- 10.958900
  d$longitude[d$location=="kassena nankana"]  <- -1.1132999
  d$latitude[d$location=="bawku municipal"]  <- 11.0500
  d$longitude[d$location=="bawku municipal"]  <- -0.2333
  d$latitude[d$location=="nadowli"]  <- 10.372021
  d$longitude[d$location=="nadowli"]  <- -2.6641890
  d$latitude[d$location=="Bagwai"]  <-  12.157700
  d$longitude[d$location=="Bagwai"]  <- 8.1357000
  d$latitude[d$location=="BAYO"]  <-  10.4524
  d$longitude[d$location=="BAYO"]  <- 11.6827
  d$latitude[d$location=="Chikun"]  <-  10.242006
  d$longitude[d$location=="Chikun"]  <- 7.0706742
  d$latitude[d$location=="Gwarzo"]  <-  12.358697
  d$longitude[d$location=="Gwarzo"]  <- 8.829500
  d$latitude[d$location=="HAWUL"]  <-  10.5
  d$longitude[d$location=="HAWUL"]  <- 12.2
  d$latitude[d$location=="Igabi"]  <- 10.831510
  d$longitude[d$location=="Igabi"]  <- 7.776152
  d$latitude[d$location=="Kajuru"]  <- 10.331663
  d$longitude[d$location=="Kajuru"]  <- 7.680287
  d$latitude[d$location=="KWAYA KUSAR"]  <- 10.562206
  d$longitude[d$location=="KWAYA KUSAR"]  <- 11.848337
  d$latitude[d$location=="Lapai"]  <- 9.066667
  d$longitude[d$location=="Lapai"]  <- 6.716667
  d$latitude[d$location=="Paikoro"]  <- 9.4930391
  d$longitude[d$location=="Paikoro"]  <- 6.837686
  d$latitude[d$location=="Shiroro"]  <- 9.956072
  d$longitude[d$location=="Shiroro"]  <- 6.833260
  d$latitude[d$location=="Gairo"]  <- -6.140271
  d$longitude[d$location=="Gairo"]  <- 36.869070
  d$latitude[d$location=="Kilosa"]  <- -10.835097
  d$longitude[d$location=="Kilosa"]  <- 38.648945
  d$latitude[d$location=="Kongwa"]  <- -7.253275
  d$longitude[d$location=="Kongwa"]  <-37.910680
  d$latitude[d$location=="Mvomero"]  <- -6.3042153
  d$longitude[d$location=="Mvomero"]  <-37.44778
  d$latitude[d$location=="Bukedea"]  <- 1.3440293
  d$longitude[d$location=="Bukedea"]  <-34.04511
  d$latitude[d$location=="Kapchorwa"]  <- 1.3967784
  d$longitude[d$location=="Kapchorwa"]  <-34.45081
  d$latitude[d$location=="Kibuku"]  <- 1.0434750
  d$longitude[d$location=="Kibuku"]  <33.79363
  d$latitude[d$location=="Kisoro"]  <- -1.2822138
  d$longitude[d$location=="Kisoro"]  <-29.69267
  d$latitude[d$location=="Kole"]  <-  2.4283032
  d$longitude[d$location=="Kole"]  <-32.80096
  d$latitude[d$location=="Kumi"]  <- 1.4951440
  d$longitude[d$location=="Kumi"]  <-33.96053
  d$latitude[d$location=="Pallisa"]  <- 1.0778522
  d$longitude[d$location=="Pallisa"]  <-34.16643
  d$latitude[d$location=="Shanono"]  <-12.048429
  d$longitude[d$location=="Shanono"]  <-7.988561
  
  
  
  # fix crops names
  b<-carobiner::fix_name(d$crop)  
  b<-gsub("soya_bean","soybean",b)
  b<-gsub("bush_bean","common bean",b)
  b<-gsub("climbing_bean","common bean",b)
  d$crop<-b 
  # fix previous crops names
  b<-carobiner::fix_name(d$previous_crop)  
  b<-gsub("millet","pearl millet",b)
  b<-gsub("fallow","no crop",b)
  b<-gsub("soyabean","soybean",b)
  b<-gsub("other","no crop",b)
  b<-gsub("sweet_potato","sweetpotato",b)
  b<-gsub("bambara_bean","common bean",b)
  b<-gsub("bush_bean","common bean",b)
  b<-gsub("irish_potato","potato",b)
  b<-gsub("pigeon_pea","pigeon pea",b)
  b<-gsub("climbing_bean","common bean",b)
  b<-gsub("green_gram","mung bean",b)
  d$previous_crop<-b 
 ###RH ? b2<-carobiner::fix_name(d$start_date) 
 ## b2 <- gsub("2025-07-08",,b2)
 ## d$start_date<-b2
## d$start_date[d$start_date == "2025-07-08"] <- "2015-07-08"
# change the date format 
##RH ?  d$start_date<-format(dmy(d$start_date),'%d/%m/%Y')
##RH ?  d$end_date<-format(dmy(d$end_date),'%d/%m/%Y')

## this works in English locales (that is, not on e.g. a French computer)
##  d$start_date <- as.Date(d$start_date, '%d-%b-%y')
##  d$end_date <- as.Date(d$end_date, '%d-%b-%y')

##  therefore
	for (i in 1:12) {
		d$start_date <- gsub(month.abb[i], i, d$start_date)
		d$end_date <- gsub(month.abb[i], i, d$end_date)
	}
  d$start_date <- as.character(as.Date(d$start_date, '%d-%m-%y'))
  d$end_date <- as.character(as.Date(d$end_date, '%d-%m-%y'))
	
  #fix start date column
##??  b2<-carobiner::fix_name(d$start_date) 
##  b2<-gsub("08/07/2025","08/07/2016",b2) # end date is 2016-11-05 and the crop is soybean
  d$start_date[d$start_date=="2025-07-08"] <- "2016-07-08"
  #data type
 ## d$start_date <- format(as.Date(d$start_date, format = '%d/%m/%Y'), "%Y-%m-%d")
##  d$end_date <- format(as.Date(d$end_date, format = '%d/%m/%Y'), "%Y-%m-%d")
  d$yield<- (as.numeric(d$yield)) 
  d$inoculated<-as.logical (d$inoculated)
  
  carobiner::write_files(dset, d, path, dataset_id, group)
  
}
  


