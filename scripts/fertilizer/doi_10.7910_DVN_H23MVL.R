# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
Data sets included in the database evaluated Site-Specific Nutrient Management (SSNM) and Farmers’ 
Fertilizer Practice (FFP) on maize, rice and wheat in Africa and Asia. Information captured includes,
with countries, cropping systems, crop type, seasons, climatic conditions, number of replications, 
number of nitrogen (N) splits, N, phosphorus (P) and potassium (K) fertilizer rates, grain yield, agronomic efficiency of N (AEN), partial factor of productivity N (PFP N), total fertilizer cost (TFC), gross return, and gross return above fertilizer cost (GRF).
(2020-10-27)
   
"
  
  uri <- "doi:10.7910/DVN/H23MVL"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication=NA,
    data_citation ="Dobermann A, Witt C, Dawe D, et al (2002) Site-specific nutrient management for intensive rice cropping systems in Asia. F Crop Res 74:37–66. 
    doi: doi: 10.1016/S0378- 4290(01)00197-6" ,
    data_institutions = "IRRI",
    carob_contributor="Cedric Ngakou",
    experiment_type="NA",
    has_weather=FALSE
     
  )
  
  ## download and read data 
  ff <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  lic <- carobiner::get_license(js)
  dset$license <- lic$name
  
  f <- ff[basename(ff) =="SSNM_Meta-analysis_data.csv"] 
  
  # read the dataset
  r <- read.csv(f,header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
   colnames(r) <- r[1,]
  d <- r[,c(4,7,8,9,10,14,15,17,28,29,31,32,34,35,38,41,43,44,49,53,57,62,64)]
  d <- d[-1,] # drop the first rows
  #normalize columns names
  colnames(d) <-  c("reference","country","location","longitude","latitude","soil_type","soil_pH","soil_SOC","previous_crop","crop",
                  "water_mangement","variety","observation_date","season","tillage","treatment","N_splits","N_fertilizer",
                  "P_fertilizer","K_fertilizer","Zn_fertilizer","rep","yield")
  
  d[c('planting_date', 'harvest_date')] <- stringr::str_split_fixed(d$observation_date, "-", 2) 
  
  d[c('latitude1', 'Latitude2')] <- stringr::str_split_fixed(d$latitude, "\u0096", 2)
  d[c('longitude1', 'Longitude2')] <- stringr::str_split_fixed(d$longitude, "\u0096", 2)
  
  d[c('soil_pH', 'soil_pH1')] <- stringr::str_split_fixed(d$soil_pH, "-", 2)
  
  d[c('soil_SOC', 'soil_SOC1')] <- stringr::str_split_fixed(d$soil_SOC, "-", 2)
 
  d$k <- d$harvest_date
  d$harvest_date <- ifelse((d$planting_date=="2006 and 2007")& is.na(d$harvest_date),"2007",
                     ifelse((d$planting_date=="2005 and 2006")& is.na(d$harvest_date),"2006",d$k))
  #fix start date 
  p <- carobiner::fix_name(d$planting_date)
  p <- gsub("2006 and 2007","2006",p)
  p <- gsub("2005 and 2006","2005",p)
  d$planting_date <- p
  
  d[d==""] <- NA
  rownames(d) <- NULL
  lat <- trimws(d$longitude1)
  lon <- trimws(d$latitude1)
  # Fix long and lat columns
#RH?  d <- d[c(361,362),]
  
  lon <- gsub("\n46\u0092", "46'", lon)
  lon <- gsub("\002", "", lon)
  lon <- gsub("75°02' E to 77°04' E", "76°03'E", lon)
  lon <- gsub(" ", "", lon)
  lon <- gsub("°","-", lon)
  lon <- gsub("'","-", lon)
  lon <- gsub("\\?","-", lon)
  lon <- stringr::str_split_fixed(lon, "-", 3)
  lon[lon[,2]=="", 2] <- NA
  d$longitude = as.numeric(lon[,1]) + as.numeric(lon[,2])/60
  d$longitude <- ifelse(lon[,3] == "W", -d$longitude, d$longitude)
  
  lat <- gsub("29°07' N to 30°08' N", "29°33' N", lat)
  lat <- gsub("\u0092", "'",lat)
  lat <- gsub(" ", "", lat)
  lat <- gsub("°","-", lat)
  lat <- gsub("'N","", lat)
  lat <- gsub("-N","", lat)
  lat <- gsub("'","", lat)
  lat <- gsub("\\?N", "", lat)
  lat <- stringr::str_split_fixed(lat, "-", 2)
  lat[lat[,2]=="", 2] <- NA
  d$latitude = as.numeric(lat[,1]) + as.numeric(lat[,2])/60
  
  # extract relevant columns 
  d <- d[c("reference","country","location","longitude","latitude","crop","previous_crop","variety","yield","tillage","N_fertilizer", "P_fertilizer","K_fertilizer","N_splits","planting_date","harvest_date","season","soil_pH",'soil_type',"soil_SOC")]
  # Add columns
  d$dataset_id <- dataset_id
  d$trial_id <- paste0(d$dataset_id,"_",d$country)
  d$on_farm <- FALSE
  d$is_survey <- TRUE
  d$irrigated <- FALSE
  # fill missing data in long and lat columns

  yx <- c("latitude", "longitude")
  d$location <- gsub("\n", "",  d$location)
  locs=data.frame(location=c("Anhui (AH)", "Guangdong", "Guangdong (GD)","Guangdong, Hunan, Jiangsu, Hubei", "Hebei","Hubei (HB)","Hubei, An'hui"  ,"Heilongjiang","Heilongjiang (HLJ)","Heilongjiang and Jilin provinces","Heilongjiang, Jilin","Henan","Hunan (HN)","Hunan, Guangdong, Jiangxi","Jiangsu","Jiangxi (JX)", "Jiaxing", "Jilin", "Jilin (JL)","Jilin Province Northeast China","Jinhua-Quzhou Basin in Central Zhejiang","Jinhua, Jin-Qu, Zheijang","Liaoning","Shandong","Shanxi","Tianmushan in Jinhua", "Xushan Village, Xiaonan County, Hubei Province","Zhejiang","East Java, Lampung, North Sumatra, South Sulawesi","Grobogan, Central Java (GRO)","Jeneponto, South Sulawesi (SUL)","Kediri, East Java (EJA)","Lampung Tengah, Lampung (LAM)","Sukamandi, West Java","Tanah Karo, North Sumatra (SUM)","An Giang, Mekong River Delta","Binh Tay, Mekong Delta","Binh Thanh, Mekong Delta","Can Tho, Mekong River Delta", "Cau Ke, Tra Vinh (TRA)","Chau Phu An Giang, Mekong River Delta","CuM\u0092gar, Dak Lak (DAK)","Dinh Mon, Mekong Delta","Go Cong Tay Tien Giang, Mekong River Delta","Hanoi, Red River Delta","Mai Son, Son La (SON)","Omon Cantho, Mekong River Delta","Omon, Mekong Delta","Tan Chau, An Giang (ANG)","Tien Giang, Mekong River Delta","Trang Bang, Tay Ninh (TAY)","Vinh Phuc & Bac Giang (DEG)","Aduthurai, Cauvery Delta, Tamil Nadu","Andhra Pradesh","Bihar","Bihar, Haryana, Karnataka, Punjab, Uttar Pradesh","Control villages in Nadia, West Bengal, India","East and south eastern coastal plain - Kendrapara and Puri, Odisha","Gurdaspur, Hoshiarpur, Ludhiana, Patiala, Faridkot, and Firozpur in Punjab province in Northwest India","Haryana","Haryana, Punjab","Intervention villages in Nadia, West Bengal, India","Karnal, Kurukshetra, Kaithal, Ambala,Yamunanagar, Panipat, and Sonepat districts of Haryana","Karnataka","Kumbakonam, Tamil Nadu","Mewat, Haryana", "Mid central table land - Dhenkanal, Odisha","Needamangalam, Tamil Nadu","Norman E. Borlaug Crop Research Centre of G.B. Pant University of Agriculture and Technology, Pantnagar","North central plateau - Keonjhar and Mayurbhanj, Odisha", "North eastern coastal plain - Balasore, Bhadrak and Jajpur, Odisha","North western plateau - Sundergarh","Odisha","Old and New Cauvery Delta in Thanjavur District","Orathanadu, Tamil Nadu","Papanasam, Tamil Nadu","Pattukkottai, Tamil Nadu","Peravurani, Tamil Nadu","Punjab","Site 1 in Siruguppa, Bijapur, and Navalgund Talukas of Northern Karnataka","Tamil Nadu","Thanjavur, Cauvery Delta, Tamil Nadu","Thiruvaiyaru, Tamil Nadu", "Thiruvonam, Tamil Nadu","Uttar Pradesh","Uttarakhand","West Bengal","Western central table land - Sambalpur","Damak and Gauradaha in Jhapa district","Itahara and Babiyabirta municipalities in Morang district of eastern Nepal","Lamjung Campus, Sundarbazar",
                             "Ilagan, Isabela (ISA)","Ilocos Norte/Sur, La Union, Pangasinan, Isabela, Zambales, Nueva Ecija, Bulacan","IRRI farm", "La Union, Pangasinan, Zambales, Bulacan","Malaybalay, Bukidnon (BUK)", "Maligaya, Nueva Ecija, Central Luzon","Munoz, Nueva Ecija, Central Luzon", "Nueva Ecija and Tarlac","Pangasinan and Tarlac","Pangasinan, Laguna, Occidental Mindoro, Iloilo, Negros Occidental, Davao, Maguindanao","Sto. Domingo, Nueva Ecija, Central Luzon","Gazipur and Comilla","Kpong","Senegal River delta or middle valley"),
  lon = c(117.22598792254, 113.323981014309, 113.323981014309, 113.323981014309, 111.709068502231, 112.270811333976, 112.270811333976, 127.644739354739, 127.644739354739,127.644739354739, 127.644739354739, 113.614270214227, 111.709068502231,111.709068502231, 119.809186818008, 115.722201905693, 120.88630578928,126.188723466564, 126.188723466564, 126.188723466564, 119.952125008289,119.952125008289, 122.487973523011, 118.716882930163, 112.289404842681,119.471307959445, 102.4791136, 120.663639832217,105.217560865301, 110.926992847186, 119.698520703012, 112.015298102972, 105.217560865301, 107.6221628, 98.4415782639879,105.182730450551, 107.077222990545, 105.576330285048, 105.530584124911, 106.061027933662, 108.2350816, 108.207813115105, 105.6127321, 106.65630875472, 105.700138403108, 104.016966704743, 105.61860110614, 105.61860110614, 105.186588199416, 106.368976632459, 106.337505896007, 105.559435839349,79.4582129581811, 79.9629347608505, 85.6096530884735, 85.6096530884735, 88.5151529615671, 86.6098179220416, 75.316667, 76.3397783078727, 76.20433155, 88.3213226972205, 76.8967847999834, 76.1675088049985, 79.3753928, 77.0495327273226, 85.5236710344528, 79.4855116780348, 79.4832094, 86.4084283090387, 86.9216712, 84.0361994, 84.4260680647707, 79.0503877505183, 79.2479397337764, 79.2544398136345, 79.3352006248279, 79.2047333548905, 75.4150448817847, 76.8939231, 78.4082902563882, 79.0503877505183, 79.0138402949767, 76.9283294672715, 80.565691754046, 79.2041888298691, 88.3213226972205, 84.2736287984146,87.6766971992305, 87.6345270995562, 84.4162053106763,121.8886466, 120.167329943126,121.262409667144, 121.0196870556, 125.018477329639, 120.5989972,124.883553324674, 121.060233210937, 120.167329943126,121.349961030971, 121.013981991963,89.15950415,0.0604009,-16.307478),
  lat =c(31.8260330242362,22.9130088457212, 22.9130088457212, 22.9130088457212, 27.6097064823689, 30.9759429420565, 30.9759429420565, 47.9848753536303, 47.9848753536303,47.9848753536303, 47.9848753536303, 33.8818369781319, 27.6097064823689,27.6097064823689, 33.0853237069793, 27.6141059651448, 30.6017419606547,43.6669219648424, 43.6669219648424, 43.6669219648424, 29.1182678313338,29.1182678313338, 40.9893001284629, 36.4825374362838, 37.5717120159242,30.2738332951321, 35.8469301, 29.2188276954035,-5.12321957977055, -7.11694573359114,-5.56744880108105, -7.82595156697472, -5.12321957977055,-6.7177474, 2.99055741734865,10.5112834166682, 10.3490014055568, 9.69984867068789, 10.1144787119785, 9.86973583201513, 16.0596052, 12.8243108048099, 10.0545853, 10.405640819521, 20.9990590746447, 21.1681638418353, 10.1087062598748, 10.1087062598748, 10.8009449415928, 10.3800922024027, 11.0794248401687, 21.3617764296056,11.0846965460328, 15.7537528805482, 25.6791629465305, 25.6791629465305, 23.4748815750091, 20.5528181902205, 32.166667, 29.1978160646455, 29.9590202, 22.5157063494245, 29.6884569166608, 14.7101597922064, 10.9572769, 27.9853672900587, 20.8266491886217, 10.7246797680025, 29.028405, 21.8954209878196, 21.5017098, 22.1181799, 20.5118159915169, 10.7510522970275, 10.5880077599998, 10.8525431212409, 10.4073892301972, 10.2546724970107, 30.8425284956194, 15.6336064, 11.0146507652449, 10.7510522970275, 10.8583894338455, 8.57689108945191, 26.9232377078024, 30.156923468869, 22.5157063494245, 21.4536858268416,26.6728616206984,26.6602649392239, 28.1259850746592,17.1486341,16.081357024461, 14.1516228503584, 14.9557932257214,8.01718083631428, 15.4399703, 11.7805951956756, 14.3550702015521,16.081357024461, 14.2465388957761, 15.6206712491258,22.4388633,6.1573585,13.984199))

  d <- merge(d, locs, by="location", all.x=TRUE)
	i <- is.na(d$longitude)
	d$longitude[i] <- d$lon[i]
	i <- is.na(d$latitude)
	d$latitude[i] <- d$lat[i]
    d$lat <- d$lon <- NULL
    # fix soil_SOC range and fertilizer
    d$soil_SOC[d$soil_SOC>20]<-NA
    d$N_fertilizer[is.na(d$N_fertilizer)]<-0
    d$P_fertilizer[is.na(d$P_fertilizer)]<-0
    d$K_fertilizer[is.na(d$K_fertilizer)]<-0
    # remove rows without location
    d<-d[!is.na(d$location),]
   # fix error in long and lat 
    d$longitude[d$location=="Alukdia"]<-88.82096
    d$latitude[d$location=="Alukdia"] <-23.6578
    d$longitude[d$location=="Badarganj"]<-89.1808277
    d$latitude[d$location=="Badarganj"]<-25.7071259
    d$longitude[d$location=="Chandbill"]<-90.2934413
    d$latitude[d$location=="Chandbill"]<-24.4769288
    d$longitude[d$location=="Jhapa "]<-87.885701
    d$latitude[d$location=="Jhapa "]<-26.5837354
    d$longitude[d$location=="Grobogan, Central Java (GRO)"]<-110.8966767
    d$latitude[d$location=="Grobogan, Central Java (GRO)"]<--7.0980947
    d$longitude[d$location=="Ilocos Norte/Sur, La Union, Pangasinan, Isabela, Zambales, Nueva Ecija, Bulacan"]<-121
    d$latitude[d$location=="Ilocos Norte/Sur, La Union, Pangasinan, Isabela, Zambales, Nueva Ecija, Bulacan"]<-15.583333
    d$longitude[d$location=="Jeneponto, South Sulawesi (SUL)"]<-120.2948856
    d$latitude[d$location=="Jeneponto, South Sulawesi (SUL)"]<--1.9758004
    d$longitude[d$location=="Kediri, East Java (EJA)"]<-112.0046051
    d$latitude[d$location=="Kediri, East Java (EJA)"]<--7.8111057
    d$longitude[d$location=="Lampung Tengah, Lampung (LAM)"]<-105.0272986
    d$latitude[d$location=="Lampung Tengah, Lampung (LAM)"]<--4.8555039
    d$longitude[d$location=="Liaoning" ]<-122.9955469
    d$latitude[d$location== "Liaoning"]<-40.9975197
    d$longitude[d$location=="Pangasinan and Tarlac" ]<-120.4964091
    d$latitude[d$location=="Pangasinan and Tarlac"]<-15.4937252
    d$longitude[d$location=="Sukamandi, West Java"  ]<-107.6221628
    d$latitude[d$location=="Sukamandi, West Java" ]<--6.7177474
    d$longitude[d$location=="Trang Bang, Tay Ninh (TAY)"]<-106.3623675
    d$latitude[d$location=="Trang Bang, Tay Ninh (TAY)"]<-11.0315517
    # fix crop name 
  p <- carobiner::fix_name(d$crop, "lower")
  d$crop <- p
  p1 <- carobiner::fix_name(d$previous_crop,"lower")
  p1 <- gsub("upland crop","no crop",p1) # no specification about upland crop
  p1 <- gsub("mungbean","mung bean",p1)
  d$previous_crop <- p1
  #fix white space in soil_SOC column
  e <- carobiner::fix_name(d$soil_type)
  e <- gsub(" ",NA,e)
  d$soil_type <- e
  #data type
  d$yield <-  (as.numeric(d$yield)) 
  d$longitude <- as.numeric(d$longitude)
  d$latitude <- as.numeric(d$latitude)
  d$N_fertilizer <- as.double(d$N_fertilizer)
  d$P_fertilizer <- as.double(d$P_fertilizer)
  d$K_fertilizer <- as.double(d$K_fertilizer)
  d$N_splits <- as.integer(d$N_splits)
  d$soil_pH <- as.double(d$soil_pH)
  d$soil_SOC <- as.double(d$soil_SOC)
  carobiner::write_files(dset, d, path=path)
  
}

                 



