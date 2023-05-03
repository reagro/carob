
carob_script <- function(path) {
  
  "
	Information is scarce for maize (Zea mays L.) response to nutrient application for many production areas in tropical Africa. 
	Research was conducted to determine macronutrient response functions and to diagnose Mg–S–Zn–B deficiencies. 
	Site–year × N-rate interactions within countries often accounted for little variation in yield relative to the N-rate effect. 
	Country mean grain yield responses to N-rate were curvilinear to plateau, but linear in Malawi. Although mean yields differed, 
	the response to N was similar for Kenya, Tanzania, and Zambia with a mean yield increase of 0.94 Mg ha–1 due to 50 kg ha–1 N compared
	with 1.59 Mg ha–1 for Malawi and Rwanda. Response to N was related to yield with no fertilizer applied (r = 0.40). 
	Only Rwanda had mean responses to P and K with respective yield increases of 0.99 and 0.22 Mg ha–1 due to 15 kg ha–1.
	Application of Mg–S–Zn–B caused a mean yield increase of 0.73 Mg ha–1 in Rwanda but had no effect in other countries. 
	Application of affordable fertilizer to twice as much land at 50% compared with 100% of the economically optimum rate results 
	in mean gains of 50% for production and agronomic efficiency and 72% for profit/cost ratio. Soil test results were not related
	to response to applied nutrients but historical yield appears to be weakly predictive of N response. The determined country-level 
	P and K response functions can be widely applied, except for Kenya, in consideration of other available information. 
	The response to Mg–S–Zn–B in Rwanda needs further investigation.
	
Wortmann, C., C. Senkoro, A.R. Cyamweshi, C. Kibunja, D. Nkonde, M. Munthali, P. Nalivata, L.N. Nabahungu, K. Kaizzi. 2018. Maize-nutrient response functions for Eastern and Southern Africa. Agron. J. 110:2070-2079. doi:10.2134/agronj2018.04.0268

Also see: doi.org/10.21955/gatesopenres.1115299.1

"
  
  uri <- "doi.org/10.5061/dryad.fg15tg2"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project = "Optimization of Fertilizer Recommendations in Africa",
    uri=uri,
    publication= "https://doi.org/10.2134/agronj2018.04.0268",
    data_institutions = "University of Nebraska-Lincoln",
    carob_contributor="Effie Ochieng and Rachel Mukami",
    experiment_type="fertilizer",
    has_weather= TRUE,
    has_soil=TRUE,
    has_management=FALSE
  )
  
  ## download and read data 
  ff  <- list.files(dirname(carobiner::get_data(uri, path, group)), full.names = TRUE)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- js$license

  f <-  ff[basename(ff) == "ESA Maize Fertilizer Response Data.xlsx"][1]
  d <- readxl::read_xlsx(f, sheet = 2)
  d1 <- readxl::read_xlsx(f, sheet = 3)
  d2 <- readxl::read_xlsx(f, sheet = 4)
  d3 <- readxl::read_xlsx(f, sheet = 5)
  d4 <- readxl::read_xlsx(f, sheet = 6)
  
  
  d$country <- "Kenya"
  d$adm3 <- as.character(strsplit(d$SY1, split = "[0-9]+"))
  v <- carobiner::fix_name(d$adm3)
  v <- carobiner::replace_values(v,
                                 c("Embu ATC", "EmbuKPS", "c(\"Kandara\", \"SR\")"),
                                 c("Embu","Embu","Kandara"))
  d$adm3 <- v
  d$crop <- "maize"
  d$rep <- d$R
  d$treatment <- d$Trt
  d$N_fertilizer <- d$N
  d$P_fertilizer <- d$P
  d$K_fertilizer <- d$K
  d$crop <- "maize"
  d$variety <- d$Variety
  d$yield <- d$GrainYld*1000 #converting to kg/ha
  d$trial_id <- paste("KE",d$SY1, sep = "_")
  
  #lat and lon as found in the reference
  LL <- data.frame( adm3 = c("Eldoret","Embu","Kandara","Kisii","Kitale","Machakos","Njoro"),
                    latitude = c(0.569,-0.494,-0.465,-0.682,1.000,-1.539,-0.344),
                    longitude = c(35.308,37.452,37.719,34.766,34.992,37.252,35.946),
                    elevation = c(2150,1500,1125,1450,1863,1585,2170),
                    soil_pH = c(5.0,6.2,5.6,6.0,5.4,6.2,5.7),
                    soil_P_total = c(22.9,15.6,64.9,144.1,15.2,14.6,7.6),
                    soil_K = c(569,261,281,421,239,314,768)
  )
  
  
  d <- merge(d,LL, by = "adm3", all.x = TRUE)
  
  
  # dates extracted from the reference
  dd <- data.frame(SY1= c("Eldoret14","Embu ATC15","EmbuKPS15","Kandara14","Kandara15SR","Kisii14","Kitale15","Machakos","Njoro14"),
                   start_date = c("2014-06-19","2015-03-31","2015-03-31","2014-03-27","2015-10-02","2014-04-03","2015-04-28","2015-04-08","2014-04-23"),
                   end_date = c("2014","2015-07-10","2015-07-10","2014-07-15","2016-01-27","2014", "2015","2015-07-12","2014-11-14"))
  
  d <- merge(d,dd, by = "SY1", all.x = TRUE)
  
  d <- d[,c("trial_id","country","adm3","latitude","longitude","crop","variety","treatment","rep","N_fertilizer","P_fertilizer","K_fertilizer","start_date","end_date","elevation",
            "soil_pH","soil_P_total","soil_K","yield")]
  
  d1$country <-"Malawi"
  d1$adm1 <- ""
  d1$adm1[grep("BDF", d1$L)] <- "Bunda"
  d1$adm1[grep("SL", d1$L)]  <- "Salima"
  d1$crop <- "maize"
  d1$rep <- d1$r
  d1$N_fertilizer <- d1$N
  d1$P_fertilizer <- d1$P
  d1$K_fertilizer <- d1$K
  d1$grain_weight <- (d1$`100-kernel wt`)*10 # to get 1000 grain weight
  d1$yield <- d1$GrainYld*1000 # to kg/ha
  d1$latitude <- ifelse(d1$adm1 == "Bunda",-14.170,-14.054)
  d1$longitude <- ifelse(d1$adm1 == "Bunda",33.740,33.740)
  d1$elevation <- ifelse(d1$adm1 == "Bunda",1150,1240)
  d1$trial_id <- paste("MW",d1$adm1,d1$L, sep = "_")
  #Dates extracted from reference
  dd <- data.frame(L = c("BDF15","BDFS15", "SLS15"), 
                   start_date = c("2015-01-05","2015-01-05","2015-01-25"),
                   end_date = c("2015-04-25","2015-04-25","2015-05-12"))
  
  d1 <- merge(d1,dd, by = "L", all.x = TRUE)
  dr <- data.frame(adm1 = c("Bunda","Salima"),
                   soil_pH = c(5.6,6.4),
                   soil_P_total = c(13.3,10.7),
                   soil_K = c(113,156))
  d1 <- merge(d1,dr,by = "adm1",all.x = TRUE)
  d1 <- d1[,c("trial_id","country","adm1","latitude","longitude","crop","rep","start_date","end_date","N_fertilizer","P_fertilizer","K_fertilizer","grain_weight","elevation",
              "soil_pH","soil_P_total","soil_K","yield")]
  
  d2<- readxl::read_xlsx(f, sheet = 4)
  
  
  d2$country <- "Rwanda"
  v <- d2$District
  v <- carobiner::fix_name(v, "title")
  d2$adm2 <- v
  d2$crop <- "maize"
  v <- d2$Variety
  v <- carobiner::replace_values(v,
                                 c("Hybrid(SC13)","ZM607","Hybrid(SC513)"),
                                 c("Hybrid SC13","ZM 607","Hybrid SC513"))
  d2$variety <- v
  d2$rep <- d2$Rep
  d2$treatment <- d2$Treatment
  d2$N_fertilizer <- d2$N
  d2$P_fertilizer <- d2$P
  d2$K_fertilizer <- d2$K
  d2$K_fertilizer[d2$K_fertilizer == "D"] <- 0
  d2$K_fertilizer <- as.numeric(d2$K_fertilizer)
  d2$trial_id <- paste("RW",d2$adm2,d2$Season, sep = "_")
  
  # the dates are extrated from the reference,with a and b differentiating seasons within years.
  d2$dd <- paste(d2$Season,d2$adm2)
  
  dd1 <- data.frame(dd =c("2014B Ngoma","2014B Nyagatare","2015 B Ngoma","2015 B Nyagatare","2015 B Musanze","2015A Bugesera", "2015A Ngoma","2015A Nyagatare","2015A Burera","2015A Musanze","2015A Huye"),
                    start_date =c("2014-03-09","2013-11-03","2015-02-27","2015-02-24","2015-02-10","2014-03-10","2014-09-26","2014-09-22","2014-09-18","2014-10-09","2015-03-04"),
                    end_date   =c("2014-07-29","2014_07-24","2015-07-16","2015-07-21","2015-08-11","2015-02-06","2015-02-10","2015-02-03","2015-02-26","2015_03-02","2015-07-23"))
  
  
  d2 <- merge(d2,dd1, by = "dd", all.x = TRUE)
  
  d2$yield <- d2$GrainYld*1000
  
  #lat and lon found in the reference
  
  LL <- data.frame(adm2 = c("Ngoma","Nyagatare","Musanze","Bugesera","Burera","Huye"),
                   latitude = c("-2.227","-1.516","-1.513","-2.239","-1.491","-2.539"),
                   longitude  = c("30.417","30.284","29.597","30.067","29.877","29.741"),
                   elevation = c(1458,1500,1930,1369,2096,1630),
                   soil_P_total = c(6.4,9.5,28.6,11.1,NA,6.4),
                   soil_K = c(114,114,225,119,NA,114)
  )
  
  
  d2 <- merge(d2,LL, by = "adm2", all.x = TRUE)
  
  d2 <- d2[, c("trial_id","country","adm2","latitude","longitude","crop","start_date","end_date",
               "variety","rep","treatment","N_fertilizer","P_fertilizer","K_fertilizer","elevation",
               "soil_P_total","soil_K","yield")]
  
  d3$country <- "Tanzania"
  v <- d3$S
  v <- carobiner::replace_values(v, c("Mav","Daredo") ,
                                 c("Mavamizi","Dareda"))
  d3$adm3 <- v
  d3$rep <- d3$R
  v <- d3$Variety
  v <- carobiner::replace_values(v, c("Seed-Co: 403","TMV-1", "DK8031"),
                                 c("Seed_Co 403","TMV 1","DK 8031"))
  d3$variety <- v
  d3$crop <-"maize"
  d3$N_fertilizer <- d3$N
  d3$N_fertilizer[d3$N_fertilizer %in% c("Diagnostic package","Diagnostic",NA)] <- 0 
  d3$N_fertilizer <- as.numeric(d3$N_fertilizer)
  d3$P_fertilizer <- d3$P
  d3$P_fertilizer[d3$P_fertilizer %in% c("Diagnostic",NA)] <- 0 
  d3$P_fertilizer[d3$P_fertilizer == "22.5     0"] <- "22.5"
  d3$P_fertilizer <- as.numeric(d3$P_fertilizer)
  d3$K_fertilizer <- d3$K
  d3 <- d3[d3$K_fertilizer %in% c(0,10,20,30),] 
  d3$yield <- d3$GrainYld*1000
  d3$trial_id <- paste("TZ",d3$adm3,d3$Y, sep = "_")
  
  #extracted lat and long info from the reference
  LL <- data.frame(adm3 = c("Ilonga","Mavamizi","Mlingano","Selian","Muheza","Dareda","Kwedizinga","Uyole"),
                   latitude = c("-6.783","-5.138","-5.138","-3.366","-5.166","-5.418","-5.418","-8.918"),
                   longitude  = c( "37.037","38.852","38.852","36.632","38.783","35.524","38.516","33.517"),
                   elevation = c(490,190,80,1410,NA,1635,350,1783),
                   soil_pH = c(6.7,6.6,5.9,7.4,NA,7.3,6.9,7.7),
                   soil_P_total = c(14.0,7.8,7.4,37.9,NA,7.3,12.7,11),
                   soil_K = c(316,402,339,2133,NA,343,277,1108))
  
  d3 <- merge(d3,LL, by = "adm3", all.x = TRUE) 
  
  #dates from the reference
  d3$dd <- paste(d3$adm3,d3$Y)
  
  dd <- data.frame(dd = c("Dareda 15","Dareda 16","Ilonga 14","Ilonga 15","Kwedizinga 16","Kwedizinga 15","Mavamizi 14","Mlingano 14","Mlingano 15","Muheza 15","Selian 14","Selian 15","Uyole 15","Uyole 16"),
                   start_date = c("2015-05-01","2016-01-01","2014-02-21","2015-03-04","2016-04-04","2015-03-21","2014-03-17","2014-03-10","2015-03-24","2015-03-19","2014-03-20","2015-04-03","2014-12-12","2015-12-24"),
                   end_date   = c("2015-07-15","2016-07-28","2014-07-10","2015-07-14","2016-07-26","2015-07-20","2014-07-18","2014-07-18","2015-08-13","2015-09-14","2014-09-14","2015-09-11","2015-08-11","2016-07-05"))
  
  d3 <- merge(d3,dd, by = "dd", all.x = TRUE)
  d3[is.na(d3)] <- 0
  d3 <- d3[,c("trial_id","country","adm3","latitude","longitude","crop","variety","rep","start_date","end_date","N_fertilizer","P_fertilizer","K_fertilizer",
              "elevation","soil_pH","soil_P_total","soil_K","yield")]
  
  d4$country <- "Zambia"
  v <- d4$S
  v <- carobiner::replace_values(v, c("Masekera","MtMukulu","Chomo","Choma Mochipapa","Chitapa" ),
                                 c("Msekera","Mt. Makulu","Choma","Choma","Msekera"))
  d4$site <- v
  d4$trial_id <- paste("ZM",d4$site,d4$Y, sep = "_")
  d4$crop <- "maize"
  v <- d4$Variety
  v <- carobiner::replace_values(v, c("ZM606","ZM521"),
                                 c("ZM 606","ZM 521"))
  d4$variety <- v
  d4$rep <- d4$R
  d4$N_fertilizer <- d4$N
  d4$P_fertilizer <- d4$P
  d4$K_fertilizer <- d4$K
  d4$yield <- d4$GrainYld*1000 # converting to kg/ha
  
  # lat and lon from the reference 
  LL <- data.frame(site = c("Mt. Makulu","Msekera","Choma","Kasama"),
                   latitude = c("-15.548","-13.640","-16.829","-10.171"),
                   longitude = c("28.251","32.555","27.063","31.226"),
                   elevation = c(1220,1224,1290,1230),
                   soil_pH = c(7.3,6.2,5.7,5.1),
                   soil_P_total = c(28.1,11.9,26.2,37.7),
                   soil_K = c(295,204,72,79)
  )
  
  d4 <- merge(d4,LL, by = "site", all.x = TRUE)
  # dates from the reference
  dd <- data.frame(trial_id = c("ZM_Choma_2014","ZM_Choma_2015","ZM_Kasama_2014","ZM_Msekera_2014","ZM_Msekera_2015","ZM_Mt. Makulu_2013","ZM_Mt. Makulu_2014","ZM_Mt. Makulu_2015"),
                   start_date = c("2014-12-08","2015-12-13","2014-12-06","2014-12-13","2015-12-18","2013-12-12","2014-12-12","2015-12-01"),
                   end_date = c("2015-05-18","2016-05-17","2015-05-14","2015-05-22","2016-05-22","2014-05-05","2015-05-05","2016-05-06"))
  
  d4 <- merge(d4,dd, by = "trial_id", all.x = TRUE)
  
  d4 <- d4[, c("trial_id","country","site","latitude","longitude","crop","variety","rep","start_date","end_date","elevation",
               "soil_pH","soil_P_total","soil_K","N_fertilizer","P_fertilizer","K_fertilizer","yield")]
  
  dd1 <- merge(d,d1, all = TRUE)
  dd2 <- merge(dd1,d2, all = TRUE)
  dd3 <- merge(dd2,d3, all = TRUE)
  z <- merge(dd3,d4, all = TRUE)
  
  z$dataset_id <- dataset_id
  z$on_farm <- TRUE
  z$is_survey <- FALSE
  z$irrigated <- FALSE
  z$row_spacing <- 75
  z$plant_spacing <- 25
  z$latitude <- as.numeric(z$latitude)
  z$longitude <- as.numeric(z$longitude)
  z$rep <- as.integer(z$rep)
  z$end_date <- ifelse(grepl("-", z$end_date),
                       as.character(as.Date(z$end_date, format = c("%Y-%m-%d"))),
                       NA)
  z$soil_K <- z$soil_K/1000 #to g/kg
  z$soil_P_total <- z$soil_P_total/1000 #to g/kg
  
  
  # all scripts must end like this
  carobiner::write_files(dset, z, path, dataset_id, group)
}

