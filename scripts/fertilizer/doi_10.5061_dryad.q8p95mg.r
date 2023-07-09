

"Title: Bean yield and economic response to fertilizer in eastern and southern Africa

Description: Bean (Phaseolus vulgaris L.) is important in sub-Saharan Africa for human dietary protein. Low yields are attributed to biotic and 
abiotic constraints including inadequate nutrient availability. Research was conducted to determine nutrient response functions for bean production
areas of Kenya, Mozambique, Rwanda, Tanzania, and Zambia.

"
carob_script <- function(path) {
  
  uri <- "doi.org/10.5061/dryad.q8p95mg"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  
  ## data set level data
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication= "https://doi.org/10.1007/s10705-018-9915-9",
    project="Optimizing Fertilizer Use in Africa",
    data_citation = "Kaizzi, K. C. et al. (2018), Data from: Bean yield and economic response to fertilizer in eastern and southern Africa, 
    Dryad, Dataset, https://doi.org/10.5061/dryad.q8p95mg",
    data_institutions = "University of Nebraska - Lincoln",
    carob_contributor="Rachel Mukami",
    data_type="on_farm & on_station")
  
  ## download and read data
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  dset$license <- carobiner::get_license(js)
  
  # reading the datasets
  f <- ff[basename(ff) == "ESA Bean Nutrient Response Dataset.xlsx"]
  d <- readxl::read_xlsx(f, sheet = 1) # kenya
  d1 <- readxl::read_xlsx(f, sheet = 2) # Mozambique
  d2 <- readxl::read_xlsx(f, sheet = 3) # Rwanda
  d3 <- readxl::read_xlsx(f, sheet = 4) # tanzania
  d4 <- readxl::read_xlsx(f, sheet = 5) # zambia

  # Publication Table 1 Site-year, FAO dominant soil type, growth habit (GH) of climbing (CL) and bush (BB) type, coordinates, mean site-year yield, and response to a diagnostic treatment (Diag) for bean nutrient response trials conducted in Rwanda during 2014–2015""
  d5 <- as.data.frame(extract_areas("C:/Users/User/Downloads/s10705-018-9915-9.pdf",pages = 3))
  names(d5) <- c("Site-year_soil type","GH","Lat (WGS84°)a","Long (WGS84°)","Elev (m)","Yield (Mg ha−1","Diag (Mg ha−1)c")
  
  d5$`Site-year_soil type`[1] <- gsub("Nyagat","Nyagat_",d5$`Site-year_soil type`[1])
  d5$`Site-year_soil type`[1] <- gsub("b","",d5$`Site-year_soil type`[1])
  d5$soil_type <- sapply(d5$`Site-year_soil type`, function(x) strsplit(x, "_")[[1]][4])
  d5$trial_id <- paste0(sapply(d5$`Site-year_soil type`, function(x) strsplit(x, "_")[[1]][1]),
                        "_",
                        sapply(d5$`Site-year_soil type`, function(x) strsplit(x, "_")[[1]][2]),"_",
                        sapply(d5$`Site-year_soil type`, function(x) strsplit(x, "_")[[1]][3]))
  d5 <- d5[,c("trial_id","Lat (WGS84°)a","Long (WGS84°)","Elev (m)","soil_type")]
  names(d5)[2] <- "latitude"
  names(d5)[3] <- "longitude"
  names(d5)[4] <- "elevation"

  # Publication Table 2 Mean trial yield, location, FAO dominant soil type, year, mean yield (Mg ha−1), latitude, longitude, elevation (m) and variety information for bean nutrient response trials conducted in Kenya, Tanzania, Zambia and Mozambique
  d6 <- as.data.frame(extract_areas("C:/Users/User/Downloads/s10705-018-9915-9.pdf",pages = 4))
  names(d6) <- c("Site_soil type","Year","Lat","Long","Elev","Variety","Yield")
  d6$`Site_soil type`<- gsub("-","_",d6$`Site_soil type`)
  d6$`Site_soil type`<- gsub(" ","",d6$`Site_soil type`)
  d6$soil_type <- sapply(d6$`Site_soil type`, function(x) strsplit(x, "_")[[1]][3])
  d6$trial_id <- paste0(sapply(d6$`Site_soil type`, function(x) strsplit(x, "_")[[1]][1]),"_",sapply(d6$`Site_soil type`, function(x) strsplit(x, "_")[[1]][2]))
  d6$trial_id[d6$trial_id == "TZ_Selian" & d6$Year == 2014] <- "TZ_Selian14"
  d6$trial_id[d6$trial_id == "TZ_Selian" & d6$Year == 2015] <- "TZ_Selian15"
  d6$trial_id[d6$trial_id == "TZ_Uyole" ] <- "TZ_Uyole15"
  d6 <- d6[,c("trial_id","Year","Lat","Long","Elev","Variety","soil_type")]
  names(d6) <- carobiner::replace_values(names(d6),
                                         c("Year","Lat","Long","Elev","Variety"),
                                         c("season","latitude","longitude","elevation","variety"))
  d6 <- unique(d6)
  
  # Publication Table 3 Soil test information of the 0 to 20 cm depth for bean-nutrient response trials conducted in eastern and southern Africa
  d7 <- as.data.frame(extract_areas("C:/Users/User/Downloads/s10705-018-9915-9.pdf",pages = 5))
  names(d7) <- c("Site-year","TC","pH","SOC(g kg-1)","P(mg kg-1)","K(mg kg-1)","Mg(mg kg-1)","S(mg kg-1)","Zn(mg kg-1)","B(mg kg-1)")
  d7$`Site-year`[d7$`Site-year` == "RW_ENyagatKat14B"] <- "RW_ENyagat_Kat14B"
  d7 <- d7[,c("Site-year","pH","SOC(g kg-1)" ,"P(mg kg-1)","K(mg kg-1)","Mg(mg kg-1)")]
  names(d7) <- carobiner::replace_values(names(d7),
                                         c("Site-year","pH","SOC(g kg-1)" ,"P(mg kg-1)","K(mg kg-1)","Mg(mg kg-1)"),
                                         c("trial_id","soil_pH","soil_SOC","soil_P_total","soil_K","soil_Mg"))
  
  # cleaning Kenya
  d <- readxl::read_xlsx(f, sheet = 1)
  d$country <- "Kenya"
  d$location <- carobiner::fix_name(d$S, "title")
  d$crop <- "common bean" # GLP2 bean variety from publication
  d$rep <- d$Rep
  d$treatment <- d$Trtcode
  d$treatment <- gsub("N","N,",d$treatment)
  d$treatment <- gsub("P","P,",d$treatment)
  d$treatment <- gsub("K ","K,",d$treatment)
  d$treatment <- gsub("Zn","Zn,",d$treatment)
  d$treatment <- gsub("Mg","Mg,",d$treatment)
  d$treatment <- gsub("BMo","B,Mo",d$treatment)
  d$treatment <- gsub("30KD","30K,D",d$treatment)
  d$treatment <- gsub("K15S","K,15S,",d$treatment)
  d$treatment <- gsub("S","S,",d$treatment)
  d$treatment <- gsub(",,",",",d$treatment)
  d$treatment <- gsub("Dia+Mo","diag+mo",d$treatment)
  d$treatment <- gsub("Dia-Mo","diag-mo",d$treatment)
  d$N_fertilizer <- d$Nrate
  d$P_fertilizer <- d$Prate
  d$K_fertilizer <- d$Krate
  d$yield <- d$GrainYld
  d$Yr[d$Yr == "2015b"] <- 2015
  d$trial_id <- paste0("KE_",d$S)
  d$trial_id[d$trial_id == "KE_Kisii"] <- "KE_Migori" 
  d <- d[,c("trial_id","country","location","crop","rep","treatment","N_fertilizer" ,"P_fertilizer", "K_fertilizer","yield")]
  vv <- merge(d,d6,by = "trial_id",all.x = TRUE)  
  d <- merge(vv,d7,by = "trial_id",all.x = TRUE) 
  
  
  # cleaning Mozambique
  d1 <- readxl::read_xlsx(f, sheet = 2)
  d1$country <- "Mozambique"
  d1$trial_id <- paste0("MZ_",d1$District)
  d1$adm2 <- d1$District
  d1$variety <- d1$v
  d1$yield <- d1$GrainYld
  d1$rep <- d1$Rep
  d1$treatment <- paste0(d1$n,"N",",",d1$p,"P",",",d1$k,"K",",","diag",d1$Diagnostic)
  d1$N_fertilizer <- d1$n
  d1$P_fertilizer <- d1$p
  d1$K_fertilizer <- d1$k
  d1$crop <- "common bean"
  d1 <- d1[,c("trial_id","country","adm2","crop","rep","variety","treatment","N_fertilizer" ,"P_fertilizer", "K_fertilizer","yield")]
  d1 <- merge(d1,d6[,c("trial_id","season","latitude","longitude","elevation","soil_type")],by = "trial_id",all.x = TRUE) 
  

  # cleaning Rwanda
  d2 <- readxl::read_xlsx(f, sheet = 3)
  d2$country <- "Rwanda"
  d2$Tr <- gsub("2014","14",d2$Tr)
  d2$Tr <- gsub("2015","15",d2$Tr)
  d2$variety <- ""
  d2$variety[d2$BT == "CL"] <- "MAC44"
  d2$variety[d2$BT == "BB"] <- "RWR2245"
  d2$crop <- "common bean"
  d2$year <- ifelse(grepl("14",d2$Tr),2014,2015)
  d2$Prov[d2$Tr %in% c("E_Ngoma_Sake_15A","E_Busegerwa_Mareba_15A")] <- "E"
  d2$adm1 <- ifelse(d2$Prov == "N","Northern (Amajyaruguru)",
                    ifelse(d2$Prov == "E","Eastern (Iburasirazuba)","Southern (Amajyepfo)"))
  d2$rep <- d2$Rep
  d2$treatment <- gsub(" ",",",d2$Treatment)
  d2$treatment <- gsub(",,",",",d2$treatment)
  d2$treatment <- carobiner::replace_values(d2$treatment,c("20N,15P,20K,15S,2.5,Zn,10,Mg,0.5B"),c("20N,15P,20K,15S,2.5Zn,10Mg,0.5B"))
  d2$N_fertilizer <- d2$N
  d2$P_fertilizer <- d2$P
  d2$K_fertilizer <- d2$K
  d2$yield <- d2$GrainYld
  d2$season <- d2$year
  d2$trial_id <- carobiner::replace_values(d2$Tr,
                                            c("E_Busegerwa_Mareba_14B","E_Busegerwa_Mareba_15A","E_Busegerwa_Musenyi_15A","E_Kirehe_Mushirkiri_15A",
                                             "E_Ngoma_Sake_14B","E_Ngoma_Sake_15A","E_Nyagatara_Katabagemu_15A","EBUGESEJustin15b","EBugeseMuseny15b",
                                             "EKIREHERukira15b","ENYAGATHAKIZI15b","ENyagatKataba14B","ENyagatRugazi14B","N_Burera_Rwerere_14B","N_Burera_Rwerere_15A",
                                             "NBURERARwerer15b","S_Huye_Mbazi_15A","S_Huye_Rubona_14B","S_Huye_Rusatira_15A","S_Nyanza_Rwabicuma_14B","SHuyestatio15b",
                                             "SHuyeTWAGIR15b","SNyanzaMUKAMA15b"),
                                           
                                           c("RW_EBusege_Mar14B","RW_EBusege_Mar15A","RW_EBusege_Mus15A","RW_EKirehe_Musi15A","RW_ENgoma_Sak14B",
                                             "RW_ENgoma_Sak15A","RW_ENyagat_Kat15A","RW_EBugese_Jus15b","RW_EBugese_Mus15b","RW_EKirehe_Ruk15b","RW_ENyaga_Tha15b",
                                             "RW_ENyagat_Kat14B","RW_ENyagat_Rug14B","RW_NBurera_Rwe14B","RW_NBurera_Rwe15A","RW_NBurera_Rwe15b","RW_SHuye_Mba15A","RW_SHuye_Rub14B",
                                             "RW_SHuye_Rus15A","RW_SNyanza_Rwa14B","RW_SHuye_Sta15b","RW_SHuye_Twa15b","RW_SNyanza_Muk15b"))
  # subseting important variables
  d2 <- d2[,c("trial_id","country","adm1","crop","variety","rep","season","treatment","N_fertilizer","P_fertilizer","K_fertilizer",
              "yield")] 
  vv <- merge(d2,d5,by = "trial_id",all.x = TRUE)  
  d2 <- merge(vv,d7,by = "trial_id",all.x = TRUE) 

  
  # cleaning Tanzania
  d3 <- readxl::read_xlsx(f, sheet = 4) 
  d3$country <- "Tanzania"
  d3$S[d3$S %in% c("Kar","Kar2")] <- "Karangai"
  d3$location <- d3$S  
  d3$rep <- d3$Rep
  d3$trial_id <- paste0("TZ_",d3$location)
  d3$trial_id[d3$trial_id == "TZ_Selian" & d3$Yr == 2014] <- "TZ_Selian14"
  d3$trial_id[d3$trial_id == "TZ_Selian" & d3$Yr == 2015] <- "TZ_Selian15"
  d3$trial_id[d3$trial_id == "TZ_Uyole" ] <- "TZ_Uyole15"
  d3$Diagnostic[is.na(d3$Diagnostic)] <- 0
  d3$treatment <- paste0(d3$n,"N",",",d3$p,"P",",",d3$k,"K",",","diag",d3$Diagnostic)
  d3$N_fertilizer <- d3$n
  d3$P_fertilizer <- d3$p
  d3$K_fertilizer <- d3$k
  d3$crop <- "common bean"
  d3$yield <- d3$GrainYld
  d3 <- d3[,c("trial_id","country","location","crop","rep","treatment","N_fertilizer" ,"P_fertilizer", "K_fertilizer","yield")]
  vv <- merge(d3,d6,by = "trial_id",all.x = TRUE)
  d3 <- merge(vv,d7,by = "trial_id",all.x = TRUE)
  
  
  # cleaning Zambia
  d4 <- readxl::read_xlsx(f, sheet = 5)
  d4$country <- "Zambia"
  d4$crop <- "common bean"
  d4$S[d4$S == "Mt Makulu"] <- "Mt.Makulu"
  d4$location <- d4$S
  d4$trial_id <- paste0("ZM_",d4$location)
  d4$rep <- d4$R
  d4$Diagnostic[is.na(d4$Diagnostic)] <- 0
  d4$treatment <- paste0(d4$N,"N",",",d4$P,"P",",",d4$K,"K",",","diag",d4$Diagnostic)
  d4$yield <- d4$GrainYld
  d4$season <- d4$Yr
  d4$N_fertilizer <- d4$N
  d4$P_fertilizer <- d4$P
  d4$K_fertilizer <- d4$K
  d4 <- d4[,c("trial_id","country","location","crop","rep","season","treatment","N_fertilizer" ,"P_fertilizer", "K_fertilizer","yield")]
  vv <- merge(d4,d6,by = c("trial_id","season"),all.x = TRUE)
  d4 <- merge(vv,d7,by = "trial_id",all.x = TRUE)
  
  #merging all countries datasets
  z <- Reduce(function(...) merge(..., all=T), list(d,d1,d2,d3,d4))
  z$dataset_id <- dataset_id
  z$inoculated <- FALSE
  z$yield_part <- "grain"
  z$irrigated <- FALSE
  z$variety_type <- ifelse(z$variety %in% c("RWR2245","GLP2"),"bush bean",
                           ifelse(z$variety == "MAC44","climbing bean",NA))
  z$yield <- z$yield*1000 # converting to kg/ha from Mg/ha
  z$yield[z$yield > 8000] <- NA # making NA yield above 8000
  z$latitude <- gsub("[^-0-9.]", "", z$latitude)
  z$latitude <- as.numeric(z$latitude)
  z$longitude <- as.numeric(z$longitude)
  z$elevation <- as.numeric(z$elevation)
  z$rep <- as.integer(z$rep)
  z$soil_pH <- as.numeric(z$soil_pH)
  z$soil_SOC <- as.numeric(z$soil_SOC)
  z$soil_SOC[z$soil_SOC > 20] <- NA
  z$soil_P_total <- as.numeric(z$soil_P_total)
  z$soil_K <- as.numeric(z$soil_K)
  z$soil_Mg <- as.numeric(z$soil_Mg)
  
  z$latitude <- ifelse(z$trial_id == "E_Busegerwa_Mushikiri_14B",-2.18893865,
                       ifelse(z$trial_id == "ENGOMAUFITUB15b",-2.1663637,
                              ifelse(z$trial_id == "E_Busegerwa_Musenyi_14B",	-2.17889725,z$latitude)))
  
  z$longitude <- ifelse (z$trial_id == "E_Busegerwa_Mushikiri_14B",30.68517191848185,
                         ifelse(z$trial_id == "ENGOMAUFITUB15b",30.5391524,
                                ifelse(z$trial_id == "E_Busegerwa_Musenyi_14B",30.02089217993953,z$longitude)))
 
  z <- z[,c("dataset_id","trial_id","country","adm1","adm2","location","latitude", "longitude", "elevation","rep", "treatment","crop", "variety",
            "variety_type","inoculated","irrigated","yield","yield_part","N_fertilizer","P_fertilizer","K_fertilizer","soil_type","soil_pH",
            "soil_SOC","soil_P_total","soil_K","soil_Mg")]
  
  # all scripts must end like this
  carobiner::write_files(dset, z, path=path)
}
