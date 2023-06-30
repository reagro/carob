
carob_script <- function(path){
  "
 Title: Non-responsiveness of crops to fertilizers under some soils in sub-Saharan Africa
 
Low productivity of agriculture observed in different parts of sub-Saharan Africa is threatening food security 
in the region. Decades of production with mostly application of small amounts of inorganic fertilizers 
(mostly macronutrients) and scarce organic resources in the context of integrated soil fertility management (ISFM) 
result in nutrient mining of secondary and micronutrients in majority of smallholder farms. With the last decade, 
crop non-responsiveness to nutrient application has become an important issue requiring scientific understanding. 
We provide data focused on identifying the extent of non-responsiveness of crops to nutrient application and the 
associated factors. Data contains crop yield response to secondary and micronutrient (SMN), manure and lime application 
relative to yields of only NP/K application. (2020-02-25)
  
  "
  
  # registering the dataset
  uri <- "doi:10.7910/DVN/GXUNAZ"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  
  # The metadata at the dataset level
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    publication= "",
    data_citation = "Kihara, Job; Okeyo, Jeremiah; Bolo, Peter; Kinyua, Michael, 2020, Non-responsiveness of crops to fertilizers under some soils in sub-Saharan Africa, doi:10.7910/DVN/GXUNAZ",
    data_institutions = "CIAT",
    carob_contributor="Rachel Mukami",
    experiment_type="Crop yield response to secondary and micronutrient (SMN), manure and lime application relative to yields of only NP/K application",
    has_weather=FALSE
     
  )
  
  ## downloading data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=2)
  dset$license <- carobiner::get_license(js)
  
  # reading the data.csv data
  f <- ff[basename(ff) == "Non responsiveness of crop to fertiliser dat V2.xlsx"]
  d <- carobiner::read.excel(f)
  #d <- as.data.frame(d)
  d$variety <- carobiner::fix_name(d$Var_Type,"title")
  d$site <- carobiner::fix_name(d$Site,"title")
  d$`Site ID` <- carobiner::fix_name(d$`Site ID`,"title")

  # swapping back coordinates that are wrongly swapped
  d$latitude <- ""
  d$longitude <- ""
  swapped_coords <- c("Affem", "Balaka", "Bembeke", "Bouake", "Cana", "Chitedze", "Dabou", "Ibadan",
                      "Gewane College of Agriculture of the Afar Region","Gezira Research Station Farm","Kanyama","Ikenne",
                      "Ikoyi","Ilora","Kade","Kishi","Kpong","Lilongwe","Lusaka","Mlomba","Mpangala","Mzuzu","Nai Farm","Nampula",
                      "Niaouli","Nyankpala","Ogbomosho","Oyo","Tigray","Warda","Tsangano","Tigray","Sussundenga","Strong",
                      "Cox","Davidson","Hulme","Kiminini","Leys","Sessaro","Sourou Valley","Sepeteri","Salima","Russell","Sabwani",	
                      "Hwedza","Dendenyore","Imla")
  d$latitude <- ifelse(d$site %in% swapped_coords, d$Lon, d$Lat)
  d$longitude <- ifelse(d$site %in% swapped_coords, d$Lat, d$Lon)
  
  # converting lon lat that are in Degree-Minute-Direction format into numeric inputs
  d$latt <- as.numeric(sapply(strsplit(sapply(strsplit(d$Lat, "N "), `[`, 1), " "),`[`,1)) + 
    (as.numeric(sapply(strsplit(sapply(strsplit(d$Lat, "N "), `[`, 1), " "),`[`,2)))/60
  
  d$lonn <- as.numeric(sapply(strsplit(sapply(strsplit(d$Lat, "N "), `[`, 2), " "),`[`,1)) + 
    (as.numeric(sapply(strsplit(sapply(strsplit(d$Lat, "N "), `[`, 2), " "),`[`,2)))/60
  
  # creating latitude and longitude inputs
  condition <- d$Dataset == "Kihara_et al. 2017_Micronutrients dataset" & d$`Site ID` == "Kayode 1984"
  d$latitude <- as.numeric(ifelse(condition,d$latt,d$latitude))
  d$longitude <- ifelse(condition,d$lonn,d$longitude)
  
  d$latitude[d$site == "Calabar"] <- 	4.9795999
  d$longitude[d$site == "Calabar"] <- 8.3373597
  d$longitude[d$site == "Affem"] <- 1.5
  d$latitude[d$site == "Affem"] <- 9.15
  d$longitude[d$`NPK+S+Ca+Mg+Zn+B` == 4214.1] <- 3.867
  d$latitude[d$site == "Manjawira"] <- -14.9994263
  d$longitude[d$site == "Manjawira"] <- 34.8533877
  d$longitude[d$site == "Sessaro"] <- 1.16700
  d$latitude[d$site == "Sessaro"] <- 8.6329999999999991
  d$longitude[d$site == "Samaru"] <- 11.183
  d$latitude[d$site == "Samaru"] <- 7.63300

  d$country <- carobiner::fix_name(d$COUNTRY,"title")
  
  # country sites based on publication and coordinates
  NGA <- c("Bakori","Bunkure","Dandume","Doguwa","Faskari","Funtua","Giwa","Ikara","Kauru","Lere","Makarfi","Soba","T/wada","Tofa",
  "Tudun Wada","Bauchi","Calabar","Ibadan","Ikenne","Ikole","Ikoyi","Ilora","Iwo","Kishi","Mokwa","Ogbomosho","Oyo",
  "Yola","Sepeteri","Samaru") # nigeria 
  ZMB <- c("Lusaka") # Zambia
  CMR <- c("Minna") # Cameroon
  MOZ <- c("Nampula","Sussundenga") #"Mozambique"
  BEN <- c("Niaouli","Cana","Warda") # "Benin"
  KEN <- c("Kand", "Sidi","Nai Farm","Strong","Cox","Davidson","Hulme","Kiminini","Leys","Russell","Sabwani") # "Kenya"
  MWI <- c("Kasu","Nkha" ,"Pamp" ,"Thuc","Balaka","Bembeke","Chitedze","Kanyama","Lilongwe","Mlomba","Mzuzu","Manjawira","Tsangano","Salima") #"Malawi"
  MLI <- c("Kolo","Kont","Sourou Valley") #"Mali"
  TZA <- c("Kibe","Mbin","Mpangala") # "Tanzania"
  TGO <- c("Affem","Sessaro") # "Togo"
  CIV <- c("Bouake","Dabou") # "Cote d'Ivoire"
  GHA <- c("Kade","Kpong","Nyankpala") # "Ghana"
  TUN <- c("Imla") #"Tunisia"
  ETH <- c("Gewane College of Agriculture of the Afar Region","Tigray")# Ethiopia
  SDN <- "Gezira Research Station Farm" # sudan

  # filling in countries that are empty
  d$country <- ifelse(d$site %in% NGA,"Nigeria",
                      ifelse(d$site %in% ZMB,"Zambia",
                             ifelse(d$site %in% CMR,"Cameroon",
                                    ifelse(d$site %in% MOZ,"Mozambique",
                                           ifelse(d$site %in% BEN,"Benin",
                                                  ifelse(d$site %in% KEN,"Kenya",
                                                         ifelse(d$site %in% MWI,"Malawi",
                                                                ifelse(d$site %in% TZA,"Tanzania",
                                                                       ifelse(d$site %in% TGO,"Togo",
                                                                              ifelse(d$site %in% CIV,"Cote d'Ivoire",
                                                                                     ifelse(d$site %in% GHA,"Ghana",
                                                                                            ifelse(d$site %in% TUN,"Tunisia",
                                                                                                   ifelse(d$site %in% MLI,"Mali",
                                                                                                          ifelse(d$site %in% ETH,"Ethiopia",
                                                                                                              ifelse(d$site %in% SDN,"Sudan",d$country)))))))))))))))
  # fixing NA coordinates
  d$latitude[d$site == "Dengi"] <- 9.3674857
  d$longitude[d$site == "Dengi"] <- 9.9627315
  d$latitude[d$site == "Ilorin"] <- 10.8818885
  d$longitude[d$site == "Ilorin"] <- 4.007165
  d$latitude[d$site == "Kafin-Maiyaki"] <- 11.4585101
  d$longitude[d$site == "Kafin-Maiyaki"] <- 8.2022946
  d$latitude[d$site == "Menengai"] <- -0.517362
  d$longitude[d$site == "Menengai"] <- 36.0787746
  d$latitude[d$site == "Tumu"] <- 10.1121061
  d$longitude[d$site == "Tumu"] <- 11.1076295
  d$latitude[d$site == "Yandev"] <- 7.36308
  d$longitude[d$site == "Yandev"] <- 9.04235
  d$latitude[d$`Site ID` == "Rhodes and Kpaka, 1982"] <- 8.64003498
  d$longitude[d$`Site ID` == "Rhodes and Kpaka, 1982"] <- -11.8400269
  d$latitude[d$Dataset == "Kihara_Wkenya"] <- 0.4994716
  d$longitude[d$Dataset == "Kihara_Wkenya"] <- 34.5698326 # western Kenya
  d$latitude[d$site == "Guessihio"] <- 6.1090571
  d$longitude[d$site == "Guessihio"] <- -6.0018931
  d$latitude <- ifelse(d$site == "Sidi" & is.na(d$latitude),-0.17265,
                       ifelse(d$site == "Kand" & is.na(d$latitude),-0.9227613,
                              ifelse(d$site == "Thuc" & is.na(d$latitude),-15.90298,d$latitude)))
  
  d$longitude <- ifelse(d$site == "Sidi" & is.na(d$longitude),34.443840000000002,
                        ifelse(d$site == "Kand" & is.na(d$longitude),37.005539570000003,
                               ifelse(d$site == "Thuc" & is.na(d$longitude),35.29974,d$longitude)))
  
  d <- unique(d) # dropping duplicate entries
  
  # no. of rows per dataset 
  row_khra <- nrow(d[d$Dataset == "Kihara_et al. 2017_Micronutrients dataset",])
  row_tmsa <- nrow(d[d$Dataset == "TAMASA",])
  # ensuring trial_id is unique per row
  d$trial_id <- ifelse(d$Dataset == "AfSIS_DT",
                       paste(d$Dataset, d$`Site ID`, d$site, d$variety, sep = "_"),
                       ifelse(d$Dataset == "Generose_Nigeria",
                              paste(d$Dataset, d$`Site ID`, d$site, d$`Field ID`, d$variety, sep = "_"),
                              ifelse(d$Dataset == "Generose_Togo",
                                     paste(d$Dataset, d$`Site ID`, d$site, d$variety, sep = "_"),
                                     ifelse(d$Dataset == "Kihara_et al. 2017_Micronutrients dataset",
                                            paste0(d$Dataset,"_",d$site,"(",seq(1,row_khra,by = 1),")","_", d$variety),
                                            ifelse(d$Dataset == "Kihara_Wkenya",
                                                   paste(d$Dataset, d$`Site ID`, d$site, d$variety, sep = "_"),
                                                   ifelse(d$Dataset == "TAMASA",
                                                          paste0(d$Dataset,"_", d$`Field ID`,"(",seq(1,row_tmsa,by = 1),")","_", d$site,"_", d$variety),
                                                          d$ID_Micro))))))
                
  d$longitude <- as.numeric(d$longitude)
  d$rep <- as.integer(d$Replications)
  d$crop <- carobiner::fix_name(d$Crop_Type,"lower")
  d$N_fertilizer <- d$N
  d$P_fertilizer <- d$P...9
  d$K_fertilizer <- d$K
  d$soil_pH <- d$pH
  d$soil_SOC <- d$SOC
  d$soil_P_total <- d$P...28
  d$soil_clay <- d$Clay
  d$soil_type <- d$SoilType
  d$rain <- d$Rainfall
  d$dataset_id <- dataset_id
  # selecting variables of interest
  d <- d[,c("dataset_id","trial_id","country","site","latitude","longitude","crop","variety","rep","NK","NP","NPK","NPK+S+Ca+Mg+Zn+B","PK","NPK+Lime","NPK+Manure","N_fertilizer","P_fertilizer","K_fertilizer",
            "soil_pH","soil_SOC","soil_P_total","soil_clay","soil_type","rain")]
  
  # converting dataset from wide to long
  d <- reshape(d, direction = "long",
                    varying = c("NK","NP","NPK","NPK+S+Ca+Mg+Zn+B","PK","NPK+Lime","NPK+Manure"),
                    v.names = "yield",
                    times = c("NK","NP","NPK","NPK+S+Ca+Mg+Zn+B","PK","NPK+Lime","NPK+Manure"),
                    idvar = "trial_id")
  # dropping row names indices
  rownames(d) <- NULL
  
  # drop yield with NAs
  d <- d[!is.na(d$yield),]
  names(d)[19] <- "treatment"
  d$country[d$country == "Cote d'Ivoire"] <- "Côte d'Ivoire"
  d$on_farm <- TRUE
  d$country[d$site == "Imla"] <- "Ethiopia"
  
  #rearranging
  d <- d[,c("dataset_id","trial_id","country","site","latitude","longitude","crop","variety","treatment","rep","N_fertilizer","P_fertilizer","K_fertilizer",
            "soil_pH","soil_SOC","soil_P_total","soil_clay","soil_type","rain","yield","on_farm")] 
  
  # all scripts must end like this
  carobiner::write_files(dset, d, path=path)
}

