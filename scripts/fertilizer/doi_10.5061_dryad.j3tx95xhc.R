# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
	Description:
    Estimates of crop nitrogen (N) uptake and offtake are critical in estimating N balances, N use efficiencies and potential losses to the environment. Calculation of crop N uptake and offtake requires estimates of crop product yield (e.g. grain or beans) and crop residue yield (e.g. straw or stover) and the N concentration of both components. Yields of crop products are often reasonably well known, but those of crop residues are not. While the harvest index (HI) can be used to interpolate the quantity of crop residue from available data on crop product yields, harvest indices are known to vary across locations, as do N concentrations of residues and crop products. The increasing availability of crop data and advanced statistical and machine learning methods present us with an opportunity to move towards more locally relevant estimates of crop harvest index and N concentrations using more readily available data. This dataset includes maize field experiment data. It is a culmination of summary statistic data collected from the literature as well as raw data requested from various researchers and organisations from around the world. These data will enable more locally relevant estimates of crop nutrient offtake, nutrient balances and nutrient use efficiency at national, regional or global levels, as part of strategies towards more sustainable nutrient management.
"
  
  uri <- "doi:10.5061/dryad.j3tx95xhc"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    ## if there is a paper, include the paper's doi here
    ## also add a RIS file in references folder (with matching doi)
    publication= "doi:10.1016/j.fcr.2022.108578",
    data_institutions = "International Fertilizer Association",
    carob_contributor="Eduardo Garcia Bendito",
    
    ## something like randomized control...
    experiment_type="meta-analysis",
    has_weather=FALSE,
    has_soil=FALSE,
    has_management=FALSE
  )
  
  ## download and read data 
  
  ff  <- list.files(dirname(carobiner::get_data(uri, path, group)), full.names = TRUE)
  js <- carobiner::get_metadata(dataset_id, path, group)
  dset$license <- js$license
  
  f <- ff[basename(ff) == "MAIZE_DATA_HI_CPN_CRN_FIELD_CROPS_RESEARCH_2022.csv"][1]
  e <- ff[basename(ff) == "DATA_ID_MAIZE_DATA_HI_CPN_CRN_FIELD_CROPS_RESEARCH_2022.csv"][1]
  
  d <- read.csv(f)
  dd <- read.csv(e)
  
  d <- merge(d,dd,by = "Data_id")
  ## process file(s)
  
  #### about the data #####
  ## (TRUE/FALSE)
  
  d$on_farm <- FALSE
  d$is_survey <- FALSE
  d$irrigated <- ifelse(d$Water_regime == "Irrigated", TRUE, FALSE)
  ## the treatment code	
  d$trial_id <- d$Publication
  d$trial_id[d$trial_id=="Horv\xe1th et al 2021"] <- "Horváth et al 2021"
  d$trial_id[d$trial_id=="Martin\xednezCuesta et al 2020"] <- "MartinínezCuesta et al 2020"
  d$trial_id[d$trial_id=="Sz\xe9les et al 2019"] <- "Széles et al 2019"
  
  ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  d$country <- d$Country
  d$country[d$country == "C\xf4te d\x92Ivoire"] <- "Côte d'Ivoire"
  
  d$Site[d$Site == "Ca\xf1ada de Gomez"] <- "Cañada de Gómez"
  d$Site[d$Site == "Ca\xf1ada Rica"] <- "Cañada Rica"
  d$Site[d$Site == "Cai\xf1ogan Alto"] <- "Cañogan Alto"
  d$Site[d$Site == "Sto. Ni\xf1o"] <- "Sto. Niño"
  d$Site[d$Site == "Sztgyv\xf6lgy"] <- "Sztgyvölgy"
  d$Site[d$Site == "Bics\xe9rd"] <- "Bicsérd"
  d$Site[d$Site == "Isma\xeflia"] <- "Ismaïlia"
  d$Site[d$Site == "Rol\xe2ndia"] <- "Rolândia"
  d$Site[d$Site == "Brgy. Cape\xf1ahan"] <- "Brgy. Capeñahan"
  d$Site[d$Site == "Chill\xe1n"] <- "Chillán"
  d$Site[d$Site == "L\xe9vis"] <- "Lévis"
  d$Site[d$Site == "La Concepci\xf3n"] <- "La Concepción"
  d$site <- carobiner::fix_name(d$Site, "title")
  
  
  d$adm1 <- d$Region_province
  d$adm1[d$adm1 == "Gouvernement Isma\xeflia"] <- "Gouvernement Ismaïlia"
  d$adm1[d$adm1 == "Hajd\xfa-Bihar"] <- "Hajdú-Bihar"
  d$adm1[d$adm1 == "Paran\xe1"] <- "Paraná"
  d$adm1[d$adm1 == "Diguill\xedn"] <- "Diguillín"
  d$adm1[d$adm1 == "Goi\xe1s"] <- "Goiás"
  d$adm1[d$adm1 == "Quer\xe9taro"] <- "Querétaro"
 
  d$adm1 <- carobiner::fix_name(d$adm1, "title")

  ## each site must have corresponding longitude and latitude
  d$longitude <- d$GPS_long_DD
  d$latitude <- d$GPS_lat_DD

  ##### Crop #####
  ## normalize variety names
  d$crop <- tolower(d$Crop)
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  d$P_fertilizer <- d$FP*10000 # Converting from kg/m2 -> kg/ha
  d$K_fertilizer <- d$FK*10000 # Converting from kg/m2 -> kg/ha
  d$N_fertilizer <- d$FN*10000 # Converting from kg/m2 -> kg/ha
  d$fertilizer_type <- "unknown" # Fertilizer type not specified
  
  ##### Yield #####
  
  d$yield <- d$CPY * 1000 # Megagram to kilogram
  
  d$dataset_id <- dataset_id
  d <- d[,c(27:length(d))]
  
  # all scripts must end like this
  carobiner::write_files(dset, d, path, dataset_id, group)
}
