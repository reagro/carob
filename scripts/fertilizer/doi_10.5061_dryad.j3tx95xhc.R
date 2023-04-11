# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
	Description:
    Estimates of crop nitrogen (N) uptake and offtake are critical in estimating N balances, N use efficiencies and potential losses to the environment. Calculation of crop N uptake and offtake requires estimates of crop product yield (e.g. grain or beans) and crop residue yield (e.g. straw or stover) and the N concentration of both components. Yields of crop products are often reasonably well known, but those of crop residues are not. While the harvest index (HI) can be used to interpolate the quantity of crop residue from available data on crop product yields, harvest indices are known to vary across locations, as do N concentrations of residues and crop products. The increasing availability of crop data and advanced statistical and machine learning methods present us with an opportunity to move towards more locally relevant estimates of crop harvest index and N concentrations using more readily available data. This dataset includes maize field experiment data. It is a culmination of summary statistic data collected from the literature as well as raw data requested from various researchers and organisations from around the world. These data will enable more locally relevant estimates of crop nutrient offtake, nutrient balances and nutrient use efficiency at national, regional or global levels, as part of strategies towards more sustainable nutrient management.
"
  
  uri <- "https://doi.org/10.5061/dryad.j3tx95xhc"
  dataset_id <- agro::get_simple_URI(uri)
  group <- "fertilizer"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    ## if there is a paper, include the paper's doi here
    ## also add a RIS file in references folder (with matching doi)
    publication= "https://doi.org/10.1016/j.fcr.2022.108578",
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
  
  
  ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  d$country <- ifelse(d$Country == "C\xf4te d\x92Ivoire", "Côte d'Ivoire", d$Country)
  d$site <- d$Site
  d$adm1 <- d$Region_province
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
  
  ##### Yield #####
  
  d$yield <- d$CPY * 1000 # Megagram to kilogram
  
  d$dataset_id <- dataset_id
  d <- d[,c(27:length(d))]
  
  # all scripts must end like this
  carobiner::write_files(dset, d, path, dataset_id, group)
}
