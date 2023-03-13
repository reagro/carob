
carob_script <- function(path){
  
  "
Title: N2Africa agronomy trials - Ethiopia, 2013
  
Description: N2Africa is to contribute to increasing biological nitrogen fixation and productivity 
of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, 
improving household nutrition and increasing income levels of smallholder farmers. As a vision of success,
N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit 
from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants
and fertilizers adapted to local settings. A strong national expertise in grain legume production and 
N2-fixation research and development will be the legacy of the project. The project is implemented in 
five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, 
Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.

" 
  uri <- "doi:10.25502/X2H1-AT51/D"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  
  ## dataset level data 
  
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    project="N2Africa",
    publication= "https://doi.org/10.1080/23311932.2020.1722353",
    data_citation = "Vanlauwe, Bernard, Samuel, A.-N., Endalkachew, W., Peter, E., Freddy, B., Jean-Marie, S., Paul, W., Regis, C., Lloyd, P., Nkeki, K., Theresa, A.-B., Esther, R., Fred, K., Ken, G., Edward, B., & Heerwaarden, J. van. (2020). N2Africa agronomy trials - Ehtiopia, 2013 [Data set].
    International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/X2H1-AT51/D",
    data_institutions = "IITA",
    carob_contributor="Rachel Mukami",
    experiment_type="Symbiotic N2 fixation",
    has_weather=TRUE,
    has_management=TRUE)
  
  ff <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  dset$license <- carobiner::get_license(js)

  # The activities.csv, nutr_deficiency_pest_disease.csv,pesticide_biocide_use.csv datasets don't 
  # contain additional info as it's important information is already represented in d1 below.
  # processing crop_observations.csv
  f <- ff[basename(ff) == "crop_observations.csv"]
  d <- read.csv(f)
  d$rep <- d$replication_no
  d$variety <- tolower(carobiner::fix_name(d$variety))
  d$variety <- ifelse(d$variety == "nassir","nasir",
                       ifelse(d$variety %in% c("awasa dume","awash dume"),"hawassa dume",
                              ifelse(d$variety == "argen","argene",
                                     ifelse(d$variety == "local","loko",
                                            ifelse(d$variety %in% c("dinknesh","dinkenesh"),"dinkinesh",
                                                   ifelse(d$variety == "dimitu","dimtu",
                                                          ifelse(d$variety == "awasa-04","hawasa04",
                                                                 ifelse(d$variety %in% c("didesa (v1)","didessa"),"didesa",
                                                                        ifelse(d$variety == "ethio-ugozilavia(v2)","ethio-ugozilavia",
                                                                               ifelse(d$variety %in% c("awash-1","awash1"),"awash 1",
                                                                                      ifelse(d$variety == "habiru","habru",
                                                                                             ifelse(d$variety == "tumssa","tumsa",d$variety))))))))))))
  # sub_treatment and sub_sub_treatment have null values hence will be ignored
  
  d$treatment <- tolower(carobiner::fix_name(d$main_treatment))
  d$treatment[d$treatment == "#name?"] <- NA
  
  #p or +p shows presence of phosphorus, while +r or i shows presence of inoculants
  
  d$treatment <- ifelse(d$treatment %in% c("+p+ +r","25kgdap&inoculant","+i +p","+i + +p",
                                           "+p and +i","with p, i","+p,+i","i, p"),"+p,+i",
                        ifelse(d$treatment %in% c("-p+ -r","-i -p","-i + -p","-p and -i",
                                                  "w/out i, p","withoutinputs"),"-p,-i",
                               ifelse(d$treatment %in% c("-p+ +r","+i -p","+i + -p","-p and +i"),
                                      "-p,+i",
                                      ifelse(d$treatment %in% c("+p+ -r","-i +p","-i + +p","+p and -i","-i,+p"),
                                             "+p,-i",
                                             ifelse(d$treatment %in% c("25kgdap","p"),"+p",
                                                    ifelse(d$treatment == "variety + +i & +p",(tolower(paste(d$variety,"+i,+p",sep = ","))),
                                                           ifelse(d$treatment %in% c("inoculant","i"),"+i",
                                                                  ifelse(d$treatment == "variety",tolower(d$variety),
                                                                         ifelse(d$treatment == "dinkenesh","dinkinesh",
                                                                                ifelse(d$treatment == "awash1","awash 1",d$treatment))))))))))
  d$treatment[d$treatment == "local"] <- "loko"
  d$start_date <- as.character(as.Date(d$date_planting,"%m/%d/%Y"))
  d$end_date <- as.character(as.Date(d$date_harvest, "%m/%d/%Y"))
  d$plant_density <- d$no_plants/(d$area_harvest_plot_m2/10000) # based on harvested plants from harvested area/ha
  d$plant_density[d$plant_density == "NaN"] <- NA
  d$yield <- as.numeric(d$grain_yield_kgperha)
  d <- d[d$yield > 0,]
  d$residue_yield<- as.numeric(d$total_yield_stover_kg_per_ha)
  d$biomass_total <- as.numeric(d$calc_weight_a_ground_biomass_kg)
  d$grain_weight <- d$dry_weight_100_seed_g * 10
  d <- d[,c("trial_id","rep","treatment","variety","start_date","end_date","plant_density","yield",
              "residue_yield","biomass_total","grain_weight")]

  # processing field_history.csv
  f1 <- ff[basename(ff) == "field_history.csv"]
  d1 <- data.frame(read.csv(f1))
  d1$previous_crop <- tolower(d1$crop_n2a_plot_p_season)
  d1$previous_crop <- ifelse(d1$previous_crop == "bread wheat","wheat",
                             ifelse(d1$previous_crop == "tef","teff",
                                    ifelse(d1$previous_crop == "noug (guziota scarba)","noug",d1$previous_crop)))
  d1$previous_crop[d1$previous_crop == ""] <- NA
  d1 <- d1[,c("trial_id","previous_crop")]
  
  # processing general.csv
  f2 <- ff[basename(ff) == "general.csv"]
  d2 <- data.frame(read.csv(f2))
  d2$adm3 <- tolower(d2$district_county)
  d2$adm3 <- ifelse(d2$adm3 == "bichena","enemay",
                    ifelse(d2$adm3 == "jama","jamma",
                           ifelse(d2$adm3 == "gobu sayo","gobu seyo",d2$adm3))) # bichena is a town in enemay district(woreda)

  # most village inputs seem to be small towns in Ethiopia according to http://www.blogabond.com/LocationBrowse.aspx?CountryCode=ET&l=Ethiopia&showAll=1
  # so they'll be allotted locations.
  d2$location <- tolower(d2$village)
  d2$site <- tolower(d2$site)
  d2$site[d2$site == ""] <- NA
  d2$elevation <- as.numeric(d2$gps_altitude)
  d2$crop <- tolower(d2$type_of_experiment)
  d2$crop <- ifelse(d2$crop %in% c("commonbean_babytrial","common bean_input","commonbean_input","Commonbean_input",
                                   "common bean_var","commonbean_variety"),"common bean",
                                   ifelse(d2$crop %in% c("chickpea_input","chickpea_variety"),"chickpea",
                                          ifelse(d2$crop %in% c("fababean_input","fababean_variety"),"faba bean","soybean")))
  d2 <- d2[,c("trial_id","country","adm3","location","site","elevation","crop")]
  d3 <- merge(d2,d1,by = "trial_id")
  
  # processing rainfall.csv
  f4 <- ff[basename(ff) == "rainfall.csv"]
  d4 <- data.frame(read.csv(f4))
  d4 <- d4[d4$rain_mm > 0,]
  
  # averaging rain amount because we cannot specifically 
  # point individual rain inputs to specific reps under specific trial_ids
  b1 <- tapply(d4$rain_mm, d4$trial_id, mean)
  d4 <- data.frame(trial_id = names(b1),rain = b1)
  rownames(d4) <- NULL

  # processing soil_data.csv
  f5 <- ff[basename(ff) == "soil_data.csv"]
  d5<- data.frame(read.csv(f5)) 
  d5$trial_id <- toupper(d5$farm_id)
  d5$soil_pH <- d5$ph
  d5$soil_SOC <- d5$tc_perc
  d5$soil_N <- d5$n_perc
  d5$soil_sand <- as.numeric(d5$sand_perc)
  d5$soil_clay <- as.numeric(d5$clay_perc)
  d5 <- d5[,c("trial_id","soil_pH","soil_SOC","soil_N","soil_sand","soil_clay")]

  # compiling into a single final dataset
  
  d6 <- merge(d3,d5,by = "trial_id",all.x = TRUE)
  d7 <- merge(d6,d4,by = "trial_id",all.x = TRUE)
  f <- merge(d,d7,by = "trial_id",all.x = TRUE)
  
  f$dataset_id <- dataset_id
 
  # Fertilizer rates: DAP will be applied using a rate of 25 kg DAP per hectare; DAP has 18:46:0 composition
  # calculating amount of P in DAP applied assuming that any +P input refers to DAP application; 
  
  P2O5 <- 25 * 0.46
  # to acquire the amount of P in P2O5, P has atomic weight 31 while O has atomic weight 16.
  P <- P2O5*((2*31)/(2*31+5*16))  
  N <- 25 * 0.18
  
  f$fertilizer_type <- "none"
  f$P_fertilizer <- ifelse(f$treatment %in% c("+p,+i","+p,-i","+p","hachalu,+i,+p","wayu,+i,+p","wolki,+i,+p","dagim,+i,+p","lalo,+i,+p","local,+i,+p"),P,0)
  f$N_fertilizer <- ifelse(f$treatment %in% c("+p,+i","+p,-i","+p","hachalu,+i,+p","wayu,+i,+p","wolki,+i,+p","dagim,+i,+p","lalo,+i,+p","local,+i,+p"),N,0)
  f$K_fertilizer <- 0
  f$inoculated <- grepl("\\+i", f$treatment)
  f$fertilizer_type[f$N_fertilizer > 0] <- "DAP"
  f$fertilizer_type[f$P_fertilizer > 0] <- "DAP"
  f$country <- "Ethiopia"
  f$row_spacing <- 40 
  f$plant_spacing <-10
  f$latitude <- 9.14500
  f$longitude <- 40.48967
  f$on_farm <- as.logical("TRUE")

  carobiner::write_files(dset, f, path, dataset_id, group)
}

