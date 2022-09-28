
"Title: N2Africa farm monitoring - Malawi, 2012 - 2013

Description: N2Africa is to contribute to increasing biological nitrogen fixation and productivity
of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility,
improving household nutrition and increasing income levels of smallholder farmers. As a vision of success,
N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit
from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants
and fertilizers adapted to local settings. A strong national expertise in grain legume production and
N2-fixation research and development will be the legacy of the project.The project is implemented in
five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo,
Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"
carob_script <- function(path) {

  uri <- "https://doi.org/10.25502/NY1Z-W564/D"
  dataset_id <- agro::get_simple_URI(uri)
  group <- "variety_performance"

  ## data set level data
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication="",
    data_citation = "Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, P., Baijukya, F.,
    Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira, L., Kamai, N., Ampadu-Boakye, T., Ronner, E.,
    Kanampiu, F., Giller, K., Baars, E., & Heerwaarden, J. van. (2020). N2Africa farm monitoring - Malawi,
    2012 - 2013 [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/NY1Z-W564/D",
    data_institutions = "IITA",
    carob_contributor="Rachel Mukami",
    experiment_type="variety_performance",
    has_weather=TRUE,
    has_management=TRUE
  )

   ## download and read data

   ff  <- carobiner::get_data(uri, path, group)
   js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
   dset$license <- carobiner::get_license(js)

   # reading the datasets
   f <- ff[basename(ff) == "a_general.csv"]
   d <- data.frame(read.csv(f))
   d$trial_id <- d$farm_id
   d$adm1 <- d$district
   d$adm2 <- d$sector_ward
   d$adm3 <- d$vilage
   d$location <- d$action_site
   d <- d[,c("trial_id","season","country","adm1","adm2","adm3","location")]

   f1 <- ff[basename(ff) == "a_technician.csv"]
   d1 <- data.frame(read.csv(f1))
   d1$trial_id <- d1$farm_id
   d1$observation_date <- as.Date(d1$date,"%m/%d/%Y")
   d1 <- d1[,c("trial_id","observation_date")]
   
   f2 <- ff[basename(ff) == "c_use_of_package_1.csv"]
   d2 <- data.frame(read.csv(f2))
   d2$trial_id <- d2$farm_id
   d2$inoculated <- ifelse(d2$inoculant_used == "Y","yes",
                            ifelse(d2$inoculant_used == "N","no",NA))
   d2$OM_used <- ifelse(d2$org_fertilizer_type %in% c("animal dung","Pit manure","compost","Dung",
                                                      "compost manure","Chimato","Compost manure","Manure",
                                                      "Animal dung","Compost","Crop residues","Pit Manure"),"yes",
                        ifelse(d2$org_fertilizer_amount_kg == 200,"yes","no"))
   d2$OM_type <- ifelse(d2$org_fertilizer_type %in% c("animal dung","Pit manure","compost","Dung",
                                                      "compost manure","Chimato","Compost manure","Manure",
                                                      "Animal dung","Compost","Crop residues","Pit Manure"),d2$org_fertilizer_type,NA)

   d2$OM_applied <- suppressWarnings(as.numeric(gsub("kg","",d2$org_fertilizer_amount_kg))*(10000/as.numeric(d2$plot_size))) #amount of org fert in kg/ha
   d2$fertilizer_type <- d2$min_fertilizer_type
   d2$min_fertiliser_amount_kg <- suppressWarnings(as.numeric(gsub("kg","",d2$min_fertiliser_amount_kg))*(10000/as.numeric(d2$plot_size))) #amount of fertilizer used in kg/ha

   # assumption is that NPK and compound fertilizer 23:21:0+4S have similar mineral composition
   d2$N_fertilizer <- ifelse(d2$fertilizer_type == "NPK, UREA",d2$min_fertiliser_amount_kg * 0.46,
                             ifelse(d2$fertilizer_type %in% c("NPK","23:21:0+4s"),
                                    d2$min_fertiliser_amount_kg * 0.23,
                                    ifelse(d2$fertilizer_type %in% c("D-Compound","D-Compound, Gypsum (25kgeach)","D-compound"),
                                           d2$min_fertiliser_amount_kg * 0.08,
                                           ifelse(d2$fertilizer_type == "Super-D",
                                                  d2$min_fertiliser_amount_kg * 0.01,NA))))

   d2$P_fertilizer <- ifelse(d2$fertilizer_type == "TSP",
                             d2$min_fertiliser_amount_kg * 0.46*((2*31)/(2*31+5*16)),
                             ifelse(d2$fertilizer_type %in% c("Sympal","sympal","Sympa","sympal "),
                                    d2$min_fertiliser_amount_kg * 0.23*((2*31)/(2*31+5*16)),
                                    ifelse(d2$fertilizer_type %in% c("NPK, UREA","NPK","23:21:0+4s"),
                                           d2$min_fertiliser_amount_kg * 0.21*((2*31)/(2*31+5*16)),
                                           ifelse(d2$fertilizer_type %in% c("D-Compound","D-Compound, Gypsum (25kgeach)","D-compound"),
                                                  d2$min_fertiliser_amount_kg * 0.18*((2*31)/(2*31+5*16)),
                                                  ifelse(d2$fertilizer_type == "Super-D",
                                                         d2$min_fertiliser_amount_kg * 0.24*((2*31)/(2*31+5*16)),NA))))) # takes into account atomic weight of both P and O

   d2$K_fertilizer <- ifelse(d2$fertilizer_type %in% c("Sympal","sympal","Sympa","sympal "),d2$min_fertiliser_amount_kg * 0.15,
                             ifelse(d2$fertilizer_type %in% c("D-Compound","D-Compound, Gypsum (25kgeach)","D-compound"),
                                    d2$min_fertiliser_amount_kg * 0.15,
                                    ifelse(d2$fertilizer_type == "Super-D",d2$min_fertiliser_amount_kg * 0.20,NA)))

   d2 <- d2[,c( "trial_id","crop","variety","inoculated","OM_used","OM_type","OM_applied",
                "fertilizer_type","N_fertilizer", "P_fertilizer","K_fertilizer")]
   
   f3 <- ff[basename(ff) == "c_use_of_package_3.csv"]
   d3 <- data.frame(read.csv(f3))
   d3$trial_id <- d3$farm_id
   d3$row_spacing <- d3$crop_1_spacing_row_to_row
   d3$plant_spacing <- d3$crop_1_spacing_plant_to_plant
   d3 <- d3[,c("trial_id","row_spacing","plant_spacing")] # assumption is spacing is in cm

   f4 <- ff[basename(ff) == "d_cropping_calendar.csv"]
   d4 <- data.frame(read.csv(f4))
   d4$trial_id <- d4$farm_id
   # to remove unnecessary rows
   x <- d4[d4$activity == "Date of planting",]
   x$start_date <- as.Date(paste(x$date_planting_mm,x$date_planting_dd,x$date_planting_yyyy,sep = "/"),"%m/%d/%Y")
   y <- d4[d4$activity == "Date of harvest",]
   y$end_date <- as.Date(paste(y$date_planting_mm,y$date_planting_dd,y$date_planting_yyyy,sep = "/"),"%m/%d/%Y")
   d4 <-  merge(x,y,by = "trial_id",all = TRUE)
   d4 <- d4[,c("trial_id","start_date","end_date")]
   
   f5 <- ff[basename(ff) == "e_harvest.csv"]
   d5 <- data.frame(read.csv(f5))
   d5 <- d5[d5$crop_1_grain_unshelled == "Y",] # getting grain weights for only unshelled grains
   d5$trial_id <- d5$farm_id
   d5$grain_weight <- (as.numeric(d5$area_harvested_m2)*d5$weight_kg*1000)/100 # standardizing to grain weight measured in g/100m2
   d5 <- d5[,c("trial_id","grain_weight")]
   
   f6 <- ff[basename(ff) == "f_farmer_assessment.csv"]  
   d6 <- data.frame(read.csv(f6))
   d6$trial_id <- d6$farm_id
   d6 <- d6[,c("trial_id","yield")] # yield's units are unknown and are rated from 1 through 4

   # combining into one dataset
   z <- carobiner::bindr(d,d1,d2,d3,d4,d5,d6)
   z$is_survey <- "yes"
   z$on_farm <- "no"
   z$longitude <- 34.30153
   z$latitude <- -13.25431
   z$dataset_id <- dataset_id
   z$country <- "Malawi"
   z$crop <- tolower(z$crop)
   z$crop <- ifelse(z$crop == "groundnuts", "groundnut",
                     ifelse(z$crop %in% c("bush bean","beans","n"),"common bean",z$crop))
   z$crop <- ifelse(z$variety == "IT8216","cowpea",
                     ifelse(z$variety == "Nsinjiro","groundnut",
                            ifelse(z$variety == "kholophethe","common bean",z$crop)))
   z$crop[is.na(z$crop)] <- "common bean" # filled NAs in crops with common bean
   
   z <- z[,c("dataset_id","trial_id","season","country","adm1","adm2","adm3","location","crop","variety",
             "start_date","end_date","inoculated","OM_used","OM_type","OM_applied","fertilizer_type",
             "N_fertilizer","P_fertilizer","K_fertilizer","grain_weight","yield","row_spacing","plant_spacing",
             "on_farm","is_survey","longitude","latitude")]
   
   # all scripts must end like this
   carobiner::write_files(dset, z, path, dataset_id, group)
   TRUE
 }
