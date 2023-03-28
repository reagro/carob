
"
RM: 2023-01-23
RM: note that no yield data are provided. So these data are not in scope for us.

Title: N2Africa farm monitoring - Malawi, 2011 - 2012

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
no_carob_script <- function(path) {
  
  uri <- "https://doi.org/10.25502/14m8-cs44/d"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  
  ## data set level data
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
	  project="N2Africa",
    uri=uri,
    publication=NA,
    data_citation = "Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, P., Baijukya, F., Sanginga, 
    J.-M., Woomer, P., Chikowo, R., Phiphira, L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, F., 
    Giller, K., Baars, E., & Heerwaarden, J. van. (2020). N2Africa farm monitoring - Malawi, 2011 - 2012 [Data set]. 
    International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/14M8-CS44/D",
    data_institutions = "IITA",
    carob_contributor="Rachel Mukami",
    experiment_type = "fertilizer",
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
  d$adm1 <- carobiner::fix_name(d$district, "title")
  d$adm1 <- carobiner::replace_values(d$adm1,
                                      c("Dedaza","Mnchinji"),
                                      c("Dedza","Mchinji"))
  d <- d[!is.na(d$adm1),]
  d$adm2 <- carobiner::fix_name(d$sector_ward, "title") 
  d$adm2 <- carobiner::replace_values(d$adm2,
                                      c("Bowe Epa","Chinguluwe Epa","Kaluma","Kapili","Kasangadzi Adp","Kaufulu Adp",
                                        "Linthipe Epa","Lipili","Lipiri Adp","Mayani Epa","Mgwangwa Epa","Mgwangwa","Mngwangwa Epa","Mkanda East","Mloyeni",
                                        "Mlonyeni Epa","Tchesa Adp"),
                                      c("Bowe","Chinguluwe","Kaluluma","Kapiri","Kasangadzi","Kaufulu",
                                        "Linthipe","Lipiri","Lipiri","Mayani","Mngwangwa","Mngwangwa","Mngwangwa","Mkanda","Mlonyeni","Mlonyeni","Tchesa"))
  
  tt <- data.frame(adm1 = c("Dowa","Kasungu","Mzimba","Dedza","Salima","Lilongwe","Mchinji","Ntcheu"),
                    latitude = c(-13.6576733,-12.5817922,-11.845810199999999,-14.0909391,-13.7634166,-14.0413366,-13.782889749999999,-14.83041285),
                    longitude = c(33.9302091,33.5190174,33.55783191131472,34.2483212,34.4495201,33.7343103,33.020473863724945,34.765831629561944))
  d <- merge(d,tt,by = "adm1",all.y = TRUE)
  d$latitude <- ifelse(d$adm2 == "Karonga",-10.08474695,d$latitude)
  d$longitude <- ifelse(d$adm2 == "Karonga",33.86619181196623,d$longitude)
  d$latitude <- ifelse(d$adm2 == "Mkanda",-13.5170084,d$latitude)
  d$longitude <- ifelse(d$adm2 == "Mkanda",32.9542932,d$longitude)
  
  # vilage contains people's names so it will be dropped

  d <- d[,c("trial_id","season","country","adm1","adm2","latitude","longitude")]
  
  f1 <- ff[basename(ff) == "c_use_of_package_2.csv"]
  d1 <- data.frame(read.csv(f1))
  d1$trial_id <- d1$farm_id
  d1$crop <- tolower(d1$crop_1)
  d1$crop[d1$crop =="bush bean"] <- "common bean"
  d1$crop[d1$crop == "sweet potato"] <- "sweetpotato"
  d1$variety <- carobiner::fix_name(ifelse(d1$variety_1 == "", NA, d1$variety_1))
  d1$variety <- carobiner::replace_values(d1$variety,
                                          c("CG7","IT 16","makwacha","NASOKO"),
                                          c("CG 7","IT-16","Makwacha","Nasoko"))
  d1$variety <- ifelse(d1$variety %in% c("IT 82E-16","IT 82E -16"),"IT82E-16",
                       ifelse(d1$variety %in% c("kalima","KALIMA"),"Kalima",
                              ifelse(d1$variety %in% c("Kholophet","Kholophete"),"Kholophethe",
                                     ifelse(d1$variety %in% c("Nsijiro","nsinjiro"),"Nsinjiro",
                                            ifelse(d1$variety %in% c("Soliteire","Solitera"),"Solitare",d1$variety)))))
  d1$inoculated <- ifelse(d1$inoculant_used == "Y", TRUE,
                          ifelse(d1$inoculant_used == "N", FALSE, NA))
  d1$OM_used <- ifelse(d1$organic_fert_type %in% 
		c("Animal dung","Animal manure","Compost","Compost manure",
          "Farmyard","Farm yard","Compost, animal manure"), TRUE,
                       ifelse(d1$organic_fert_amount > 0, TRUE, FALSE))
  
  # Fixing OM_type to make it NA where 0 OM applied
  d1$OM_type <- ifelse(d1$organic_fert_amount <= 0, NA,
                       ifelse(d1$organic_fert_type %in% 
                                c("Animal dung","Animal manure","Compost","Compost manure",
                                  "Farmyard","Farm yard","Compost, animal manure"), d1$organic_fert_type,NA))
  d1$OM_type <- carobiner::replace_values(d1$OM_type,
                                          c("Animal dung","Farm yard","Compost"),
                                          c("Animal manure","Farmyard","Compost manure"))
  d1$OM_type <- carobiner::fix_name(d1$OM_type,"title")
  d1$OM_applied <- as.numeric(d1$organic_fert_amount)
  d1$fertilizer_type <- NA
                               
  d1$fertilizer_type[grepl("TSP", d1$mineral_fert_type)] <- "TSP"
  d1$fertilizer_type[grepl("phosphate", d1$mineral_fert_type)] <- "SSP"
  d1$fertilizer_type[grepl("D", d1$mineral_fert_type)] <- "D compound"
  d1$fertilizer_type[grepl("Super d", d1$mineral_fert_type)] <- "D compound" # Super D and D Compound seem to be the same blend (https://agra.org/wp-content/uploads/2020/08/Malawi-Report_Assessment-of-Fertilizer-Distribution-Systems-and-Opportunities-for-Developing-Fertilizer-Blends.pdf)
  d1$fertilizer_type[grepl("S-", d1$mineral_fert_type)] <- "S compound"
  d1$fertilizer_type[grepl("UREA", d1$mineral_fert_type)] <- "urea" # Assigning all fertilizations with UREA to urea, since NPK is not a fertilizer...
  d1$fertilizer_type[grepl("4S", d1$mineral_fert_type)] <- "urea" # Assigning all fertilizations with UREA to urea, since NPK is not a fertilizer...
  d1$fertilizer_type[grepl("ympal", d1$mineral_fert_type, ignore.case = TRUE)] <- "sympal" # This is a fertilizer specific to Kenya. For more info read here: https://www.researchgate.net/profile/Charlotte-Schilt/publication/283304707_N2Africa_Final_Report_of_the_first_Phase_-_2009_-_2013/links/5d77729c4585151ee4ab2639/N2Africa-Final-Report-of-the-first-Phase-2009-2013.pdf
  d1$fertilizer_type[d1$mineral_fert_amount > 0 & is.na(d1$fertilizer_type)] <- "unknown"
  
  # Adjusting NPK amounts
  d1$N_fertilizer <- ifelse(d1$fertilizer_type %in% c("urea"), d1$mineral_fert_amount * 0.46,  # assumption is that mineral fert amount is in kg
                            ifelse(d1$mineral_fert_type == "23:21:0+4S", d1$mineral_fert_amount * 0.23,
                                   ifelse(d1$fertilizer_type %in% c("D compound", "S compound"), d1$mineral_fert_amount * 0.08, NA)))
  
  d1$P_fertilizer <- ifelse(d1$fertilizer_type == "TSP", d1$mineral_fert_amount * 0.46*((2*31)/(2*31+5*16)),
                            ifelse(d1$fertilizer_type == "sympal", d1$mineral_fert_amount * 0.23*((2*31)/(2*31+5*16)),
                                   ifelse(d1$fertilizer_type %in% c("urea","unknown","S compound"), d1$mineral_fert_amount * 0.21*((2*31)/(2*31+5*16)),
                                          ifelse(d1$fertilizer_type == "D compound", d1$mineral_fert_amount * 0.18*((2*31)/(2*31+5*16)),
                                                 ifelse(d1$fertilizer_type == "SSP", d1$mineral_fert_amount * 0.145, NA)))))
                                                
  d1$K_fertilizer <- ifelse(d1$fertilizer_type == "sympal", d1$mineral_fert_amount * 0.15,
                            ifelse(d1$mineral_fert_type == "S compound", d1$mineral_fert_amount * 0.07,
                                   ifelse(d1$fertilizer_type == "D compound", d1$mineral_fert_amount * 0.20, NA)))
  
  d1 <- d1[,c("trial_id","crop","variety","inoculated","OM_used","OM_applied","OM_type","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer")]
  
  
  f2 <- ff[basename(ff) == "c_use_of_package_4.csv"]
  d2 <- data.frame(read.csv(f2))
  d2$trial_id <- d2$farm_id
  d2$row_spacing <- as.numeric(d2$crop_1_spacing_row_to_row)
  d2$plant_spacing <- as.numeric(d2$crop_1_spacing_plant_to_plant)
  d2 <- d2[,c("trial_id","row_spacing","plant_spacing")] # assumption is spacing is in cm
  

  f3 <- ff[basename(ff) == "d_cropping_calendar.csv"]
  d3 <- data.frame(read.csv(f3))
  d3$trial_id <- d3$farm_id
  d3$start_date <- as.character(as.Date(paste(d3$date_planting_mm,d3$date_planting_dd,d3$date_planting_yyyy,sep = "/"),"%m/%d/%Y"))
  d3 <- d3[,c("trial_id","start_date")]
  
  ##  Removing e_harvest.csv, since it doesn't provide pressing information.

  f5 <- ff[basename(ff) == "g_farmer_assessment.csv"]  
  d5 <- data.frame(read.csv(f5))
  d5$trial_id <- d5$farm_id
  # should yield be NA or ) if there are no quantitative figures
  d5$yield <- 0 # yield is has no quantitative figures
  d5 <- d5[,c("trial_id","yield")] 
  
  # combining into one dataset
  dd <- merge(d,d3 ,by ="trial_id",all.x = TRUE)
  jj <- data.frame(trial_id = unique(d5$trial_id),yield = rep(NA,length(unique(d5$trial_id))))
  dd1 <- merge(dd,jj, by = "trial_id",all.x = TRUE)
  dd2 <- merge(d1,d2, by = "trial_id", all = TRUE)
  z <- merge(dd2,dd1,by = "trial_id",all.x = TRUE)
  # z <- Reduce(function(...) merge(..., all=T), list(d,d1,d2,d3,d5))
  z$is_survey <- TRUE
 ## it is a survey, so per definition on-farm. 
  z$on_farm <- TRUE
  z$dataset_id <- dataset_id
  z$country <- "Malawi"
  z$yield <- as.numeric(z$yield)
  z <- z[,c("dataset_id","trial_id","country","adm1","adm2","latitude","longitude","crop","variety","season",
            "start_date","inoculated","OM_used","OM_type","OM_applied","fertilizer_type",
            "N_fertilizer","P_fertilizer","K_fertilizer","row_spacing","plant_spacing","yield",
            "on_farm","is_survey")]
  
  # all scripts must end like this
  carobiner::write_files(dset, z, path, dataset_id, group)
}
