
# RH: "groundnut" is "randomly assinged as the crop! 
# RH: you have got to be kidding. We do not do any random assingments?
# RH: It seems the the crop info is perhaps available (as variables) in "d"


#################################################################################
#N2Africa was aimed at increasing biological nitrogen fixation and productivity
#of grain legumes through effective production technologies including inoculants
#and fertilizers adapted to local settings which was aimed at increasing soil
#fertility.The trails were conducted in 11 African countries
#################################################################################

carob_script <- function(path){

uri <- "doi.org/10.25502/JHRJ-9423"
dataset_id <- agro::get_simple_URI(uri)
group <- "variety_performance"


dset <- data.frame (
  dataset_id = dataset_id,
  group = group,
  uri = uri,
  publication = NA,
  data_citation =" Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat,
  P., Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira,
  L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller, 
  K., Ampadu-Boakye, T., & Heerwaarden, J. van. (2020). N2Africa farm 
  monitoring - Mozambique, 2011 - 2012 [Data set]. International 
  Institute of Tropical Agriculture (IITA). 
  https://doi.org/10.25502/JHRJ-9423",
  carob_contributor = "Effie Ochieng",
  experiment_type = "variety_performance",
  has_weather = FALSE,
  has_management = FALSE)

#extract the data
ff <- carobiner::get_data(uri,path,group)
js <- carobiner::get_metadata(dataset_id, path, group)
dset$license <- carobiner::get_license(js)

#read the data
f <- ff[basename(ff) == "a_general.csv"]
d <- data.frame(read.csv2(f, sep = ","))
f1 <- ff[basename(ff)== "b_info_site_2.csv"]
d1 <- data.frame(read.csv2(f1, sep = ","))
f2 <- ff[basename(ff) =="c_use_of_package_2.csv" ]
d2 <- data.frame(read.csv2(f2, sep = ","))
f3 <- ff[basename(ff) =="c_use_of_package_4.csv" ]
d3 <- data.frame(read.csv2(f3, sep = ","))
f4 <- ff[basename(ff) == "d_cropping_calendar.csv"]
d4<- data.frame(read.csv2(f4, sep = ",")) 
f5 <- ff[basename(ff) == "e_harvest.csv"]
d5 <- data.frame(read.csv2(f5, sep = ","))

#processing data set by data set
names(d)
e <- d[, c(3,5,6,8)]

colnames(e) <- c("season","adm2","adm3","trial_id")

e1 <- d1[, c(3,4)]
colnames(e1) <- c("trial_id","previous_crop")

e2 <- d2[, c(3,5,6)]

#cleaning fetilizer types
d2$mineral_fert_type[d2$mineral_fert_type == "UREA"|d2$mineral_fert_type == "Ureia"|d2$mineral_fert_type == "ureia"]<- "Urea" 
d2$mineral_fert_type[d2$mineral_fert_type == "Ssp" |d2$mineral_fert_type == "SSp"]<- "SSP" 
e2$N_fertilizer[d2$mineral_fert_type == "Urea"]<- 40 #found by calculating elemental N in Urea applied to the plot size and converted to kg/ha
e2$P_fertilizer[d2$mineral_fert_type == "SSP" ]<- 40 #found by calculating elemental P in SSP applied to the plot size and converted to kg/ha
# e2$lime[d2$mineral_fert_type == "lime"] <- 100 will lime be added?

colnames(e2) <- c("trial_id","crop","variety","N_fertilizer","P_fertilizer")

e3 <- d3[, c(3,6,7)]

colnames(e3) <- c("trial_id","row_spacing","plant_spacing")

d4$trial_id <- d4$farm_id
d4$start_date <- as.Date(paste(d4$date_planting_yyyy, d4$date_planting_mm, d4$date_planting_dd, sep = "-") )
d4$end_date <- as.Date(paste(d4$date_harvest_yyyy,d4$date_harvest_mm,d4$date_harvest_dd, sep = "-"))

e4 <- d4[, c("trial_id","start_date","end_date")]

d5$trial_id <- d5$farm_id
d5$crop_1_weight_grain <- as.integer(d5$crop_1_weight_grain)
d5$yield <- 10000/d5$crop_1_area_harvested * d5$crop_1_weight_grain

e5 <- d5[, c("trial_id","yield")]

#merging the processed data sets
z <- carobiner::bindr(e,e1,e2,e3,e4,e5)

z$country <- "Mozambique"
z$latitude <- -18.66569
z$longitude <- 35.52956
z$on_farm <- "no"
z$is_survey <- "yes"

z$crop <- ifelse(z$crop %in% c("Groundnut",NA),"groundnut")# NA randomly assigned groundnuts
z$dataset_id <- dataset_id

# all scripts should end like this
carobiner::write_files(dset, z, path, dataset_id, group)
TRUE

}
