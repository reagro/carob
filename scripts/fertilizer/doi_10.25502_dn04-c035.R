#################################################################################
#N2Africa was aimed at increasing biological nitrogen fixation and productivity
#of grain legumes through effective production technologies including inoculants
#and fertilizers adapted to local settings which was aimed at increasing soil
#fertility.The trails were conducted in 11 African countries
#################################################################################

carob_script <- function(path){

	uri <- "doi:10.25502/dn04-c035"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"

	dset <- data.frame(
		dataset_id = dataset_id,
		group = group,
		uri = uri,
		publication = NA,
		data_citation ="Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, P., Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira, L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller, K., Ampadu-Boakye, T., & Heerwaarden, J. van. (2020). N2Africa farm monitoring - Mozambique, 2011 - 2012, III [Data set]. International Institute of Tropical Agriculture (IITA). doi:10.25502/DN04-C035",
		carob_contributor = "Effie Ochieng",
		experiment_type = "variety_trials",
		has_weather =  FALSE,
		has_management = FALSE
	)

ff <- carobiner::get_data(uri,path,group)
js <- carobiner::get_metadata(dataset_id, path, group, major = 1, minor = 0)
dset$license <- carobiner::get_license(js)

#read the data
f <- ff[basename(ff) == "a_general.csv"]
d <- read.csv(f)
f1 <- ff[basename(ff)== "b_info_site_2.csv"]
d1 <- read.csv(f1)
f2 <- ff[basename(ff) =="c_use_of_package_2.csv" ]
d2 <- read.csv(f2)
f3 <- ff[basename(ff) == "d_cropping_calendar.csv"]
d3 <- read.csv(f3)
f4 <- ff[basename(ff) == "e_harvest.csv"]
d4 <- read.csv(f4)
 
#processing the first dataset
d$trial_id <- d$farm_id
d$adm2 <- carobiner::fix_name(d$district, "title")
d$adm3 <- carobiner::fix_name(d$sector_ward, "title")

d <- d[, c("trial_id", "adm2","adm3")]

#processing the 2nd dataset
d1$trial_id <- d1$farm_id
d1$crop_rotation <- trimws(tolower(d1$main_crop_last_season))

d1$crop_rotation[d1$crop_rotation == "maize, soybean"] <- "maize"
d1$crop_rotation[d1$crop_rotation %in% c("soya", "soja", "soya bean", "soybean")] <- "soybean"
d1$crop_rotation[d1$crop_rotation %in% c("ground nut", "ground nuts", "groundnuts")] <- "groundnut"
d1$crop_rotation[d1$crop_rotation == "beans"] <- "common bean"
d1$crop_rotation[d1$crop_rotation %in% c("", "no")] <- NA

d1 <- d1[, c("trial_id","crop_rotation")]

#processing the 3rd dataset
d2$trial_id <- d2$farm_id
d2$crop <- d2$crop_1
d2$variety <- d2$variety_1

d2$inoculated <- ifelse(d2$inoculant_used %in% c("Biagro", "Y", "Yes"), TRUE, 
					ifelse(d2$inoculant_used %in% c("N", "no"), FALSE, NA))


#cleaning fertilizer types
d2$mineral_fert_type <- tolower(d2$mineral_fert_type)
d2$mineral_fert_type[d2$mineral_fert_type %in% c("urea", "ureia", "ureia")]<- "urea"

i <- grep("ssp", d2$mineral_fert_type)
d2$mineral_fert_type[i] <- "SSP"

i <- grep("no", d2$mineral_fert_type)
d2$mineral_fert_type[i] <- "none"

i <- d2$mineral_fert_type %in% c("lime", "cal")
d2$mineral_fert_type <- "lime"

##FIXME##
## how much lime was applied?
#d2$lime <- 
## how much gypsum was applied?
#d2$gypsum <- 

# add organic fert
# organic_fert_type, organic_fert_amount

d2$fertilizer_type <- d2$mineral_fert_type

#SSP is P, not K!
d2$K_fertilizer <- 0
d2$P_fertilizer[d2$mineral_fert_type == "SSP"] <- 30
d2$N_fertilizer[d2$mineral_fert_type == "urea"] <- 60


d2 <- d2[, c("trial_id","crop","variety","inoculated","P_fertilizer","K_fertilizer","N_fertilizer","fertilizer_type")]

#processing the 4th dataset
d3$trial_id <- d3$farm_id
d3$start_date <- ifelse(d3$date_planting_yyyy == 0, NA, paste(d3$date_planting_yyyy, sprintf("%02d", d3$date_planting_mm), sprintf("%02d", d3$date_planting_dd), sep = "-"))
d3$end_date <- ifelse(d3$date_harvest_yyyy == 0, NA, paste(d3$date_harvest_yyyy, sprintf("%02d", d3$date_harvest_mm), sprintf("%02d", d3$date_harvest_dd), sep = "-"))

d3 <- d3[,c("trial_id","start_date","end_date")]

#cleaning the 5th dataset
d4$trial_id <- d4$farm_id
#changing the data types to numeric 
d4$crop_1_area_harvested <- as.numeric(d4$crop_1_area_harvested)
d4$crop_1_weight_grain <- as.numeric(d4$crop_1_weight_grain)

d4$yield <- (10000/d4$crop_1_area_harvested)*d4$crop_1_weight_grain 

d4 <- d4[, c("trial_id","yield")]

# EGB: This is the wrong use of carobiner::bindr!!
# # You don't want to append but rather join using the common IDs. For example:
q <- merge(d, d1, "trial_id")
q <- merge(q, d2, "trial_id")
q <- merge(q, d3, "trial_id")
q <- merge(q, d4, "trial_id")
# q <- carobiner::bindr(d,d1,d2,d3,d4)


q$country <- "Mozambique"

# that is not good enough
#q$latitude <- -18.66569
#q$longitude <- 	35.52956

#cleaning the crop variable, NA randomly filled with groundnut

## RH: we do not randomly fill in data 
## RH: if it is not clear what crop it is, the records should be removed as it has no value 
## RH: and it seems that there is no variety for these crops either! 
## RH: we could remove these records (after fix_name) with  q <- crop[!is.na(q$crop), ]
## RH: but surely we can find out which crop it should be. 
## RH: For example, "d3" has no crop or variety names at all. But that should be available somewhere?

#q$crop <- ifelse(q$crop %in% c("Soybean", "Soybean ","Soybean (SSP)","Soybean (INOC)", "Soybean (PD1)","Soybean (No-SSP)",
#                               "Soybean (No-INOC)","Soybean (PD2)"),"soybean","groundnut")

q$crop <- carobiner::fix_name(q$crop, "lower")
q$crop[grepl("soybean", q$crop)] <- "soybean"
# # EGB: Many rows with NA across. Removing them
# q <- q[complete.cases(q$crop),]

# this being a varietal trial, we should try to standardize these names
q$variety <- carobiner::fix_name(q$variety, "first")
#unique(q$variety) |> sort()
q$variety <- gsub("Santa -", "Santa ", q$variety)
q$variety <- gsub("Tgx", "TGX ", q$variety, ignore.case = TRUE)
q$variety <- gsub("TGX -", "TGX ", q$variety)
q$variety <- gsub("TGX  ", "TGX ", q$variety)
q$variety <- gsub("- ", "-", q$variety)
#unique(q$variety) |> sort()


q$dataset_id <- dataset_id

# all scripts should end like this
carobiner::write_files(dset, q, path=path)
}
