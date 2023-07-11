# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility,
    improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable,
    long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings.
    A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.
    
    The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.

"

	uri <- "https://doi.org/10.25502/2knk-y279"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project="N2Africa",
		uri=uri,
		data_citation="Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, P., Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira, L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller, K., Ampadu-Boakye, T., & Heerwaarden, J. van. (2020). N2Africa use survey - Zimbabwe, 2012 [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/2KNK-Y279",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= NA,
		data_institutions = "IITA",
   	data_type = "survey", # or, e.g. "on-farm experiment", "survey", "compilation"
		carob_contributor = "Eduardo Garcia Bendito"  
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	dset$license <- carobiner::get_license(js)


	f1 <- ff[basename(ff) == "legumes_area_and_management.csv"]
	f2 <- ff[basename(ff) == "general.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	
	
## process file(s)

## use a subset
	d1 <- carobiner::change_names(r1, colnames(r1), colnames(r1))
	d1 <- d1[d1$area > 0,]
	d2 <- carobiner::change_names(r2, colnames(r2), colnames(r2))
	d <- merge(d2,d1,"farm_id")

	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- as.logical(TRUE)
	d$is_survey <- as.logical(TRUE)
	d$irrigated <- as.logical(FALSE)

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- trimws(d$country)
	d$site <- trimws(d$village)
	d$adm1 <- trimws(d$district_lga)
	d$elevation <- d$gps_altitude
## each site must have corresponding longitude and latitude

	# # EGB: Extracting spatial coordinates:
	
	# s <- unique(d[nchar(d$adm1) > 1,c("country", "adm1", "site")])
	# s$latitude <- NA
	# s$longitude <- NA
	# s$longitude[s$site == "Chinamaringa"] <- 31.184
	# s$latitude[s$site == "Chinamaringa"] <- -17.761
	# s$longitude[s$site == c("Muzvuwe", "Muzuwe", "Muzuva")] <- 30.976
	# s$latitude[s$site == c("Muzvuwe", "Muzuwe", "Muzuva")] <- -17.507
	# s$longitude[s$site %in% c("Mutuugwazi", "Mutuungwazi", "Mutungwazi", "Mutugwasi", "Mutugwazi", "Mutungwasi")] <- 31.301
	# s$latitude[s$site %in% c("Mutuugwazi", "Mutuungwazi", "Mutungwazi", "Mutugwasi", "Mutugwazi", "Mutungwasi")] <- -17.531
	# s$longitude[s$site %in% c("Gwangwara")] <- 31.935
	# s$latitude[s$site %in% c("Gwangwara")] <- -17.846
	# s$longitude[s$site %in% c("Chinamasa")] <- 32.581
	# s$latitude[s$site %in% c("Chinamasa")] <- -18.503
	# s$longitude[s$site %in% c("Gumbodete")] <- 26.862 # https://www.google.com/maps/place/Dete,+Zimbabwe
	# s$latitude[s$site %in% c("Gumbodete")] <- -18.620 # https://www.google.com/maps/place/Dete,+Zimbabwe
	# s$longitude[s$site %in% c("Nyarire", "Nyariri")] <- 32.019 # https://www.google.com/maps/place/Nyadire,+Zimbabwe
	# s$latitude[s$site %in% c("Nyarire", "Nyariri")] <- -17.425 # https://www.google.com/maps/place/Nyadire,+Zimbabwe
	# s$longitude[s$site %in% c("Manyonga")] <- 31.578 # https://www.google.com/maps/place/Manyongo
	# s$latitude[s$site %in% c("Manyonga")] <- -17.512 # https://www.google.com/maps/place/Manyongo
	# s$longitude[s$site %in% c("Dzvete")] <- 31.594 # https://www.google.com/maps/place/Dzvete
	# s$latitude[s$site %in% c("Dzvete")] <- -17.478 # https://www.google.com/maps/place/Dzvete
	# s$longitude[s$site %in% c("Tongogara")] <- 31.600 # https://www.google.com/maps/place/Tongogara,+Zimbabwe
	# s$latitude[s$site %in% c("Tongogara")] <- -16.940 # https://www.google.com/maps/place/Tongogara,+Zimbabwe
	# s$longitude[s$site %in% c("Munyawiri")] <- 31.136 # https://www.google.com/maps/place/Munyawiri
	# s$latitude[s$site %in% c("Munyawiri")] <- -17.509 # https://www.google.com/maps/place/Munyawiri
	# s$longitude[s$site %in% c("Chinamhora")] <- 31.223 # https://www.google.com/maps/place/Chinamhora,+Zimbabwe
	# s$latitude[s$site %in% c("Chinamhora")] <- -17.512 # https://www.google.com/maps/place/Chinamhora,+Zimbabwe
	# s$longitude[s$site %in% c("Nyanyiwa")] <- 30.857 # https://www.google.com/maps/place/Nyanyiwa
	# s$latitude[s$site %in% c("Nyanyiwa")] <- -18.340 # https://www.google.com/maps/place/Nyanyiwa
	# s$longitude[s$site %in% c("Muponda")] <- 32.721 # https://www.google.com/maps/place/Muponda+primary+School
	# s$latitude[s$site %in% c("Muponda")] <- -18.650 # https://www.google.com/maps/place/Muponda+primary+School
	# s$adm1[s$adm1 == "Chegut"] <- "Chegutu"
	# s$adm1[s$adm1 == "Makone"] <- "Makoni"
	# s$adm1[s$adm1 == "Gormonzi"] <- "Goromonzi"
	# for (i in 1:nrow(s)) {
	#   if(is.na(s$latitude[i]) | is.na(s$longitude[i])){
	#     ll <- carobiner::geocode(country = s$country[i], adm1 = s$adm1[i], location = s$site[i], service = "geonames", username = "efyrouwa")
	#     ii <- unlist(jsonlite::fromJSON(ll))
	#     c <- as.integer(ii["totalResultsCount"][[1]])
	#     s$latitude[i] <- as.numeric(ifelse(c == 1, ii["geonames.lat"][1], ii["geonames.lat1"][1]))
	#     s$longitude[i] <- as.numeric(ifelse(c == 1, ii["geonames.lng"][1], ii["geonames.lng1"][1]))
	#   }
	# }
	# ss <- unique(s[nchar(s$adm1) > 1 & is.na(s$latitude), c("country", "adm1", "site")])
	# ss$adm1[ss$adm1 == "Chegut"] <- "Chegutu"
	# ss$adm1[ss$adm1 == "Makone"] <- "Makoni"
	# ss$adm1[ss$adm1 == "Gormonzi"] <- "Goromonzi"
	# for (i in 1:nrow(ss)) {
	#   ll <- carobiner::geocode(country = ss$country[i], adm1 = ss$adm1[i], location = ss$adm1[i], service = "geonames", username = "efyrouwa")
	#   ii <- unlist(jsonlite::fromJSON(ll))
	#   c <- as.integer(ii["totalResultsCount"][[1]])
	#   ss$latitude[i] <- as.numeric(ifelse(c == 1, ii["geonames.lat"][1], ii["geonames.lat1"][1]))
	#   ss$longitude[i] <- as.numeric(ifelse(c == 1, ii["geonames.lng"][1], ii["geonames.lng1"][1]))
	# }
	# sss <- s
	# for (i in 1:nrow(sss)) {
	#   if(is.na(sss$latitude[i]) & is.na(sss$longitude[i])){
	#     sss$latitude[i] <- ss$latitude[ss$country == sss$country[i] & ss$adm1 == sss$adm1[i] & ss$site == sss$site[i]][1]
	#     sss$longitude[i] <- ss$longitude[ss$country == sss$country[i] & ss$adm1 == sss$adm1[i] & ss$site == sss$site[i]][1]
	#   }
	# }
	# sss <- dput(sss)
	
	# # Formatting coordinates into the dataset
	sss <- data.frame(
	  country = c("Zimbabwe", "Zimbabwe", "Zimbabwe", 
	            "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", 
	            "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", 
	            "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", 
	            "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", 
	            "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", 
	            "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", 
	            "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", 
	            "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", 
	            "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", 
	            "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", 
	            "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", 
	            "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", 
	            "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", 
	            "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", 
	            "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", 
	            "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", 
	            "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe"),
	  adm1 = c("Murehwa", "Murehwa", "Murehwa", "Murehwa", "Murehwa", "Murehwa", "Murehwa", 
	           "Murehwa", "Chegutu", "Chegutu", "Chegutu", "Chegutu", "Chegutu", 
	           "Chegutu", "Chegutu", "Chegutu", "Chegutu", "Chegutu", "Chegutu", 
	           "Chegutu", "Chegutu", "Chegutu", "Makoni", "Makoni", "Makoni", 
	           "Makoni", "Makoni", "Makoni", "Makoni", "Makoni", "Makoni", "Goromonzi", 
	           "Makoni", "Makoni", "Makoni", "Makoni", "Makoni", "Makoni", "Makoni", 
	           "Makoni", "Makoni", "Makoni", "Makoni", "Makoni", "Makoni", "Goromonzi", 
	           "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", 
	           "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", 
	           "Goromonzi", "Goromonzi", "Makoni", "Makoni", "Makoni", "Makoni", 
	           "Murehwa", "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", 
	           "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", 
	           "Goromonzi", "Chegutu", "Chegutu", "Chegutu", "Goromonzi", "Goromonzi", 
	           "Goromonzi", "Chegutu", "Chegutu", "Chegutu", "Chegutu", "Goromonzi", 
	           "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", 
	           "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", 
	           "Murewa", "Goromonzi", "Goromonzi", "Murewa", "Murewa", "Murehwa", 
	           "Chegutu", "Chegutu", "Chegutu"),
	  site = c("Zhakata", "Mapanga", "Soka", "Ngundu", "Chikwati", "Chkwati", "Chikati", "Chikwete", 
	           "Mapfumo", "Mlubi", "Mhlubi", "Kujokera", "Madzongwe", "Muzavazi", 
	           "Chinamaringa", "Marufu", "Muponde", "Mlubi", "Paradzai", "Mupondi", 
	           "Kakono", "Zenzile", "Mutowa", "Fungayi", "Handina", "Maravanyika", 
	           "Mataranyika", "Makaure", "Zunidza", "Mapfumo", "Muzvuwe", "Molife", 
	           "Mutuugwazi", "Mutungwazi", "Rukweza", "Gwangwara", "Chinamasa", 
	           "Chamunorwa", "Mutugwasi", "Muzuwe", "Dhuwa", "Mudhowa", "Mutugwazi", 
	           "Handina", "Mutungwasi", "Mapuranga", "Gwamura", "Ngoshi", "Rusike", 
	           "Magadzo", "Marufu", "Kachuta", "Chawengwa", "Marimo", "Chginji", 
	           "Mavaza", "Kahari", "Gukwe", "Masedza", "Jana", "Mandizha", "Dhuva", 
	           "Suka", "Chishiri", "Choga", "Bungu", "Mutyawasara", "Chinamasa", 
	           "Chiuta", "Chiuta", "Gumbodete", "Masawi", "Mangwaza", "Nyarire", 
	           "Mushingaidzwa", "Mapira", "Manyonga", "Dzvete", "Gwati", "Nyariri", 
	           "Kawara", "Mabhiza", "Chimbambaira", "Mberi", "Tendenguwo", "Tongogara", 
	           "Muzuva", "Dewu", "Dhewu", "Magudzo", "Taziva", "Murungweni", 
	           "Munyawiri", "14", "Mapanga", "Chinamhora", "Chirima", "Ngundu", 
	           "Zhakata", "Nyanyiwa", "Magwaza", "Mandebvu", "Muponda"),
	  latitude = c(-17.80057, -17.80057, -17.80057, -17.80057, -17.80057, -17.80057, -17.80057, 
	               -17.80057, -18.13021, -18.13021, -18.13021, -18.13021, -18.13021, 
	               -18.13021, -17.761, -18.13021, -18.13021, -18.13021, -18.13021, 
	               -18.13021, -18.13021, -18.13021, -18.3355, -18.3355, -18.3355, 
	               -18.3355, -18.3355, -18.3355, -18.3355, -18.3355, -17.507, -17.80695, 
	               -17.531, -17.531, -18.3355, -17.846, -18.503, -18.3355, -17.531, 
	               -18.3355, -18.3355, -18.3355, -17.531, -18.3355, -17.531, -17.80695, 
	               -17.80695, -17.80695, -17.80695, -17.80695, -17.80695, -17.80695, 
	               -17.80695, -17.80695, -17.80695, -17.80695, -17.80695, -17.80695, 
	               -18.3355, -18.3355, -18.3355, -18.3355, -17.80057, -17.80695, 
	               -17.80695, -17.80695, -17.80695, -18.503, -17.80695, -17.80695, 
	               -18.62, -17.80695, -17.80695, -17.425, -18.13021, -18.13021, 
	               -17.512, -17.478, -17.80695, -17.425, -18.13021, -18.13021, -18.13021, 
	               -17.80695, -17.80695, -16.94, -17.507, -17.80695, -17.80695, 
	               -17.80695, -17.80695, -17.80695, -17.509, -17.80695, -17.80057, 
	               -17.512, -17.80695, -17.80057, -17.80057, -18.34, -18.13021, 
	               -18.13021, -18.65),
	  longitude = c(31.83083, 31.83083, 31.83083, 31.83083, 31.83083, 31.83083, 31.83083, 31.83083, 30.14074, 30.14074, 
	                30.14074, 30.14074, 30.14074, 30.14074, 31.184, 30.14074, 30.14074, 
	                30.14074, 30.14074, 30.14074, 30.14074, 30.14074, 32.1465, 32.1465, 
	                32.1465, 32.1465, 32.1465, 32.1465, 32.1465, 32.1465, 30.976, 
	                31.36372, 31.301, 31.301, 32.1465, 31.935, 32.581, 32.1465, 31.301, 
	                32.1465, 32.1465, 32.1465, 31.301, 32.1465, 31.301, 31.36372, 
	                31.36372, 31.36372, 31.36372, 31.36372, 31.36372, 31.36372, 31.36372, 
	                31.36372, 31.36372, 31.36372, 31.36372, 31.36372, 32.1465, 32.1465, 
	                32.1465, 32.1465, 31.83083, 31.36372, 31.36372, 31.36372, 31.36372, 
	                32.581, 31.36372, 31.36372, 26.862, 31.36372, 31.36372, 32.019, 
	                30.14074, 30.14074, 31.578, 31.594, 31.36372, 32.019, 30.14074, 
	                30.14074, 30.14074, 31.36372, 31.36372, 31.6, 30.976, 31.36372, 
	                31.36372, 31.36372, 31.36372, 31.36372, 31.136, 31.36372, 31.83083, 
	                31.223, 31.36372, 31.83083, 31.83083, 30.857, 30.14074, 30.14074, 
	                32.721))
	
	d <- merge(d, sss, by = c("country", "adm1", "site"))
	
##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- NA
	d$crop[d$type_legume_variety %in% c("Common beans", "Climbing Beans", "Bush Beans", "Sugarbeans")] <- "common bean"
	d$crop[d$type_legume_variety %in% c("Groundnuts", "groundnuts")] <- "groundnut"
	d$crop[d$type_legume_variety %in% c("Cowpeas")] <- "cowpea"
	d$crop[d$type_legume_variety %in% c("Soybeans", "Soyabeans")] <- "soybean"
	d$crop[d$type_legume_variety %in% c("Bambara nuts")] <- "bambara groundnut"

# # EGB: Add intercropping
	d$inoculated <- as.logical(FALSE)
	d$inoculated[d$inoculant_applied == "Y"] <- as.logical(TRUE)
	d$intercrops <- NA
	d$intercrops[tolower(trimws(d$with_which_crops)) %in% c("cowpea and maize", "maize and cowpeas", "cowpea :maize")] <- "maize; cowpea"
	d$intercrops[tolower(trimws(d$with_which_crops)) %in% c("maixe and sugar beans")] <- "maize; common bean"
	d$intercrops[tolower(trimws(d$with_which_crops)) %in% c("groundnuts")] <- "groundnut"
	d$intercrops[tolower(trimws(d$with_which_crops)) %in% c("cowpeas")] <- "cowpea"
	d$intercrops[tolower(trimws(d$with_which_crops)) %in% c("maize")] <- "maize"

##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	d$start_date <- as.character(format(as.Date("2011-01-01"), "%Y"))
	d$end_date  <- as.character(format(as.Date("2012-01-01"), "%Y"))
	d$season <- as.character(d$season)
	

##### Fertilizers #####
## note that we use P and K, not P2O5 and K2O
	d$fert <- (d$amount_fert_kg)/((d$area)/10000) # From kg/m2 to kg/ha
	d$synthetic_fert_types <- tolower(trimws(d$synthetic_fert_types))
	# Fertilizer type
	d$fertilizer_type <- NA
	d$fertilizer_type[d$synthetic_fert_types %in% c("ssp, lime", "ssp and lime", "lime, ssp", "lime and ssp", "ssp. lime")] <- "SSP; lime"
	d$fertilizer_type[d$synthetic_fert_types %in% c("compound d", "compund d")] <- "D-compound"
	d$fertilizer_type[d$synthetic_fert_types %in% c("ammonium nitrate", "an", "amonium nitrate", "an applied in maize")] <- "AN"
	d$fertilizer_type[d$synthetic_fert_types %in% c("ammonium nitrate, compound d", "compound d, an", "compound d and amonium nitrate", "an and compund d", "compound d and an", "compound d, ammonium nitrate", "compound d, an in the maize crop")] <- "D-compound; AN"
	d$fertilizer_type[d$synthetic_fert_types %in% c("compound d and gypsum")] <- "D-compound; gypsum"
	d$fertilizer_type[d$synthetic_fert_types %in% c("ssp, gypsum")] <- "SSP; gypsum"
	d$fertilizer_type[d$synthetic_fert_types %in% c("gypsum")] <- "gypsum"
	d$fertilizer_type[d$synthetic_fert_types %in% c("ssp")] <- "SSP"
	d$fertilizer_type[d$synthetic_fert_types %in% c("lime")] <- "lime"
	# Fertilizer amounts
	d$N_fertilizer <- 0
	d$N_fertilizer[d$fertilizer_type %in% c("D-compound", "D-compound; gypsum")] <- d$fert[d$fertilizer_type %in% c("D-compound", "D-compound; gypsum")] * 0.1
	d$N_fertilizer[d$fertilizer_type %in% c("AN")] <- d$fert[d$fertilizer_type %in% c("AN")] * 0.34
	d$N_fertilizer[d$fertilizer_type %in% c("D-compound; AN")] <- d$fert[d$fertilizer_type %in% c("D-compound; AN")] * 0.2
	d$P_fertilizer <- 0
	d$P_fertilizer[d$fertilizer_type %in% c("SSP", "SSP; gypsum", "SSP; lime")] <- d$fert[d$fertilizer_type %in% c("SSP", "SSP; gypsum", "SSP; lime")] * 0.0874
	d$P_fertilizer[d$fertilizer_type %in% c("D-compound", "D-compound; gypsum", "D-compound; AN")] <- d$fert[d$fertilizer_type %in% c("D-compound", "D-compound; gypsum", "D-compound; AN")] * 0.2
	d$K_fertilizer <- 0
	d$K_fertilizer[d$fertilizer_type %in% c("D-compound", "D-compound; gypsum", "D-compound; AN")] <- d$fert[d$fertilizer_type %in% c("D-compound", "D-compound; gypsum", "D-compound; AN")] * 0.1
	d$S_fertilizer <- 0
	d$S_fertilizer[d$fertilizer_type %in% c("D-compound", "D-compound; AN")] <- d$fert[d$fertilizer_type %in% c("D-compound", "D-compound; AN")] * 0.09
	d$S_fertilizer[d$fertilizer_type %in% c("D-compound; gypsum")] <- d$fert[d$fertilizer_type %in% c("D-compound; gypsum")] * 0.135
	d$Ca_fertilizer <- 0
	d$Ca_fertilizer[d$fertilizer_type %in% c("lime", "SSP; lime")] <- d$fert[d$fertilizer_type %in% c("lime", "SSP; lime")] * 0.35
	d$Ca_fertilizer[d$fertilizer_type %in% c("gypsum", "SSP; gypsum", "D-compound; gypsum")] <- d$fert[d$fertilizer_type %in% c("gypsum", "SSP; gypsum", "D-compound; gypsum")] * 0.23
	# # EGB: Lime and Gypsum can be captured, but the amount is not indicated
	# d$lime[d$fertilizer_type %in% c("lime", "ssp, lime", "ssp and lime", "lime, ssp", "lime and ssp", "ssp. lime")] <- d$fert
	# d$gypsum[d$fertilizer_type %in% c("gypsum", "ssp, gypsum", "compound d and gypsum")] <- d$fert

	# Treatment code	
	d$treatment <- paste0("N", as.character(ifelse(!is.na(d$N_fertilizer), round(d$N_fertilizer,0), 0)),
	                      "P", as.character(ifelse(!is.na(d$P_fertilizer), round(d$P_fertilizer,0), 0)),
	                      "K", as.character(ifelse(!is.na(d$K_fertilizer), round(d$K_fertilizer,0), 0)))
	
	d$OM_used <- NA
	d$OM_used[d$organic_fert_applied == "Y"] <- as.logical(TRUE)
	d$OM_used[trimws(d$organic_fert_applied) == "N"] <- as.logical(FALSE)
# # EGB: Add inoculation
   d$inoculated <- as.logical(FALSE)
   d$inoculated[d$inoculant_applied == "Y"] <- as.logical(TRUE)
   # # EGB: Inoculant is not specified. Only providers.
   # d$inoculant <- NA
   # d$inoculant[isTRUE(d$inoculated)] <- as.character(d$inoculant_source)
   
##### Yield #####
  # # EGB: No biomass
	# d$biomass_total <- 

  d$yield <- (d$production_field_kg)/((d$area)/10000) # From kg/m2 to kg/ha
	#what plant part does yield refer to?
	d$yield_part <- "pod"

	# # EGB: subsetting dataset	
	dd <- d[, c("dataset_id", "on_farm", "is_survey",
	           "country", "adm1", "site", "latitude", "longitude", "elevation",
	           "crop", "intercrops", "treatment",
	           "fertilizer_type", "N_fertilizer", "P_fertilizer", "K_fertilizer", "S_fertilizer", "Ca_fertilizer", "OM_used",
	           "yield", "yield_part",
	           "irrigated", "inoculated")]

# all scripts must end like this
	carobiner::write_files(dset, dd, path=path)
}
