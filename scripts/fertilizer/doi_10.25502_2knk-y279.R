# R script for "carob"


carob_script <- function(path) {

"Description:

    N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility,
    improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable,
    long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings.
    A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.
    
    The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.

"

	uri <- "doi:10.25502/2knk-y279"
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
	
## no point in using fields with area <= zero
	r1 <- r1[r1$area > 0,]
	r <- merge(r1, r2, "farm_id")


##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()

	d <- data.frame(
		dataset_id = dataset_id,
		trial_id = r$farm_id, 
		on_farm = TRUE,
		is_survey = TRUE,
		irrigated = FALSE,
		country = trimws(r$country),
		site = carobiner::fix_name(r$village),
		adm1 = carobiner::fix_name(r$district_lga),
		elevation = r$gps_altitude
	)
	
##### Crop #####
	d$crop[r$type_legume_variety %in%
		c("Common beans", "Climbing Beans", "Bush Beans", "Sugarbeans")] <- "common bean"	
	d$crop[r$type_legume_variety %in% c("Groundnuts", "groundnuts")] <- "groundnut"
	d$crop[r$type_legume_variety == "Cowpeas"] <- "cowpea"
	d$crop[r$type_legume_variety %in% c("Soybeans", "Soyabeans")] <- "soybean"
	d$crop[r$type_legume_variety %in% c("Bambara nuts")] <- "bambara groundnut"

	d$variety_type <- NA
	d$variety_type[r$type_legume_variety == "Bush Beans"] <- "bush bean"
	d$variety_type[r$type_legume_variety == "Climbing Beans"] <- "climbing bean"

## EGB: Add intercropping
	d$intercrops <- NA	
	r$with_which_crops <- tolower(trimws(r$with_which_crops))
	d$intercrops[r$with_which_crops %in% c("cowpea and maize", "maize and cowpeas", "cowpea :maize")] <- "maize; cowpea"
	d$intercrops[r$with_which_crops == "maixe and sugar beans"] <- "maize; common bean"
	d$intercrops[r$with_which_crops == "groundnuts"] <- "groundnut"
	d$intercrops[r$with_which_crops == "cowpeas"] <- "cowpea"
	d$intercrops[r$with_which_crops == "maize"] <- "maize"

##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- "2011"
	d$harvest_date <- "2012"
	d$season <- trimws(r$season)
	

##### Fertilizers #####
## note that we use P and K, not P2O5 and K2O
	# for readability
	p <- tolower(trimws(r$synthetic_fert_types))
	k <- rep(NA, length(p))
	# Fertilizer type
	k[p %in% c("ssp, lime", "ssp and lime", "lime, ssp", "lime and ssp", "ssp. lime")] <- "SSP; lime"
	k[p %in% c("compound d", "compund d")] <- "D-compound"
	k[p %in% c("ammonium nitrate", "an", "amonium nitrate", "an applied in maize")] <- "AN"
	k[p %in% c("ammonium nitrate, compound d", "compound d, an", "compound d and amonium nitrate", "an and compund d", "compound d and an", "compound d, ammonium nitrate", "compound d, an in the maize crop")] <- "D-compound; AN"
	k[p %in% c("compound d and gypsum")] <- "D-compound; gypsum"
	k[p == c("ssp, gypsum")] <- "SSP; gypsum"
	k[p == "gypsum"] <- "gypsum"
	k[p == "ssp"] <- "SSP"
	k[p == "lime"] <- "lime"
	d$fertilizer_type <- k

	ftab <- carobiner::get_accepted_values("fertilizer_type", path)[, c("name", "N", "P", "K", "S")]
	ftab <- ftab[ftab$name %in% c("SSP", "D-compound", "AN"), ]
	fmat <- as.matrix(ftab[,-1]) / 100
	fr <- matrix(0, ncol=4, nrow=nrow(d))
	colnames(fr) <- c("N_fertilizer", "P_fertilizer", "K_fertilizer", "S_fertilizer")
	i <- grep("SSP", k)
	fr[i, ] <- rep(fmat[ftab$name=="SSP", ] , each=length(i))
	i <- grep("AN", k)
	fr[i, ] <- fr[i, ] + rep(fmat[ftab$name=="AN", ] , each=length(i))
	i <- grep("D-comp", k)
	fr[i, ] <- fr[i, ] + rep(fmat[ftab$name=="D-compound", ], each=length(i))

	famount <- 10000 * r$amount_fert_kg / r$area  # From kg/m2 to kg/ha
	d <- cbind(d, fr * famount)

	# # EGB: Lime and Gypsum can be captured, but the amount is not indicated
	# d$lime[grep("lime", d$fertilizer_type)] <- 99
	# d$gypsum[grep("gypsum", d$fertilizer_type)] <- 99
	
##RH I really want to capture lime. We should ask them about it 
	message("to do: lime & gypsum")

	# Treatment code	
	d$treatment <- paste0("N", round(d$N_fertilizer),
	                      "P", round(d$P_fertilizer),
	                      "K", round(d$K_fertilizer))
	
	d$OM_used <- r$organic_fert_applied == "Y"
	d$OM_used[r$organic_fert_applied == ""] <- NA

# # EGB: Add inoculation
	d$inoculated <- r$inoculant_applied == "Y"
   # # EGB: Inoculant is not specified. Only providers.
   
##### Yield #####
  # # EGB: No biomass
	d$yield <- 10000 * r$production_field_kg / r$area # From kg/m2 to kg/ha

#RH: I am under the impression that N2Africa measure peanut seed, not pods. 
	#what plant part does yield refer to?
	d$yield_part <- ifelse(d$crop == "peanut", "pod", "grain")


#RH I moved this to the bottom because "merge" changes the order of the data. 
# So that is a downside of the method that I prefer. I prefer to avoid subsetting at then end. 
# You can also add add "record_ID" before merge and then sort. 

## some of the coordinates refer to the site, others to the adm1. 
## it would be clearer, and less verbose, to separate that and do this in two steps 
## first for the cases where the site is used. Then for the cases where only the adm1 is used.
## then it is also clear where improvements would be most useful. 
## before using dput I would round to 3 decimals (that is already ~100 m precision)

## see below for georeferencing documentation (bc it is so much, which is great)
	sss <- data.frame(
	  country = "Zimbabwe", 
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
	
	# all.x=TRUE
	d <- merge(d, sss, by = c("country", "adm1", "site"), all.x=TRUE)

# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}



## Georeferencing documentation 
##
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
	# s$longitude[s$site == c("Gwangwara")] <- 31.935
	# s$latitude[s$site == c("Gwangwara")] <- -17.846
	# s$longitude[s$site == c("Chinamasa")] <- 32.581
	# s$latitude[s$site == c("Chinamasa")] <- -18.503
	# s$longitude[s$site == c("Gumbodete")] <- 26.862 # https://www.google.com/maps/place/Dete,+Zimbabwe
	# s$latitude[s$site == c("Gumbodete")] <- -18.620 # https://www.google.com/maps/place/Dete,+Zimbabwe
	# s$longitude[s$site %in% c("Nyarire", "Nyariri")] <- 32.019 # https://www.google.com/maps/place/Nyadire,+Zimbabwe
	# s$latitude[s$site %in% c("Nyarire", "Nyariri")] <- -17.425 # https://www.google.com/maps/place/Nyadire,+Zimbabwe
	# s$longitude[s$site == c("Manyonga")] <- 31.578 # https://www.google.com/maps/place/Manyongo
	# s$latitude[s$site == c("Manyonga")] <- -17.512 # https://www.google.com/maps/place/Manyongo
	# s$longitude[s$site == c("Dzvete")] <- 31.594 # https://www.google.com/maps/place/Dzvete
	# s$latitude[s$site == c("Dzvete")] <- -17.478 # https://www.google.com/maps/place/Dzvete
	# s$longitude[s$site == c("Tongogara")] <- 31.600 # https://www.google.com/maps/place/Tongogara,+Zimbabwe
	# s$latitude[s$site == c("Tongogara")] <- -16.940 # https://www.google.com/maps/place/Tongogara,+Zimbabwe
	# s$longitude[s$site == c("Munyawiri")] <- 31.136 # https://www.google.com/maps/place/Munyawiri
	# s$latitude[s$site == c("Munyawiri")] <- -17.509 # https://www.google.com/maps/place/Munyawiri
	# s$longitude[s$site == c("Chinamhora")] <- 31.223 # https://www.google.com/maps/place/Chinamhora,+Zimbabwe
	# s$latitude[s$site == c("Chinamhora")] <- -17.512 # https://www.google.com/maps/place/Chinamhora,+Zimbabwe
	# s$longitude[s$site == c("Nyanyiwa")] <- 30.857 # https://www.google.com/maps/place/Nyanyiwa
	# s$latitude[s$site == c("Nyanyiwa")] <- -18.340 # https://www.google.com/maps/place/Nyanyiwa
	# s$longitude[s$site == c("Muponda")] <- 32.721 # https://www.google.com/maps/place/Muponda+primary+School
	# s$latitude[s$site == c("Muponda")] <- -18.650 # https://www.google.com/maps/place/Muponda+primary+School
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

