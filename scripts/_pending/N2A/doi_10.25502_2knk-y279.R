# R script for "carob"


carob_script <- function(path) {

"

    N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility,
    improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable,
    long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings.
    A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.
    
    The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.

"

	uri <- "doi:10.25502/2knk-y279"
	group <- "fertilizer"
	ff <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		carobiner::get_metadata(uri, path, group, major=1, minor=0),
		project="N2Africa",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= NA,
		data_institute = "IITA",
		data_type = "survey", # or, e.g. "on-farm experiment", "survey", "compilation"
		carob_contributor = "Eduardo Garcia Bendito",
		carob_date="2023-07-11"
	)



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

	ftab <- carobiner::accepted_values("fertilizer_type")
	get_elements <- carobiner::get_function("get_elements_from_product", path, group)
	elements <- get_elements(ftab, k)

## is this correct for "D-compound; AN"? That is, the below (and above) assumes that 
## r$amount_fert_kg was applied of D-compound AND the same amount of AN. Is that correct?

## RH there are many cases with a very high fertilizer amount. We 
## could remove these records, but it would be better to carefully review what is going on


	famount <- 10000 * r$amount_fert_kg / r$area  # From kg/m2 to kg/ha
	d <- cbind(d, elements * famount)
    message("review: fertilizers, many NAs")

	# # EGB: Lime and Gypsum can be captured, but the amount is not indicated
	# d$lime[grep("lime", d$fertilizer_type)] <- 99
	# d$gypsum[grep("gypsum", d$fertilizer_type)] <- 99
	
##RH I really want to capture lime. We should ask them about it 
	message("to do: lime & gypsum")

	# Treatment code	
	d$treatment <- paste0("N", ifelse(!is.na(d$N_fertilizer), round(d$N_fertilizer), 0),
	                      "P", ifelse(!is.na(d$P_fertilizer), round(d$P_fertilizer), 0),
	                      "K", ifelse(!is.na(d$K_fertilizer), round(d$K_fertilizer), 0))
	
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
    # EGB: Fixed
	# d$yield_part <- ifelse(d$crop == "peanut", "pod", "grain")
    d$yield_part <- "grain"


#RH I moved this to the bottom because "merge" changes the order of the data. 
# So that is a downside of the method that I prefer. I prefer to avoid subsetting at then end. 
# You can also add add "record_ID" before merge and then sort. 

## some of the coordinates refer to the site, others to the adm1. 
## it would be clearer, and less verbose, to separate that and do this in two steps 
## first for the cases where the site is used. Then for the cases where only the adm1 is used.
## then it is also clear where improvements would be most useful. 
## before using dput I would round to 3 decimals (that is already ~100 m precision)

## see below for georeferencing documentation (bc it is so much, which is great)

  # 1st sites
  s <- data.frame(country = c("Zimbabwe", "Zimbabwe", "Zimbabwe",
                              "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe",
                              "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe",
                              "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe"),
                  adm1 = c("Chegutu", "Chegutu", "Chegutu", "Chegutu", "Goromonzi",
                           "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi",
                           "Goromonzi", "Makoni", "Makoni", "Makoni", "Makoni", "Makoni",
                           "Makoni", "Makoni", "Makoni", "Murehwa"),
                  site = c("Chinamaringa","Muponda", "Nyarire", "Nyariri", "Chinamasa", "Chinamhora",
                           "Dzvete", "Gumbodete", "Manyonga", "Munyawiri", "Tongogara",
                           "Chinamasa", "Gwangwara", "Mutugwasi", "Mutugwazi", "Mutungwasi",
                           "Mutungwazi", "Mutuugwazi", "Muzvuwe", "Nyanyiwa"),
                  latitude = c(-17.761, -18.65, -17.425, -17.425, -18.503, -17.512, -17.478, -18.62,
                               -17.512, -17.509, -16.94, -18.503, -17.846, -17.531, -17.531,
                               -17.531, -17.531, -17.531, -17.507, -18.34),
                  longitude = c(31.184, 32.721, 32.019, 32.019, 32.581, 31.223, 31.594, 26.862, 31.578,
                                31.136, 31.6, 32.581, 31.935, 31.301, 31.301, 31.301, 31.301,
                                31.301, 30.976, 30.857))
  d2 <- merge(d, s, by = c("country", "adm1", "site"), all.x=TRUE)
  d2 <- d2[!is.na(d2$longitude),]
  # 2nd ADM units
  s <- data.frame(country = c("Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe",
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
                              "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe"), 
                  adm1 = c("Chegutu", "Chegutu", "Chegutu", "Chegutu", "Chegutu", 
                           "Chegutu", "Chegutu", "Chegutu", "Chegutu", "Chegutu", "Chegutu", 
                           "Chegutu", "Chegutu", "Chegutu", "Chegutu", "Chegutu", "Chegutu", 
                           "Chegutu", "Chegutu", "Goromonzi", "Goromonzi", "Goromonzi", 
                           "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", 
                           "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", 
                           "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", 
                           "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", 
                           "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", 
                           "Goromonzi", "Goromonzi", "Goromonzi", "Goromonzi", "Makoni", 
                           "Makoni", "Makoni", "Makoni", "Makoni", "Makoni", "Makoni", 
                           "Makoni", "Makoni", "Makoni", "Makoni", "Makoni", "Makoni", 
                           "Makoni", "Makoni", "Makoni", "Makoni", "Murehwa", "Murehwa", 
                           "Murehwa", "Murehwa", "Murehwa", "Murehwa", "Murehwa", "Murehwa", 
                           "Murehwa", "Murewa", "Murewa", "Murewa"),
                  site = c("Mlubi", "Chimbambaira", "Kakono", "Kawara", "Kujokera", "Mabhiza", 
                           "Madzongwe", "Magwaza", "Mandebvu", "Mapfumo", "Mapira", 
                           "Marufu", "Mhlubi", "Muponde", "Mupondi", "Mushingaidzwa", 
                           "Muzavazi", "Paradzai", "Zenzile", "Chiuta", "14", "Bungu", 
                           "Chawengwa", "Chginji", "Chirima", "Chishiri", "Choga", "Dewu", "Dhewu",
                           "Gukwe", "Gwamura", "Gwati", "Kachuta", "Kahari", 
                           "Magadzo", "Magudzo", "Mangwaza", "Mapuranga", "Marimo", 
                           "Marufu", "Masawi", "Mavaza", "Mberi", "Molife", "Murungweni", 
                           "Mutyawasara", "Muzuva", "Ngoshi", "Rusike", "Taziva", "Tendenguwo", 
                           "Handina", "Chamunorwa", "Dhuva", "Dhuwa", "Fungayi", "Jana", 
                           "Makaure", "Mandizha", "Mapfumo", "Maravanyika", "Masedza", 
                           "Mataranyika", "Mudhowa", "Mutowa", "Muzuwe", "Rukweza", 
                           "Zunidza", "Chikati", "Chikwati", "Chikwete", "Chkwati", 
                           "Mapanga", "Ngundu", "Soka", "Suka", "Zhakata", "Mapanga", "Ngundu", "Zhakata"),
                  latitude = c(-18.13021, -18.13021, -18.13021, -18.13021, -18.13021, -18.13021, -18.13021, -18.13021, 
                               -18.13021, -18.13021, -18.13021, -18.13021, -18.13021, -18.13021, 
                               -18.13021, -18.13021, -18.13021, -18.13021, -18.13021, -17.80695, 
                               -17.80695, -17.80695, -17.80695, -17.80695, -17.80695, -17.80695, 
                               -17.80695, -17.80695, -17.80695, -17.80695, -17.80695, -17.80695, 
                               -17.80695, -17.80695, -17.80695, -17.80695, -17.80695, -17.80695, 
                               -17.80695, -17.80695, -17.80695, -17.80695, -17.80695, -17.80695, 
                               -17.80695, -17.80695, -17.80695, -17.80695, -17.80695, -17.80695, 
                               -17.80695, -18.3355, -18.3355, -18.3355, -18.3355, -18.3355, 
                               -18.3355, -18.3355, -18.3355, -18.3355, -18.3355, -18.3355, 
                               -18.3355, -18.3355, -18.3355, -18.3355, -18.3355, -18.3355, 
                               -17.80057, -17.80057, -17.80057, -17.80057, -17.80057, -17.80057, 
                               -17.80057, -17.80057, -17.80057, -17.80057, -17.80057, -17.80057),
                  longitude = c(30.14074, 30.14074, 30.14074, 30.14074, 30.14074, 30.14074, 30.14074, 30.14074, 30.14074, 30.14074, 
                                30.14074, 30.14074, 30.14074, 30.14074, 30.14074, 30.14074, 
                                30.14074, 30.14074, 30.14074, 31.36372, 31.36372, 31.36372, 
                                31.36372, 31.36372, 31.36372, 31.36372, 31.36372, 31.36372, 
                                31.36372, 31.36372, 31.36372, 31.36372, 31.36372, 31.36372, 
                                31.36372, 31.36372, 31.36372, 31.36372, 31.36372, 31.36372, 
                                31.36372, 31.36372, 31.36372, 31.36372, 31.36372, 31.36372, 
                                31.36372, 31.36372, 31.36372, 31.36372, 31.36372, 32.1465, 
                                32.1465, 32.1465, 32.1465, 32.1465, 32.1465, 32.1465, 32.1465, 
                                32.1465, 32.1465, 32.1465, 32.1465, 32.1465, 32.1465, 32.1465, 
                                32.1465, 32.1465, 31.83083, 31.83083, 31.83083, 31.83083, 
                                31.83083, 31.83083, 31.83083, 31.83083, 31.83083, 31.83083, 31.83083, 31.83083))
  d3 <- merge(d, s, by = c("country", "adm1", "site"), all.x=TRUE)
  d3 <- d3[!is.na(d3$longitude),]
  # Appending both tables with coordinates
  d <- carobiner::bindr(d2,d3)

	carobiner::write_files(meta, d, path=path)
}



## Georeferencing documentation 
##
## each site must have corresponding longitude and latitude
	# # EGB: Extracting spatial coordinates:
	
# 	s <- unique(d[nchar(d$adm1) > 1 & !is.na(d$country),c("country", "adm1", "site")])
#     s <- s[!is.na(s$country),]
# 	s$latitude <- NA
# 	s$longitude <- NA
# 	s$longitude[s$site == "Chinamaringa"] <- 31.184
# 	s$latitude[s$site == "Chinamaringa"] <- -17.761
# 	s$longitude[s$site == c("Muzvuwe", "Muzuwe", "Muzuva")] <- 30.976
# 	s$latitude[s$site == c("Muzvuwe", "Muzuwe", "Muzuva")] <- -17.507
# 	s$longitude[s$site %in% c("Mutuugwazi", "Mutuungwazi", "Mutungwazi", "Mutugwasi", "Mutugwazi", "Mutungwasi")] <- 31.301
# 	s$latitude[s$site %in% c("Mutuugwazi", "Mutuungwazi", "Mutungwazi", "Mutugwasi", "Mutugwazi", "Mutungwasi")] <- -17.531
# 	s$longitude[s$site == c("Gwangwara")] <- 31.935
# 	s$latitude[s$site == c("Gwangwara")] <- -17.846
# 	s$longitude[s$site == c("Chinamasa")] <- 32.581
# 	s$latitude[s$site == c("Chinamasa")] <- -18.503
# 	s$longitude[s$site == c("Gumbodete")] <- 26.862 # https://www.google.com/maps/place/Dete,+Zimbabwe
# 	s$latitude[s$site == c("Gumbodete")] <- -18.620 # https://www.google.com/maps/place/Dete,+Zimbabwe
# 	s$longitude[s$site %in% c("Nyarire", "Nyariri")] <- 32.019 # https://www.google.com/maps/place/Nyadire,+Zimbabwe
# 	s$latitude[s$site %in% c("Nyarire", "Nyariri")] <- -17.425 # https://www.google.com/maps/place/Nyadire,+Zimbabwe
# 	s$longitude[s$site == c("Manyonga")] <- 31.578 # https://www.google.com/maps/place/Manyongo
# 	s$latitude[s$site == c("Manyonga")] <- -17.512 # https://www.google.com/maps/place/Manyongo
# 	s$longitude[s$site == c("Dzvete")] <- 31.594 # https://www.google.com/maps/place/Dzvete
# 	s$latitude[s$site == c("Dzvete")] <- -17.478 # https://www.google.com/maps/place/Dzvete
# 	s$longitude[s$site == c("Tongogara")] <- 31.600 # https://www.google.com/maps/place/Tongogara,+Zimbabwe
# 	s$latitude[s$site == c("Tongogara")] <- -16.940 # https://www.google.com/maps/place/Tongogara,+Zimbabwe
# 	s$longitude[s$site == c("Munyawiri")] <- 31.136 # https://www.google.com/maps/place/Munyawiri
# 	s$latitude[s$site == c("Munyawiri")] <- -17.509 # https://www.google.com/maps/place/Munyawiri
# 	s$longitude[s$site == c("Chinamhora")] <- 31.223 # https://www.google.com/maps/place/Chinamhora,+Zimbabwe
# 	s$latitude[s$site == c("Chinamhora")] <- -17.512 # https://www.google.com/maps/place/Chinamhora,+Zimbabwe
# 	s$longitude[s$site == c("Nyanyiwa")] <- 30.857 # https://www.google.com/maps/place/Nyanyiwa
# 	s$latitude[s$site == c("Nyanyiwa")] <- -18.340 # https://www.google.com/maps/place/Nyanyiwa
# 	s$longitude[s$site == c("Muponda")] <- 32.721 # https://www.google.com/maps/place/Muponda+primary+School
# 	s$latitude[s$site == c("Muponda")] <- -18.650 # https://www.google.com/maps/place/Muponda+primary+School
# 	s$adm1[s$adm1 == "Chegut"] <- "Chegutu"
# 	s$adm1[s$adm1 == "Makone"] <- "Makoni"
# 	s$adm1[s$adm1 == "Gormonzi"] <- "Goromonzi"
# 	for (i in 1:nrow(s)) {
# 	  if(is.na(s$latitude[i]) | is.na(s$longitude[i])){
# 	    ll <- carobiner::geocode(country = s$country[i], adm1 = s$adm1[i], location = s$site[i], service = "geonames", username = "efyrouwa")
# 	    ii <- unlist(jsonlite::fromJSON(ll))
# 	    c <- as.integer(ii["totalResultsCount"][[1]])
# 	    s$latitude[i] <- as.numeric(ifelse(c == 1, ii["geonames.lat"][1], ii["geonames.lat1"][1]))
# 	    s$longitude[i] <- as.numeric(ifelse(c == 1, ii["geonames.lng"][1], ii["geonames.lng1"][1]))
# 	  }
# 	}
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

