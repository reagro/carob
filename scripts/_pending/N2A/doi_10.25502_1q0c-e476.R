# R script for "carob"


carob_script <- function(path) {

"Description:

    N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility,
    improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable,
    long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings.
    A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.
    
    The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.

"

	uri <- "https://doi.org/10.25502/1q0c-e476"
	group <- "fertilizer"
	ff <- carobiner::get_data(uri, path, group)
	## dataset level data 
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		project="N2Africa",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= NA,
		data_institutions = "IITA",
   	data_type = "survey", # or, e.g. "on-farm experiment", "survey", "compilation"
		carob_contributor = "Eduardo Garcia Bendito"  
		carob_date="2023-07-12",
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
	  country = ifelse(r$country == "", "Rwanda", trimws(r$country)),
	  site = gsub("I", "", carobiner::fix_name(r$village)),
	  adm1 = carobiner::fix_name(r$district_lga),
	  elevation = r$gps_altitude
	)
	
##### Crop #####
	d$crop[r$type_legume_variety %in% c("Groundnuts")] <- "groundnut"
	d$crop[r$type_legume_variety %in% c("Soybeans")] <- "soybean"

## EGB: Add intercropping
	d$intercrops <- NA	
	r$with_which_crops <- tolower(trimws(r$with_which_crops))
	d$intercrops[r$with_which_crops == "manihot esculenta crantz"] <- "cassava"
	
##### Time #####
	## time can be year (four characters), year-month (7 characters) or date (10 characters).
	## use 	as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- "2011"
	d$harvest_date <- "2012"
	d$season <- "2011/2012"
	
##### Fertilizers #####
	## note that we use P and K, not P2O5 and K2O
	# for readability
	p <- tolower(trimws(r$synthetic_fert_types))
	k <- rep(NA, length(p))
	# Fertilizer type
	k[p %in% c("ssp")] <- "SSP"
	k[p %in% c("ssp,ureia", "ssp, ureia", "ssp,ureia,")] <- "SSP; urea"
	d$fertilizer_type <- k
	
	ftab <- carobiner::get_accepted_values("fertilizer_type", path)[, c("name", "N", "P", "K", "S")]
	ftab <- ftab[ftab$name %in% c("SSP", "urea"), ]
	fmat <- as.matrix(ftab[,-1]) / 100
	fr <- matrix(0, ncol=4, nrow=nrow(d))
	colnames(fr) <- c("N_fertilizer", "P_fertilizer", "K_fertilizer", "S_fertilizer")
	i <- grep("SSP", k)
	fr[i, ] <- rep(fmat[ftab$name=="SSP", ] , each=length(i))
	i <- grep("urea", k)
	fr[i, ] <- fr[i, ] + rep(fmat[ftab$name=="urea", ], each=length(i))
	
	famount <- 10000 * r$amount_fert_kg / r$area  # From kg/m2 to kg/ha
	d <- cbind(d, fr * famount)
	
	# Treatment code	
	d$treatment <- paste0("N", round(d$N_fertilizer),
	                      "P", round(d$P_fertilizer),
	                      "K", round(d$K_fertilizer))
	
	d$OM_used <- r$organic_fert_applied == "Y"
	d$OM_used[r$organic_fert_applied == "N"] <- FALSE
	d$OM_used[r$organic_fert_applied == ""] <- NA

	# # EGB: Add inoculation
	d$inoculated <- r$inoculant_applied == "Y"
	# # EGB: Inoculant is not specified. Only providers.
	
##### Yield #####
	# # EGB: Yield is not reported. Temporarily set to zero
	d$yield <- 0
	message("Yield is missing in te dataset. Enquire data holder")
	# d$yield <- 10000 * r$production_field_kg / r$area # From kg/m2 to kg/ha
	
	#what plant part does yield refer to?
	d$yield_part <- ifelse(d$crop == "groundnut", "pod", "grain")
	
	# # Formatting coordinates into the dataset
	sss <- data.frame(country = "Mozambique",
	                  adm1 = c("Mogovolas", "Mogovolas", "Mogovolas", "Mogovolas", 
	                           "Mogovolas", "Nampula", "Nampula", "Mogovolas", "Angonia", "Angonia", 
	                           "Macanga", "Angonia", "Angonia"),
	                  site = c("Calipo", "Muhua", 
	                           "Naihava", "Mwepane", "Chalaua", "Mariaze", "Muriaze", "Maula", 
	                           "Domue", "Ulongu", "Gandali", "Ulongue", "Dmue"),
	                  latitude = c(-15.625, 
	                               -15.938, -15.785, -15.7376, -16.079, -15.11646, -15.11646, -15.7376, 
	                               -14.528, -14.712, -14.69, -14.712, -14.528),
	                  longitude = c(39.098, 
	                                38.887, 39.705, 39.25971, 39.121, 39.2666, 39.2666, 39.25971, 
	                                34.074, 34.338, 33.701, 34.338, 34.074))

	d <- merge(d, sss, by = c("country", "adm1", "site"), all.x=TRUE)
	carobiner::write_files(dset, dd, path=path)
}

# # # EGB: Extracting spatial coordinates:
# 
# s <- unique(d[,c("country", "adm1", "site")])
# s$latitude <- NA
# s$longitude <- NA
# s$longitude[s$site == "Calipo"] <- 39.098 # https://www.google.com/maps/place/Calipo,+Mozambique
# s$latitude[s$site == "Calipo"] <- -15.625 # https://www.google.com/maps/place/Calipo,+Mozambique
# s$longitude[s$site == c("Muhua")] <- 38.887 # https://www.google.com/maps/place/Muh%C3%AAa,+Mozambique
# s$latitude[s$site == c("Muhua")] <- -15.938 # https://www.google.com/maps/place/Muh%C3%AAa,+Mozambique
# s$longitude[s$site %in% c("Naihava")] <- 39.705 # https://www.google.com/maps/place/Naihava,+Mozambique
# s$latitude[s$site %in% c("Naihava")] <- -15.785 # https://www.google.com/maps/place/Naihava,+Mozambique
# # s$longitude[s$site %in% c("Mwepane")] <-  # 
# # s$latitude[s$site %in% c("Mwepane")] <-  # 
# s$longitude[s$site %in% c("Chalaua")] <- 39.121 # https://www.google.com/maps/place/Chalaua,+Mozambique
# s$latitude[s$site %in% c("Chalaua")] <- -16.079 # https://www.google.com/maps/place/Chalaua,+Mozambique
# # s$longitude[s$site %in% c("Mariaze", )] <-  # https://www.google.com/maps/place/Gacaca,+Rwanda
# # s$latitude[s$site %in% c("Mariaze")] <-  # https://www.google.com/maps/place/Gacaca,+Rwanda
# # s$longitude[s$site %in% c("Maula")] <-  # https://www.google.com/maps/place/Gitare+Secondary+School
# # s$latitude[s$site %in% c("Maula")] <-  # https://www.google.com/maps/place/Gitare+Secondary+School
# s$longitude[s$site %in% c("Domue", "Dmue")] <- 34.074 # https://www.google.com/maps/place/Domue,+Mozambique
# s$latitude[s$site %in% c("Domue", "Dmue")] <- -14.528 # https://www.google.com/maps/place/Domue,+Mozambique
# s$longitude[s$site %in% c("Ulongu", "Ulongue")] <- 34.338 # https://www.google.com/maps/place/Ulongwe,+Mozambique
# s$latitude[s$site %in% c("Ulongu", "Ulongue")] <- -14.712 # https://www.google.com/maps/place/Ulongwe,+Mozambique
# s$longitude[s$site %in% c("Gandali")] <- 33.701 # https://www.google.com/maps/place/Igreja+CAT%C3%93LICA+-+Gandali
# s$latitude[s$site %in% c("Gandali")] <- -14.690 # https://www.google.com/maps/place/Igreja+CAT%C3%93LICA+-+Gandali
# ss <- unique(s[is.na(s$latitude), c("country", "adm1", "site")])
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
