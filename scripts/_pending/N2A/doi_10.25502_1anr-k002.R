# R script for "carob"

carob_script <- function(path) {
  
"N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household  nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project."

	uri <- "doi:10.25502/1anr-k002"
	group <- "agronomy"
	ff	 <- carobiner::get_data(uri, path, group)
 
	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		project="N2Africa",
		publication= NA,
		data_organization = "IITA",
		carob_contributor="Effie Ochieng'",
		carob_date="2023-05-30",
		data_type="on-farm experiment",
		response_vars = "yield",
		treatment_vars = "variety;N_fertilizer;P_fertilizer;K_fertilizer;lime;Ca_fertilizer;Zn_fertilizer;B_fertilizer;Mo_fertilizer;Mg_fertilizer;OM"
    )
  
	
	f <- ff[basename(ff) == "data_table.csv"]
	d <- read.csv(f)
	
	d$trial_id <- d$id
	d$irrigated <- FALSE 
	d$is_survey <- TRUE
	d$on_farm <- FALSE
	
	#subsetting each treatment with their respective observations into data different datasets then appending the rows later
	old_nms <- c("trial_id","treatmentX", "row_spacing_crop_1_plot_X.cm", "plant_spacing_crop_1_plot_X.cm","no_plants_hole_crop_1_plot_X.nr", "width_of_harvested_plot_crop_1_plot_X.m","number_of_rows_crop_1_plot_X.nr", "grain_weight_crop_1_plot_X.kg", "pod_weight_groundnut_crop_1_plot_X.kg","above_ground_biomass_weight_crop_1_plot_X.kg")
	new_names <- c("trial_id","treatment","row_spacing","plant_spacing","plants_hole","width_harvest","no_rows","seed_weight","pod_weight","fwy_residue")

	old_nms3 = gsub("number_of_rows_crop_1_plot_X.nr", "number_of_rows_crop_1_plot_.nr", old_nms)

	lst <- lapply(1:12, function(i) {
			if (i == 3) {
				di <- d[, gsub("X", i, old_nms3)]
			} else {
				di <- d[, gsub("X", i, old_nms)]
			}
			colnames(di) <- new_names
			di
		})
	dd1 <- data.frame(do.call(rbind, lst))
	dd1 <- dd1[!is.na(dd1$treatment), ]
	
	dd1$row_spacing <- as.numeric(dd1$row_spacing)
	dd1$plant_spacing <- as.numeric(dd1$plant_spacing)
	dd1$plant_spacing[dd1$plant_spacing == 252] <- 25

	
	#subsetting for the other variables of interest
	dd2 <- d[,c("trial_id","submissiondate","start","irrigated","on_farm","is_survey","end","country","lga_district_woreda", "sector_ward", "legume_planted_in_the_n2africa_trial","crop_1_previous_season","inoculation_n2africa_field")]

	dd3 <- merge(dd2, dd1, by = "trial_id", all = TRUE) 
	
	
 # bringing in the groundnut yield entries with the rest of the yield entries 
	i <- is.na(dd3$seed_weight) & !is.na(dd3$pod_weight)
	dd3$seed_weight[i] <- dd3$pod_weight[i]
		
	dd3$inoculated <- dd3$inoculation_n2africa_field == "y"
	dd3$crop <- carobiner::replace_values(dd3$legume_planted_in_the_n2africa_trial, c("soya_bean", "climbing_bean", "bush_bean"), c("soybean","common bean","common bean"))
	dd3$planting_date <- dd3$start
	dd3$harvest_date <- dd3$end
	dd3$country <- carobiner::fix_name(dd3$country, case = "title")
	dd3$adm2 <- carobiner::fix_name(dd3$lga_district_woreda, case = "title")
	dd3$location <- carobiner::fix_name(dd3$sector_ward, case = "title")

 #to get the varieties and the fertilizer inputs
	ft <- strsplit(dd3$treatment , "[,+]+")
	
	max_splits <- max(sapply(ft, length))
	split_list <- list()
	for (i in 1:max_splits) {
		split_list[[i]] <- sapply(ft, function(x) ifelse(length(x) >= i, x[i], NA))
	}
	dd4 <- cbind(dd3, as.data.frame(split_list))
	
	
	latitude <- apply(d[, c("gps_latitude_field.decimal_degrees","gps_field_device_latitude.decimal_degrees")], 1, mean, na.rm=TRUE)
	longitude <- apply(d[, c("gps_field_device_longitude.decimal_degrees","gps_longitude_field.decimal_degrees")], 1, mean, na.rm=TRUE)
	lonlat <- cbind(d[, "trial_id", drop=FALSE], longitude, latitude)
	
	dd5 <- merge(dd4, lonlat, by = "trial_id")
 
 
	#filling in the fertilizer info using the split columns
 
	ft <- c(dd5$c..TGX1951.3F....TGX1951.3F....TGX1951.3F....TGX1951.3F....TGX1951.3F..., 
			dd5$c...I......I......control.....I......I......I.Only.....I......I...., 
			dd5$c...I......I......control.....I......I......I.Only.....I......I....,
			dd5$c...K...NA..NA....K.....K...NA....K...NA....K...NA....K...NA..,
			dd5$c...Micronutrients...NA..NA..NA....Micronutr...NA....Micronutr...,
			dd5$c.NA..NA..NA..NA...OM...NA...OM...NA..NA..NA..NA..NA..NA..NA..,
			dd5$c.NA..NA..NA..NA...OM...NA...OM...NA..NA..NA..NA..NA..NA..NA..,
			dd5$c.NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA...1,
			dd5$c.NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA...2,
			dd5$c.NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA...3,
			dd5$c.NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA...4)

	ft <- gsub(" ","",ft)
	ft <- gsub("POnly","P",ft)
	ft <- gsub("Manure","OM",ft)
	ft <- gsub("FYM","OM",ft)
	ft[ grepl("icro", ft) ] <- "micronutrients" 
	ft[ grepl("psum", ft) ] <- "gypsum" 
	ft[ grepl("rol", ft) ] <- "control" 
	ft[ grepl("OM", ft) ] <- "OM" 

	# making the rows in the vector the same as the rows
	num_rows <- nrow(dd5)
	ft <- ft[1:num_rows]
	dd5$mix <- ft

	dd5$variety <- ifelse(dd5$mix %in% c("TGX1951-3F","Nabe12C","TGX1955-4F","Pendo","Tumaini","SAMNUT24","Songotra","Maksoy3N", "Lyamungu90", "Local","Samnut23","SAMNUT22","Jenguma","Minjingu"), dd5$mix, NA) 
	
	dd5$N_fertilizer <- 0
	## RH: needs to be fixed
	dd5$gypsum <- -99
	
	dd5$P_fertilizer <- ifelse(dd5$mix %in% c("P","PK","NPK"), 30, 0)
	dd5$K_fertilizer <- ifelse(dd5$mix %in% c("NPK","PK","K"), 20, 0)
	dd5$OM_used <- dd5$mix == "OM"
	dd5$Zn_fertilizer <- ifelse(dd5$mix == "Zn", 2.5, 0)
	dd5$S_fertilizer <- ifelse(dd5$mix == "S", 12, 0)
	
	# putting in the yield information
	# 1) get the length and width of plot size in meters
	dd5$length <- ((dd5$row_spacing) * (dd5$no_rows-1)) /100
		 
	 
	#2) get plot size in m2
	dd5$plot_size <- dd5$length * dd5$width_harvest

	## some fields are unreasonably small or large
	dd5$plot_size[dd5$plot_size < 30] <- NA
	dd5$plot_size[dd5$plot_size > 200] <- NA
	 
	#3) get the yield/ha
	dd5$yield <- 10000 * dd5$seed_weight / dd5$plot_size

	## some yields are crazy
	dd5$yield[dd5$yield > 6000] <- NA

	dd5$fwy_residue <- 10000 * dd5$fwy_residue / dd5$plot_size
	dd5$fwy_residue[dd5$fwy_residue > 10000] <- NA

	dd5$planting_date <- as.character(as.Date(dd5$planting_date, format = "%m/%d/%Y"))
	dd5$harvest_date <- as.character(as.Date(dd5$harvest_date, format = "%m/%d/%Y"))

	z <- dd5[, c("trial_id", "irrigated","on_farm","is_survey","country", "adm2", "location","latitude","longitude","planting_date","harvest_date","crop","variety","inoculated","row_spacing","N_fertilizer","P_fertilizer","K_fertilizer","Zn_fertilizer","S_fertilizer","OM_used","plant_spacing","yield","fwy_residue")]

	z <- z[is.finite(z$yield), ]
	z$yield_part <- "grain"
	z$yield_part[z$crop == "groundnut"] <- "pod"


	#library(terra)
	#w = geodata::world(path="data")
	#e = extract(w, z[,c("longitude", "latitude")])
	#e = cbind(e, z$country, z$longitude, z$latitude)
	#table(e[,3], e[,4])
	#i = which(e[,3] == "Democratic Republic of the Congo")
	#plot(z[z$country=="Uganda", c("longitude", "latitude")])
									
	tza = which(z$country == "Tanzania" & z$latitude > 0)
	z$latitude[tza] <- -z$latitude[tza] 
		
	gha = which(z$country == "Ghana")
	z$longitude[gha] <- -z$longitude[gha] 
	i <- which(z$location == "Narango")
	z$latitude[i] <- 10.6
	z$longitude[i] <- -0.1
	i <- which(z$location == "Bykufa Biyu")
	z$longitude[i] <- -0.4
	i <- which(z$location == "Pishegu")
	z$latitude[i] <- 9.97
	z$longitude[i] <- -0.63

	uga <- which(z$country == "Uganda" & z$latitude > 0 & z$longitude < 31)
	z$latitude[uga] <- -z$latitude[uga]

	i <- which(z$country=="Ghana" & z$adm2=="Tolon" & z$location=="Cheshegu")
	z$latitude[i] <- 9.46
	
	z <- carobiner::geocode_duplicates(z, vars=c( "country", "adm2", "location"))

	#i <- which(is.na(z$latitude))
	#u <- unique(z[i, c("country", "adm2", "location")])
	#g <- carobiner::geocode(u$country, u$location, u$adm2)
	#g$put
	georef = data.frame(country = c("Ghana", "Ghana", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Tanzania", "Tanzania", "Tanzania", "Tanzania", "Tanzania", "Tanzania", "Tanzania", "Tanzania", "Tanzania", "Tanzania", "Tanzania", "Tanzania", "Tanzania", "Tanzania"), 
	adm2 = c("Tolon", "Tolon", "Bayo", "Bayo", "Bayo", "Biu", "Biu", "Biu", "Hawul", "Hawul", "Hawul", "Hawul", "Hawul", "Hawul", "Hawul", "Kwaya Kusar", "Kwaya Kusar", "Kwaya Kusar", "Gairo", "Kilosa", "Kilosa", "Kilosa", "Kongwa", "Kongwa", "Kongwa", "Kongwa", "Kongwa", "Kongwa", "Kongwa", "Kongwa", "Mvomero", "Mvomero"), 
	location = c("Gbulahigu", "Tunayili", "Jauro Garga", "Maina Baba", "Teli", "Kabura", "Miringa", "Yamarkumi", "Buba Kayamda", "Marama", "Mbulatawiwi", "Sakwa", "Tila", "Tong", "Vina Dam", "Gashina", "Gusi", "Kurba Gayi", "Chigela Mogohigwa", "Mhenda", "Ulaya Mbuyuni", NA, "Hogoro", "Ibwaga", "Kinangali", "Lenjulu Majawanga", "Majawanga", "Matongoro", "Mlali", "Tubugwe Kibaoni", "Lubungo", NA), 
    lon = c(-0.9677118, -0.975013403430885, 11.5494, 11.5786, 11.7044, 12.2262016, 12.1515071250359, 12.1634, 11.9763928, 12.221662, 12.100181, 12.189298, 12.1310782645939, 12.2587299, 12.0501649, 11.8596, 12.0172, 11.8303723281892, 36.9010534, 36.9268124, 36.9129876, 36.9211504377905, 36.450462, 36.4713136, 36.6367131, 36.8242004, 36.8242004, 36.4682397, 36.7506831573386, 36.629133, 37.4949216, 37.611782015731), 
    lat = c(9.3576293, 9.3699943, 10.321, 10.3422, 10.4454, 10.5809263, 10.7320023, 10.6585, 10.5713263, 10.428557, 10.4334259, 10.4914201, 10.54434965, 10.5429129, 10.5865922, 10.5227, 10.3993, 10.44856195, -6.0976429, -7.1751048, -7.0355513, -6.95946295, -5.9545867, -6.2229198, -6.065013, -6.1119194, -6.1119194, -5.8027105, -6.2998788, -6.356593, -6.8322667, -6.6008742))

	z <- merge(z, georef, all.x=TRUE, by=c( "country", "adm2", "location"))
	z$latitude <- ifelse(is.na(z$latitude), z$lat, z$latitude)
	z$longitude <- ifelse(is.na(z$longitude), z$lon, z$longitude)
	z$lon <- z$lat <- NULL

	#i <- which(is.na(z$latitude))
	#u <- unique(z[i, c("country", "adm2")])
	#uu <- u[!is.na(u$adm2), -3]
	#g <- carobiner::geocode(uu$country, uu$adm2)
	#g$put
	georef2 <- data.frame(
		country = c("Ghana", "Ghana", "Nigeria", "Nigeria", "Tanzania", "Tanzania"), 
		adm2 = c("Tolon", "Wa-West",  "Hawul", "Kwaya Kusar", "Gairo", "Kilosa"), 
		lon = c(-1.0666, -2.6833, 12.3078, 11.8543, 36.8825, 36.9212), 
		lat = c(9.4325, 9.8986, 10.4545, 10.4857, -6.2292, -6.9595))
	
	z <- merge(z, georef2, all.x=TRUE, by=c( "country", "adm2"))
	z$latitude <- ifelse(is.na(z$latitude), z$lat, z$latitude)
	z$longitude <- ifelse(is.na(z$longitude), z$lon, z$longitude)
	z$lon <- z$lat <- NULL
	
	# assigning central point in Tolon
	i <- which(z$country=="Nigeria" & is.na(z$adm2) & z$location=="Uran I")
	z$latitude[i] <- 8.8028 
	z$longitude[i] <- 12.0435
	z$geo_from_source <- FALSE
	
		
	carobiner::write_files(meta, z, path=path)
}	
	
