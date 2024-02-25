# R script for "carob"

# ## ISSUES 

# ....


carob_script <- function(path) {
  
  "Description:
  Human Zn and Fe deficiencies can be reduced through agronomic biofortification, but information on factors influencing maize grain-Zn and -Fe levels remains scant. This analysis: (1) Establishes the global distribution of Zn and Fe concentrations in maize grain; (2) Assesses different agronomic practices’ contributions to increasing maize-grain Zn and Fe levels; and (3) Identifies key biophysical factors to guide agronomic biofortification. Using 1,332 data points in 102 published papers from 24 countries, we estimated a 24% probability of grain-Zn concentrations exceeding the benchmark target of 38 mg kg−1."
  
  uri <- "doi:10.7910/DVN/SOAWL6"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "maize_trials"

  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)
  

  ## dataset level data 
  dset <- data.frame(
	carobiner::extract_metadata(js, uri, group=group),
    project=NA,
    data_citation="Job Kihara; Sileshi, Gudeta W.; Bolo, Peter Omondi; Mutambu, Dominic Mwanzia; Sila, Andrew Musili, 2023. Maize grain zinc and iron concentrations as influenced by agronomic management and biophysical factors: a meta-analysis. https://doi.org/10.7910/DVN/SOAWL6, Harvard Dataverse, V1",
    publication= NA,
    data_institutions = "CIAT",
    data_type="experiment",
    carob_contributor="Fredy Chimire",
    carob_date="2024-2-24"
  )
  
  # Select sheet with revised data from the text file file 
  f <- ff[basename(ff) == "02a. Maize data for meta_analysis_updated.txt"]
  
 # read dataset
  r <- read.table(f, header = TRUE, sep = "\t", quote = "", fill = TRUE, na.strings = "")
  
  d <- data.frame(country=r$Country, 
				  soil_clay=as.numeric(r$Clay),
                  soil_sand=as.numeric(r$Sand), soil_silt=as.numeric(r$Silt),
				  soil_SOC=r$SOC...., soil_SOM=r$SOM, soil_pH=r$pH,
                  Zn_fertilizer=r$Zn_Applied_Trt, planting_date=r$Yr_experiment,
                  soil_pH_CaCl2=r$pH_CaCl2, site= r$TrialSite,
                  N_fertilizer=r$N_Applied_Cnt, K_fertilizer= r$K_Applied_Cnt, 
				  P_fertilizer=r$P_Applied_Cnt,
                  irrigated= r$Irrigation_Trt, yield = as.numeric(r$GrainYld_Trt))
 
    
  # for first dataset
  d$dataset_id <- dataset_id
  #zn =233,nfer= 655, p=300
  d$crop <- "maize"
  d$planting_date[grep("-", d$planting_date)] <- NA
  d$planting_date[grep("\\.", d$planting_date)] <- NA
  d$planting_date <- gsub("[^0-9]", "", d$planting_date)

  d$yield <- as.numeric(d$yield) * 1000
### ???  d$yield <- ifelse(is.na(d$yield),0,d$yield)
  
#  d$Zn_fertilizer <- as.numeric(gsub("[^0-9.]", 0, d$Zn_fertilizer))
#  d$N_fertilizer <- as.numeric(gsub("[^0-9.]", 0, as.character(d$N_fertilizer)))
#  d$K_fertilizer <- as.numeric(gsub("[^0-9.]", 0, as.character(d$K_fertilizer)))
#  d$P_fertilizer <- as.numeric(gsub("[^0-9.]", 0, as.character(d$P_fertilizer)))

	d$Zn_fertilizer[grep("yes", d$Zn_fertilizer, ignore.case=TRUE)] <- NA
	d$Zn_fertilizer <- as.numeric(d$Zn_fertilizer)
	
	
  # Convert planting_date to character
  d$planting_date <- as.character(d$planting_date)
  d$striga_trial <- FALSE
  d$borer_trial <- FALSE
  d$striga_infected <- FALSE
 
  d$yield_part <- "grain"
  d$trial_id <- as.character(as.integer(as.factor(paste(d$country, d$site))))
  
  # Convert to UTF-8 encoding
  d$site <- sapply(d$site, \(x) iconv(x, "UTF-8", "UTF-8"))
  
  # Remove quotes from elements in the 'r' column
  d$site <- gsub("\"", "", d$site)
  d <- d[!is.na(d$site), ]
  
  # Identify rows where 'r' column contains numeric characters
  numeric_rows <- grepl("\\d+", d$site)  # Matches any digit (\d+) one or more times
  # Subset the dataframe to exclude rows with numeric characters in 'site' column
  d <- d[!numeric_rows, ]
  #filtered_df$country <- carobiner::fix_name(filtered_df$country, "title")
  
  
  # Pick unique coordinates of the location
  locs <- unique(d[,c("country","site")]) 
  #geocodes_filtered <- subset(geocodes, !(country %in% c("Bosnia and Herzegovina", "Malaysia")))
  
  locs <- na.omit(locs) # remove null values
  # Function to convert coordinates to proper GPS format
  
  #geocodes <- carobiner::geocode(country=locs$country,location=locs$location,adm1=locs$adm1)
 
### cannot have "live geocoding" in script 

 geocode <- carobiner::geocode(country=locs$country,location=locs$site)$df
  geocode <- na.omit(geocode) # remove null values
  geocodes <- subset(geocode, !(country %in% c("Bosnia and Herzegovina", "Malaysia")))
  
  # dataframe withlocations not retrieved from carobiner geo code
  q <- geocode[is.na(geocode$lon),]
  q1 <- subset(geocode, (country %in% c("Bosnia and Herzegovina", "Malaysia")))
  q2 <- rbind(q1,q)
  
  # manually search for coordinates with NA
  coord <- list("Navsari Agricultural University" = c(20.9250, 72.9079),
                "Khalsa College Amritsar" = c( 31.6417,74.8358),
                "Zonal Agriculture Research Station, Navile, Shimoga" = c(14.7007,75.5721),
                "Damo, El-Fayoum Governorate"= c(29.3216, 30.8531),
                "Faculty of Agriculture, Fayoum University"= c(29.3214, 30.8385),
                "Agricultural Research Station, Bhavanisagar"= c(11.4835, 77.1370),
                "Palamur"= c(16.6413, 78.1031),
                "College of Agriculture CSK HPKV, Palampur"= c(32.7513, 76.5466),
                "FCAV/UNESP"= c(-21.2437, -48.2930),
                "UNESP (campus of Jaboticabal)"= c(-21.2424, -48.2942),
                "College of Agriculture, Fayoum University"= c(29.3214, 30.8385),
                "College of Agriculture, Fayoum University,"= c(29.3214, 30.8385),
                "Shenda Park Maharashtra"= c(16.6756, 74.2353),
                "Assam Agricultural University, Jorhat"= c(26.7250, 94.1937),
                "University ofAgriculture, Faisalabad, Pakistan"= c(31.4313, 73.0748),
                "Agricultural Research Institute, Faisalabad, Pakistan"= c(31.4043, 73.0487),
                "Maharana Pratap University of Agriculture and Technology"= c(24.6011, 73.7393),
                "College of Agriculture, University of Sargodha,"= c(32.1337, 72.6867),
                "University of Agriculture, Faisalabad"= c(31.4329, 73.0743),
                "Indian Agricultural Research Institute, New Delhi"= c(28.6355, 77.1644),
                "al-Gemmiza Agricultural Research Station"= c(31.1420, 30.9476),
                "University of Agriculture, Peshawar"= c(34.02081, 71.4814),
                "Mansoura University, Dakhlia"= c(31.0453, 31.3513),
                "Ayub Agricultural Research Institute, Faisalabad, Pakistan"= c(31.4043, 73.0487),
                "Chinhengo"= c(-17.7684, 31.0395),
                "Ramayanpatti village"= c(8.7651, 77.6707),
                "Ramayanpatti village "= c(8.7651, 77.6707),
                "Quzhou Experimental Station"= c(28.9646, 118.8766),
                "Qalubeya governorate"= c(30.3908, 31.4457),
                "Demo village"= c(29.3210, 30.9307),
                "Taralu (Peri-urban)"= c(12.7876, 77.524),
                "Kaggalhalli (Rural)"= c(12.7350, 77.4879),
                "Muhwati"= c(-18.305, 30.6845),
                "BeniSuef Governorate"= c(28.9353, 30.9612),
                "Chandra Shekhar Azad University of Agriculture and Technology, Kanpur"= c(26.4912, 80.3071),
                "ICAR-Indian Agricultural Research Institute, New Delhi"= c(28.6355, 77.1644),
                "Pommanapadi village"= c(11.1453, 78.7660),
                "Ramayanpatti village"= c(8.7690, 77.6714),
                "Quzhou Experimental Station"= c(28.9645, 118.8762),
                "Pampas (Tilisarao, TI)"= c(-32.7319, -65.2872),
                "Pampas (Oliveros, OL)"= c(-32.7319, -65.2872),
                "Pampas (Pergamino, PE)"= c(-32.7319, -65.2872),
                "National Agricultural Research Centre"= c(33.6801, 73.1393),
                "Bougni"= c(36.5451, 3.9389),
                "Tulatuli"= c(23.4567, 90.7315),
                "Gabura"= c(22.2574, 89.27445),
                "Old Brahmaputra Floodplain"= c(26.7549, 92.4210),
                "Kabootar-abad station"= c(32.4977, 51.8284),
                "Quzhou Experimental Station"= c(28.9646, 118.8763),
                "Chitedze Agricultural Research Station (CARS)"= c(-13.9813, 33.6372),
                "Ba?cyny"= c(54.1320, 20.0415),
                "Lower Silesia, South-western Poland)"= c(51.2158, 16.9155),
                "Al-Maamorah district"= c(31.2930, 30.0403),
                "Research Farm of Hill Agricultural Research and Extension Center, Bajaura"= c(31.8351, 77.1707),
                "NG-SAM-MZ"= c(4.8906, 7.1071),
                "NG-DD-MZ-"= c(10.6759, 7.4802),
                "NG-GW-MZ-"= c(10.6759, 7.4802),
                "Mississippi State University"= c(33.4565, -88.7965),
                "Selangor"= c(3.5166, 101.5138),"Northwest"= c(44.3814, 17.8184))
    #q$location <- trimws(q$location, "right")           
    q2$lat <- unlist(lapply(q2$location, function(loc) coord[[loc]][1]))
    q2$lon <- unlist(lapply(q2$location, function(loc) coord[[loc]][2]))
    
 
    
  #geocodes2 <- carobiner::change_names(geocodes1,c("location", "lon","lat"),c("site","longitude","latitude"))           
  coordinates <- rbind(geocodes,q2)
    
  coordinates <- carobiner::change_names(coordinates,c("location", "lon","lat"),c("site","longitude","latitude"))           
  
  
  
  
  #geocodes3 <- unique(geocodes2)
  
  mergeddf <- merge(coordinates,d,by= c("country","site"),all.x=TRUE)
  d1 <- mergeddf
  d1$country <- carobiner:: replace_values(d1$country, from="USA", to= "United States")
  
  #convert column into required data format
  d1$irrigated <- ifelse(tolower(d1$irrigated) == "yes", TRUE, FALSE)
  
  
  carobiner::write_files(dset, d1, path=path)
}
