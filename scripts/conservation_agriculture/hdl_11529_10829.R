# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
The present data is based on on-farm demonstration sites set in Malawi to demonstrate 
the best options available at the moment for the management of drought-tolerant maize 
varieties and conservation agriculture practices in Balaka, Machinga and Zomba 
communities. Crop yields in southern Africa are generally low compared to the world 
average and the average of developing regions. Thus, this calls for the identification 
of more sustainable strategies that are capable of increasing yields. Amongst the 
possible strategies is conservation agriculture (CA). This data is a subset of a larger
data set from southern Africa that seeks to demonstrate the effects of CA technologies 
as compared to the traditional farmers' practices. The CA treatments included: 1. Farmers
check. Traditional land preparation (ridges) and maize management. Residues may be grazed,
removed, burned or incorporated into the ridges. 2. Conservation Agriculture – sole maize. 
No tillage, no burning. Previous year’s ridges retained (but not reformed). Residue retained 
(mulch). 3. Conservation Agriculture – maize/legume intercrop. No tillage, no burning.
Previous year’s ridges retained (but not reformed). Residue retained (mulch). 
The data set presents yields for maize and the legumes from these sites over 10 seasons 
(2005-2015). (2016-12-08)

"

	uri <- "hdl:11529/10829"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "conservation_agriculture"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation='Thierfelder, Christian, 2016, "Options available for the management of drought-tolerant maize varieties and conservation agriculture practices in Malawi", https://hdl.handle.net/11529/10829, CIMMYT Research Data & Software Repository Network, V1',
		publication=NA,
		data_institutions = "CIMMYT",
   	data_type="on-farm experiment",
		carob_contributor="Mitchelle Njukuya",
		carob_date="2023-06-21",
		revised_by="Effie Ochieng'"
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)
	dset$license <- carobiner::get_license(js)
	
	# f <- ff[basename(ff) == "Summary files Malawi 2005-15..xlsx"]
	# r <- readxl::read_excel(f,sheet="Maize working data") |> as.data.frame()
	# r1 <- readxl::read_excel(f,sheet="Legume yields") |> as.data.frame()
	# r2 <- readxl::read_excel(f,sheet="Intercropped legume yields") |> as.data.frame()
	
	# efyrouwa: .xlsx files can be read by carobiner::read.excel() 
	f <- ff[basename(ff) == "Summary files Malawi 2005-15..xlsx"]
  r <- carobiner::read.excel(f, sheet = 1)
  r1 <- carobiner::read.excel(f, sheet = 2)
  r2 <- carobiner::read.excel(f, sheet = 3)	
  
  ## process file(s)
  
  ## use a subset
	d <- carobiner::change_names(r, c("Harvest Year","District","Village","Site/area","crop grown","Tmnt.","Stalk yield (kg/ha)","Grain yield (kg/ha)"),c("harvest_date","adm1","site","rep","crop","treatment","residue_yield","yield"))
	d$crop <- tolower(d$crop)
	d$country <- "Malawi"
	d$intercrops <- NA
	d$intercrops[d$`sole/intercrop`==1 & d$treatment %in% c("CA +Maize/Pp", "CA+maize/Pp", "Maizepp")] <- "pigeon pea"
	d$intercrops[d$`sole/intercrop`==1 & d$treatment %in% c("CA +Maize/mucuna")] <- "velvet bean"
	d$intercrops[d$`sole/intercrop`==1 & d$treatment %in% c("CA+Maize/Cp")] <- "cowpea"
	t <- d$treatment
	t <- carobiner::fix_name(t, case = "lower")
	t <- gsub("/pp","_with_peagon_pea",t)
	t <- gsub("pp","_with_peagon_pea",t)
	t <- gsub("/mucuna","_with_velvet_bean",t)
	t <- gsub("/cp","_with_cowpea",t)
	t <- gsub("ca","conservation_agriculture_",t)
	t <- gsub("\\+\\s?mz","with_maize",t)
	t <- gsub("\\s?\\+\\s?maize","with_maize",t)
	t <- gsub(" ","_",t)
	t <- gsub("\\s?\\/leg","_with_legume",t)
	t <- gsub("_leg_","_legume_",t)
	t <- gsub("_rot","_rotation",t)
	t <- gsub("conventional_controls","conventional_control",t)
  #efyrouwa: what is treatment ds? and sb
	d$treatment <- t
	d$on_farm <- TRUE 
	d$is_survey <- FALSE
	d$site <- carobiner::replace_values(d$site,c("Champhira","Lemu","Matandika"),c("Champhila","Balaka","Machinga"))
  l <- unique(d[c("country", "site")])
  
  # efyrouwa: finding the coordinates, this part should not be in the script or be commented out
  # for (i in 1:nrow(l)) {
  #     ll <- carobiner::geocode(country = l$country[i],location = l$site[i], service = "geonames", username = "efyrouwa")
  #     ii <- unlist(jsonlite::fromJSON(ll))
  #     c <- as.integer(ii["totalResultsCount"][[1]])
  #     l$latitude[i] <- as.numeric(ifelse(c == 1, ii["geonames.lat"][1], ii["geonames.lat1"][1]))
  #     l$longitude[i] <- as.numeric(ifelse(c == 1, ii["geonames.lng"][1], ii["geonames.lng1"][1]))
  #   }
  #  code below will print the cordinates then copy paste to make make a data frame
  #   sss <- dput(l) 
   
	ll <- data.frame(country = c("Malawi", "Malawi", "Malawi", "Malawi","Malawi", "Malawi", "Malawi",
	                             "Malawi", "Malawi", "Malawi", "Malawi","Malawi", "Malawi", "Malawi"), 
	                 site = c("Malula", "Enyezini", "Chisepo", "Chipeni", "Zidyana", "Mwansambo","Balaka", 
	                          "Machinga", "Linga", "Herbert", "Songani", "Chinguluwe", "Champhila", "Kaluluma"), 
	                 latitude = c(-14.96656, -11.46457, -13.63281, -13.81782, -13.21417, -13.6966, -14.97928, 
	                              -15.06665, -13.06345, -13.5396, -15.3, -13.81066, -12.40513, -12.58075), 
	                 longitude = c(34.99189, 33.86478, 33.47223, 33.38493, 34.31717, 33.55553, 34.95575, 35.22543, 
	                               33.43611, 33.02705, 35.48333, 33.49862, 33.64337, 33.51833))
	
	d <- merge(d,ll, by = c("country", "site"), all.x = TRUE)
	d$yield_part <- "grain"
	
	d <- d[,c("country","adm1","site","latitude","longitude","treatment","harvest_date","crop","yield_part","intercrops","on_farm","is_survey","residue_yield","yield")]
	
	# second data set
	d1 <- carobiner::change_names(r1,c("Site","Tmnt.","Crop grown","Final stand (pl/ha)","Grain yield (kg/ha)","total Biomass yield (kg/ha)", "Year" ),c("adm1","treatment","crop","plant_density","yield","biomass_total","harvest_date"))
	d1$country <- "Malawi"
	d1$crop <- carobiner::fix_name(d1$crop, case = "lower")
	d1$crop <- carobiner::replace_values(d1$crop,c("soya","cowpeas"),c("soybean","cowpea"))
	d1$crop[d1$crop %in% c("pigeonpea","p/peas")] <- "pigeon pea"
	d1$crop[d1$crop %in% c("groundnuts","gnuts","g/nuts")] <- "groundnut"
	t <- d1$treatment
	t <- carobiner::fix_name(t, case = "lower")
	t <- gsub("ca", "conservation_agriculture",t)
	t <- gsub("\\s?\\+\\s?legume","_with_legume",t)
	t <- gsub(" ","_",t)
	t <- gsub("\\s?\\+\\s?g/nuts","with_groundnut",t)
	t <- gsub("\\gnuts","with_groundnut",t)
	t <- gsub("\\s?\\+groundnuts","with_groundnut",t)
	t <- gsub("\\+groundnnuts","_with_groundnut",t)
	t <- gsub("\\+|-mz", "_with_maize",t)
	t <- gsub("\\+|?p/peas","_with_pigeonpea",t)
	t <- gsub("\\+p/pea","_with_pigeonpea",t)
	t <- gsub("\\+ppea","_with_pigeonpea",t)
	t <- gsub("\\ppea","_with_pigeonpea",t)
	t <- gsub("groudnnuts","_with_groundnut",t)
	d1$treatment <- t
	d1$on_farm <- TRUE 
	d1$is_survey <- FALSE
	d1$adm1 <- carobiner::replace_values(d1$adm1,c("Champhira","Lemu","Matandika"),c("Champhila","Balaka","Machinga"))
	l <- unique(d1[,c("country","adm1")])
	
	# efyrouwa: lat and lon for the second dataset
	# follow process above

	ll <- data.frame(country = c("Malawi", "Malawi", "Malawi", "Malawi","Malawi", "Malawi", "Malawi",
	                             "Malawi", "Malawi", "Malawi", "Malawi","Malawi"),
	                  adm1 = c("Malula", "Zidyana", "Chipeni", "Mwansambo","Balaka", "Herbert", 
	                           "Kaluluma", "Champhila", "Chinguluwe", "Machinga","Songani", "Linga"), 
	                  latitude = c(-14.96656, -13.21417, -13.81782, -13.6966, -14.97928, -13.5396, 
	                               -12.58075, -12.40513, -13.81066, -15.06665, -15.3, -13.06345), 
	                  longitude = c(34.99189, 34.31717, 33.38493, 33.55553, 34.95575, 33.02705, 
	                                33.51833, 33.64337, 33.49862, 35.22543, 35.48333, 33.43611))
	
	d1 <- merge(d1,ll, by=c("country", "adm1"), all.x = TRUE)
	d1$yield_part <- "seed"
  d1 <- d1[, c("country","adm1","harvest_date","treatment","crop","yield_part","plant_density","biomass_total",
                "yield", "on_farm","is_survey","latitude","longitude")]

	# third data set
	d2 <- carobiner::change_names(r2, c("Year","Site name","Tmnt.","Crop grown","Final stand (pl/ha)","total Biomass yield (kg/ha)","Grain yield (kg/ha)"),
	                                  c("harvest_date","site","treatment","crop","plant_density","biomass_total","yield"))
	d2$crop[d2$crop %in% c("P/peas","p/peas","Pigeonpea")] <- "pigeon pea"
	d2$crop[d2$crop %in% c("cowpeas","cowpes")] <- "cowpea"
	d2$treatment <- carobiner::replace_values(d2$treatment,"Legume intercrop","legume_intercrop")
	d2$country <- "Malawi"
	d2$on_farm <- TRUE 
	d2$is_survey <- FALSE
	d2$yield_part <- "seed"
	d2 <- d2[,c("country","site","harvest_date","crop","treatment","biomass_total","plant_density","yield","yield_part","on_farm","is_survey")]
	
	#joining tables
	d5 <- carobiner::bindr(d, d1, d2)
	d5$dataset_id <- dataset_id
	d5$trial_id <- paste(1:nrow(d5),d5$treatment, sep = "_")
	d5$harvest_date <- as.character(d5$harvest_date)
	
	d5 <- d5[, c("country","adm1","site","latitude","longitude","treatment","harvest_date","crop","yield_part","intercrops","on_farm","is_survey","residue_yield","yield","plant_density","biomass_total","dataset_id","trial_id")]

	
	# all scripts must end like this
	carobiner::write_files(dset, d5, path=path)
}



