
# fertilizer amount is not OK


carob_script <- function(path) {
	
	"Description:

	N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project. The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.

"
	
	uri <- "doi:10.25502/A856-V212"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		uri=uri,
		publication= NA, 
		data_citation = "Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, P., Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira, L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller, K., Ampadu-Boakye, T., & Heerwaarden, J. van. (2020). N2Africa impact survey - Nigeria, 2013 [dataset]. International Institute of Tropical Agriculture (IITA). 
		https://doi.org/10.25502/A856-V212" ,
		data_institutions = "IITA",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-08-20",
		data_type="survey",
		project=NA 
	)
	
	## download and read data 
	
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)
	
	
	f <- ff[basename(ff) == "a_general_1.csv"] 
	f1 <- ff[basename(ff) == "a_general_2.csv"] 
	f6 <- ff[basename(ff) == "c_land_holding_management.csv"]
	f7 <- ff[basename(ff) == "d_crop_production_use.csv"]
	f10 <- ff[basename(ff) == "e_changes_legume_haulm_process.csv"]
	f12 <- ff[basename(ff) == "e_changes_production_use_2.csv"]
	# read the dataset
	r <- read.csv(f)
	r1 <- read.csv(f1)
	r6 <- read.csv(f6)
	r7 <- read.csv(f7)
	r10 <- read.csv(f10)
	r12 <- read.csv(f12)
	
	
	## process file(s)
	d <- r[, c("farm_id","country","sector_state","action_site","village","gps_latitude","gps_longitude","gps_latitude_dec","gps_longitude_dec")] 
	
	colnames(d) <- c("trial_id", "country","location","adm1","adm2","latitude1","longitude1","latitude2","longitude2")
	
	# fix long and lat
	i <- is.na(d$latitude1)
	d$latitude1[i] <- d$latitude2[i]
	
	i <- is.na(d$longitude1)
	d$longitude1[i] <- d$longitude2[i]
	d$longitude <- d$longitude1
	d$latitude <- d$latitude1
	
	d1 <- r6[, c("farm_id","farm_size_ha","inoculant_type","other_min_fert_type")] 
	
	colnames(d1) <- c("trial_id","farm_size","inoculation_type","fertilizer_type")
	
	d2 <- r7[, c("farm_id", "crop","total_production_farm","weight_unit")] 
	
	colnames(d2) <- c("trial_id", "crop","yield1","yield_unit")
	
	#merge d1 and d
	d2 <- merge(d1,d2,by="trial_id")
	# process 
	d3 <- r12[,c("farm_id","legume_area_now_ha","crop","yield_amount_now","yield_unit_now")]
	colnames(d3) <- c("trial_id","farm_size","crop","yield1","yield_unit")
	d3$inoculation_type <- NA
	d3$fertilizer_type <- NA
	# append d3 and d2
	d3 <- rbind(d3,d2)
	# remove character in yield value 
	d3 <- d3[d3$yield1!="7BAGS" & d3$yield1!="8BAGS" & d3$yield1!="4BAGS" & d3$yield1!="9BAGS" & d3$yield1!="5BAGS",]
	d3$yield1[d3$yield1=="5O"] <- 50
	d3$yield1 <- as.numeric(d3$yield1)
	d3$yield_unit <- tolower(d3$yield_unit)
	d3$yield <- ifelse((d3$yield_unit=="kg") & (d3$farm_size > 0), d3$yield1/d3$farm_size, NA)
	d3 <- d3[!is.na(d3$yield), ]
	
	# merge d and d3
	d <- merge(d, d3, bx="trial_id", all.y = TRUE)
	d$inoculated <- FALSE
	d$inoculated[!is.na(d$inoculation_type)| d$inoculation_type !=""] <- TRUE
	d <- d[, c("country", "trial_id", "location","adm1","adm2","longitude", "latitude","crop", "yield","fertilizer_type","inoculated")]
	
	# # EGB: These are not OM applications, but more like the residue use.
	# # In any case none of the values indicate it was integrated in the field. Therefore, removing.
	# # process
	# d4 <- r10[,c("farm_id","haulm_use_now")]
	# colnames(d4) <- c("trial_id","OM_type")
	#merge d and d4
	# d <- merge(d,d4,by="trial_id",all.x = T)
	# Add columns
	d$dataset_id <- dataset_id
	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$irrigated <- FALSE
	# fix lon and Lat 
	d$latitude[d$adm2=="Asako"] <- 7.61667
	d$longitude[d$adm2=="Asako"] <- 3.48333
	d$latitude[d$adm2=="Tashan nabargi"] <- 10.9832503
	d$longitude[d$adm2=="Tashan nabargi"] <- 8.0588025

	# Fix fertilizer_type
	p <- carobiner::fix_name(d$fertilizer_type,"lower")
	p <- gsub("npk urea","NPK; urea",p)
	p <- gsub("npk/urea","NPK; urea",p)
	p <- gsub("n.p.k., s. s. p.","NPK; SSP",p)
	p <- gsub("nplc","NPK",p)
	p <- gsub("npk and urea","NPK; urea",p)
	p <- gsub("npk / urea","NPK; urea",p)
	p <- gsub("n.p.k., urea","NPK; urea",p)
	p <- gsub("n.p.k.,","NPK",p)
	p <- gsub("npk, urea" ,"NPK; urea",p)
	p <- gsub("n.p.k."	 ,"NPK",p)
	p <- gsub("npk,urea"	 ,"NPK; urea",p)
	p <- gsub("npk,"	 ,"NPK",p)
	p <- gsub("nok"		,"NPK",p)
	p <- gsub("biocide","none",p)
	p <- gsub("y","none",p)
	p <- gsub("npk"	,"NPK",p)
	p <- gsub("ssp"	,"SSP",p)
	p[p == "sononea bean ,inoculant"] <- NA


	d$fertilizer_type <- p	
	#add fertilizer
	d$N_fertilizer <- 0
	d$P_fertilizer <- 0
	d$K_fertilizer <- 0
	i <- d$fertilizer_type=="NPK 15 15 15"
	d$N_fertilizer[i] <- 15
	d$P_fertilizer[i] <- 15/2.29
	d$K_fertilizer[i] <- 15/1.2051

	i <- d$fertilizer_type %in% c("NPK 20.10.10", "NPK 20:10:10", "NPK")
	d$N_fertilizer[i] <- 20
	d$P_fertilizer[i] <- 20/2.29
	d$K_fertilizer[i] <- 20/1.2051
	
	i <- d$fertilizer_type %in% c("20:10:10, 46%urea", "20:10:0,46:0:0", "20:10:10, 46:0:0", "20:10:10,46:0:0", "20:10:10,46%n", "NPK; urea")
	d$N_fertilizer[] <- 20+46
	d$P_fertilizer[i] <- 10/2.29
	d$K_fertilizer[i] <- 10/1.2051

## todo: set fertilizer to NA if the amount > 0 but product unknown 
##       set fertilizer to 0 if the amount == 0
	
	# fix fertilizer type
	d$fertilizer_type[d$fertilizer_type %in% c("NPK 15 15 15", "NPK 20.10.10", "NPK 20:10:10", "NPK")] <- "NPK"
	d$fertilizer_type[d$fertilizer_type %in% c("20:10:10, 46%urea", "20:10:0,46:0:0", "20:10:10, 46:0:0","20:10:10,46:0:0", "20:10:10,46%n")] <- "NPK; urea"
	d$fertilizer_type[d$fertilizer_type==""] <- NA
	
	#fix country name
	d$country <- carobiner::fix_name(d$country, "title")

	
	
	# fix crop name 
	p <- carobiner::fix_name(d$crop,"lower")
	p[p %in% c("0.4ha", "0.6ha", "crop", "sorghum cowpea", "sorghum, g/nut", "sorghum, soyabean")] <- NA
	p[grep("^cow", p)] <- "cowpea"
	p[grep("pea$", p)] <- "cowpea"
	p[grep("nut$", p)] <- "groundnut"
	p <- gsub("groundut" ,"groundnut",p)
	p[grep("soy", p)] <- "soybean"
	p <- gsub("s/bean" ,"soybean",p)
	p[grep("inger", p)] <- "finger millet"
	p[grep("millet", p)] <- "finger millet"
	p <- gsub("cassaca","cassava",p)
	p <- gsub("tomatoes" ,"tomato",p)
	p <- gsub("guinea corn" ,"maize",p)
	p <- gsub("hot pepper", "chili pepper" ,p)
	p <- gsub("sorgum"	,"sorghum" ,p)
	d$crop <- p
		
	# remove rows with two crop in one cell but just one value of yield
	d <- d[!grepl(",", d$crop), ]
	d <- d[!(d$crop %in% c("crop", "sorghum cowpea", "0.6ha", "0.4ha")), ]
	
	#fix crop yield limit by crop
	d$yield[d$crop=="groundnut" & d$yield > 10000] <- NA
	d$yield[d$crop=="cowpea" & d$yield > 10000] <- NA
	d$yield[d$crop=="sorghum" & d$yield > 10000] <- NA

	d <- d[!is.na(d$crop), ]
	
	# fix whitespace in variable
	d$adm1[d$adm1==""] <- NA
	d$adm2[d$adm2==""] <- NA

	#is this the best we can do (not even year)?
	## d$planting_date = as.character(NA)
	
	d$yield_part <- "seed"
	# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

