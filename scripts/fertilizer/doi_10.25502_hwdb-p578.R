#################################################################################
#N2Africa was aimed at increasing biological nitrogen fixation and productivity
#of grain legumes through effective production technologies including inoculants
#and fertilizers adapted to local settings which was aimed at increasing soil
#fertility.The trails were conducted in 11 African countries
#################################################################################

carob_script <- function(path){
 
	uri <- "doi.org/10.25502/hwdb-p578"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	 
	#dataset level data
	
	dset <- data.frame(
		dataset_id = dataset_id,
		group = group,
		uri = uri,
        project="N2Africa",
		publication = NA,
		data_citation ="Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, P., Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira, L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, 
		F., Giller, K., Ampadu-Boakye, T., & Heerwaarden, J. van. (2020). N2Africa farm 
		monitoring - Mozambique, 2012 - 2013 [Data set]. International Institute of 
		Tropical Agriculture (IITA). https://doi.org/10.25502/HWDB-P578",

		carob_contributor = "Effie Ochieng",
		experiment_type = "variety_trials",
		has_weather =	TRUE,
		has_management = FALSE
	)
	
	#Registering the dataset
	ff <- carobiner::get_data(uri,path,group)
	js <- carobiner::get_metadata(dataset_id, path, group, major = 1, minor = 0)
	dset$license <- carobiner::get_license(js) 
	
	# read the data
	bn <- basename(ff)
	f <- ff[bn == "a_general.csv"]
	d <- read.csv(f)
	
	f1 <- ff[bn == "c_use_of_package_1.csv"]
	d1 <- read.csv(f1)
	
	f2 <- ff[bn == "e_harvest.csv"]
	d2 <- read.csv(f2)
	
	f3 <- ff[bn == "c_use_of_package_3.csv"]
	d3 <- read.csv(f3) 
	
	f4 <- ff[bn == "d_cropping_calendar.csv"]
	d4 <- read.csv(f4)
	
	f5 <- ff[bn == "b_info_site_2.csv"]
	d5 <- read.csv(f5)
	
	
	
	#start processing the 1st data
	d <- d[,c("farm_id","season","country","action_site","sector_ward","vilage","gps_latitude","gps_longitude")]
	
	d$trial_id <- d$farm_id
	d$season <- d$season
	d$country <- d$country
	v <- carobiner::fix_name(d$action_site, "Title")
	v <- carobiner::replace_values(v, c("Maica","Tsangano-Fonte boa"),
	                                 c("Matica", "Tsangano"))
	d$adm2 <- v
	w <- carobiner::replace_values(d$sector_ward, c( "Agonia", "Calipo/Muhua","Calipo/Mucua", "Calipo/Marasse", "Calipo/Mirasse","Calopo/Marrasse","Calipo/ Marasse","Calipo/ Muhua","Calipo/ Mirasse","Calipo/Maleliha","Uerro","Marasse"),
	                                             c("Angonia","Muhua","Mucua","Mirrasse","Mirrasse","Mirrasse","Mirrasse","Muhua","Mirrasse","Calipo","Uorra","Mirrasse"))
	d$adm3 <- w
	x <- carobiner::fix_name(d$vilage, "Title")
	x <- carobiner::replace_values(d$vilage, c("Siwama", "Gurue-UP2","Murimo","Lioma-Namiepe","Lioma-Nintulo","Madiea","Niza","Mpupha","Namurekele","Mulosa","Namphi","Nahaco","Pwasiua","Mugunuwa","Jordan", "Vaiya","Nvine","Ruace-pissi","Nakuilo","Jordao","Mwetxo", "Mpupwa","Mohiyera","Viola 2"),
	 c("Manica","Gurue","Murrimo","Namiepe","Nintulo","Madea","Zembe","Gurue","Gurue","Gurue","Gurue","Gurue","Gurue","Gurue","Gurue","Gurue","Gurue","Pissi","Gurue","Gurue","Gurue","Gurue","Gurue","Viola"))
	
	d$site <- x
	
	# selecting the areas with no coordinates get the trial_ids
#	lat_lon1 <- d[is.na(d$gps_latitude) | is.na(d$gps_longitude), ]
	
	# the given latitudes were not negative
	d$gps_latitude <- -(d$gps_latitude)
	
	
 # creating a data frame with the unique entries and the latitudes and longitudes, coordinates hard coded
	
  lat_lon <- data.frame(trial_id = c("FM_MOZ0752_TEC","FM_MOZ0751_TEC",	"FM_MOZ0750_TEC",	"FM_MOZ0749_TEC", "FM_MOZ0748_TEC", "FM_MOZ0747_TEC", "FM_MOZ0745_TEC", "FM_MOZ0746_TEC", "FM_MOZ0744_TEC", "FM_MOZ0743_TEC", "FM_MOZ0742_TEC", "FM_MOZ0741_TEC", "FM_MOZ0740_TEC", 
  "FM_MOZ0738_TEC","FM_MOZ0739_TEC", "FM_MOZ0737_TEC", "FM_MOZ0736_TEC", "FM_MOZ0735_TEC", "FM_MOZ0734_TEC", "FM_MOZ0732_TEC", "FM_MOZ0733_TEC", "FM_MOZ0731_TEC", "FM_MOZ0730_TEC", "FM_MOZ0729_TEC", "FM_MOZ0728_TEC", "FM_MOZ0727_TEC", "FM_MOZ0725_TEC","FM_MOZ0726_TEC", "FM_MOZ0724_TEC", "FM_MOZ0723_TEC", "FM_MOZ0722_TEC", "FM_MOZ0720_TEC", "FM_MOZ0721_TEC", "FM_MOZ0719_TEC", "FM_MOZ0718_TEC", "FM_MOZ0717_TEC", "FM_MOZ0716_TEC", "FM_MOZ0715_TNS", "FM_MOZ0714_TNS", "FM_MOZ0713_TNS","FM_MOZ0712_TNS", "FM_MOZ0711_TNS", "FM_MOZ0710_TNS", "FM_MOZ0709_TNS", "FM_MOZ0708_TNS", "FM_MOZ0707_TNS", "FM_MOZ0706_TNS", "FM_MOZ0705_TNS", "FM_MOZ0705_TEC", "FM_MOZ0704_TNS", "FM_MOZ0704_TEC", "FM_MOZ0702_ZAM", "FM_MOZ0701_ZAM","FM_MOZ0700_ZAM", "FM_MOZ0699_ZAM", "FM_MOZ0698_ZAM", "FM_MOZ0154_MAN", "FM_MOZ0150_MAN", "FM_MOZ0139_MAN", "FM_MOZ0135_MAN", "FM_MOZ0134_MAN", "FM_MOZ0132_MAN", "FM_MOZ0130_ZAM", "FM_MOZ0128_MAN", "FM_MOZ0127_MAN", "FM_MOZ0126_MAN","FM_MOZ0124_ZAM", "FM_MOZ0123_ZAM", "FM_MOZ0122_ZAM", "FM_MOZ0121_ZAM", "FM_MOZ0120_ZAM", "FM_MOZ0019_NPL", "FM_MOZ0020_NPL", "FM_MOZ0022_NPL", "FM_MOZ0024_NPL", "FM_MOZ0029_NPL", "FM_MOZ0030_NPL", "FM_MOZ0031_NPL", "FM_MOZ0032_NPL","FM_MOZ0077_ZAM", "FM_MOZ0078_ZAM", "FM_MOZ0079_ZAM", "FM_MOZ0080_ZAM", "FM_MOZ0081_ZAM", "FM_MOZ0082_ZAM", "FM_MOZ0083_ZAM", "FM_MOZ0084_ZAM", "FM_MOZ0085_ZAM", "FM_MOZ0086_ZAM", "FM_MOZ0087_ZAM", "FM_MOZ0088_ZAM", 
   "FM_MOZ0089_ZAM","FM_MOZ0090_ZAM", "FM_MOZ0091_ZAM", "FM_MOZ0093_ZAM", "FM_MOZ0094_ZAM", "FM_MOZ0095_ZAM", "FM_MOZ0096_ZAM", "FM_MOZ0097_ZAM", "FM_MOZ0098_ZAM", "FM_MOZ0099_ZAM", "FM_MOZ0100_ZAM", "FM_MOZ0101_ZAM", "FM_MOZ0102_ZAM", "FM_MOZ0103_ZAM","FM_MOZ0104_ZAM", "FM_MOZ0105_ZAM", "FM_MOZ0106_ZAM", "FM_MOZ0107_ZAM", "FM_MOZ0108_ZAM", "FM_MOZ0109_ZAM", "FM_MOZ0110_ZAM", "FM_MOZ0111_ZAM", "FM_MOZ0112_ZAM", "FM_MOZ0113_ZAM", "FM_MOZ0114_ZAM", "FM_MOZ0115_ZAM", "FM_MOZ0116_ZAM","FM_MOZ0117_ZAM", "FM_MOZ0118_ZAM", "FM_MOZ0119_ZAM"),
   
   site = c( "Mossurize", "Mossurize", "Barue", "Barue", "Barue", "Barue", "Manica", "Barue", "Ulongue", "Manica", "Manica", "Manica", "Macanga", "Macanga", "Macanga", "Macanga", "Macanga", "Macanga", "Macanga", "Macanga", "Macanga", "Macanga", "Tsangano", "Tsangano", "Tsangano", "Tsangano", 
    "Tsangano", "Tsangano", "Tsangano", "Tsangano", "Tsangano", "Domue", "Domue", "Domue", "Domue", "Domue", "Domue", "Domue", "Domue", "Ulongue", "Ulongue", "Ulongue", "Ulongue", "Ulongue", "Ulongue", "Ulongue", "Ulongue", "Ulongue", "Nintulo", "Ulongue", "Lioma", "Serra", "Ewarelo", "Mutequelesse", "Tetete", "Tetete", "Mussacumbira", "Mussacumbira", "Zembe", "Zembe", "Zembe", "Zembe", "Gurue", "Zembe", "Zembe", 
    "Zembe", "Ewarelo", "Ruace", "Ruace", "Gurue", "Ruace", "Nametil", "Nametil", "Nametil", "Nametil", "Nametil", "Nametil", "Nametil", "Nametil", "Gurue", "Gurue", "Gurue", "Gurue", "Lioma", "Ruace", "Gurue", "Ruace", "Ruace", "Mutequelesse", "Gurue", "Gurue", "Gurue", "Pissi", "Mutequelesse", "Murrimo", "Gurue", "Magige", "Gurue", "Gurue", "Magige", "Gurue", "Gurue", "Mutequelesse", "Gurue", 
    "Gurue", "Mutequelesse", "Gurue", "Gurue", "Gurue", "Mutequelesse", "Gurue", "Ruace", "Ruace", "Tetete", "Tetete", "Gurue", "Ruace", "Magige", "Gurue", "Gurue", "Ruace"),
                            
    lat = c(-21.21, -21.21, -17.50, -17.50, -17.50, -17.50, -19.00, -17.50, -14.72, -19.00, -19.00, -19.00, -14.68, -14.68, -14.68, -14.68, -14.68, -14.68, -14.68, -14.68, -14.68, -14.68, -15.16, -15.16, -15.16, -15.16, -15.16, -15.16, -15.16, -15.16, -15.16, -14.47, -14.47, -14.47, -14.47, -14.47, -14.47, -14.47, -14.47, -14.72, -14.72, -14.72, -14.72, -14.72, -14.72, -14.72, -14.72, -14.72, -15.09, -14.72, -15.17, -16.47, -15.16, -19.00, -15.38, -15.38, -13.43, -13.43, -19.29, -19.29, -19.29, -19.29, -15.46, -19.29, -19.29, -19.29, -15.16, -14.91, -14.91, -15.46, -14.91, -14.21, -14.21, -14.21, -14.21, -14.21, -14.21, -14.21, -14.21, -15.46, -15.46, -15.46, -15.46, -15.17, -14.91, -15.46, -14.91, -14.91, -19.00, -15.46, -15.46, -15.46, -14.90, -19.00, -15.37, -15.46, -15.32, -15.46, -15.46, -15.32, -15.46, -15.46, -19.00, -15.46, -15.46, -19.00, -15.46, -15.46, -15.46, -19.00, -15.46, -14.91, -14.91, -15.38, -15.38, -15.46, -14.91, -15.32, -15.46, -15.46, -14.91),
    
    lon = c(33.38, 33.38, 33.65, 33.65, 33.65, 33.65, 33.5, 33.65, 34.36, 33.5, 33.5, 33.5, 32.73,  32.73	,32.73, 32.73, 32.73, 32.73, 32.73, 32.73, 32.73, 32.73, 34.5, 34.5, 34.5, 34.5,  34.5, 34.5, 34.5,34.5, 34.5, 34.2, 34.2, 34.2, 34.2, 34.2, 34.2, 34.2, 34.2,  34.36, 34.36, 34.36, 34.36, 34.36, 34.36, 34.36, 34.36, 34.36, 37.1, 34.36, 36.8, 35.7,  36.94, 33.5, 36.5, 36.5, 38.6, 38.6, 33.35, 33.35, 33.35, 33.35, 36.98, 36.98, 33.35,  33.35, 36.94, 36.35, 36.35, 36.98, 36.35, 40.55, 40.55, 40.55, 40.55, 40.55, 40.55, 40.55,  40.55, 36.98, 36.98, 36.98, 36.98, 36.8, 36.35, 36.98, 36.35, 36.35, 33.5, 36.98, 36.98,  36.98, 36.6, 33.5, 36.8,36.98, 36.7, 36.98, 36.98, 36.7, 36.98, 36.98, 33.5, 36.98,  36.98, 33.5, 36.98, 36.98, 36.98, 33.5, 36.98, 36.35, 36.35, 36.5, 36.5, 36.98, 36.35, 36.7, 36.98, 36.98, 36.35))   
  
	d <- merge(d, lat_lon, by=c("trial_id", "site"), all.x=TRUE)
	d$latitude <- d$gps_latitude
	d$longitude <- d$gps_longitude
	ina <- is.na(d$latitude) | is.na(d$longitude)
	d$longitude[ina] <- d$lon[ina]
	d$latitude[ina] <- d$lat[ina]
	d$lat <- d$lon <- NULL
    
    d <- d[, c("trial_id","country","adm2","site","latitude","longitude")]
  
  
  #subset the variables of interest in d1 and d2
  
  d1 <- d1[, c("farm_id","plot_no","plot_size","crop","variety","inoculant_used","min_fertilizer_type","min_fertiliser_amount_kg")]
  d2 <- d2[, c("farm_id","plot_no","area_harvested_m2","weight_kg")]
  
  dd <- merge(d1,d2, by = c("farm_id","plot_no"), all.x = T )
  
  # working on fertilizer types
  dd$min_fertilizer_type[dd$min_fertilizer_type %in% c("SSP/Urea", "SSP+Ureia", "SSP+Urea",  "Urea+SSP")] <- "SSP; urea"	
  dd$min_fertilizer_type[dd$min_fertilizer_type %in% c("ssp", "Phosphor(SSP)", "SSP+Inoc","Y","12")] <- "SSP"
  dd$min_fertilizer_type[dd$min_fertilizer_type %in% c("Ureia", "Urea")] <- "urea"
  dd$min_fertilizer_type[dd$min_fertilizer_type %in% c( "None", "", "N")] <- "none"
  
  #working on fertilizer amounts
  dd$min_fertiliser_amount_kg[dd$min_fertiliser_amount_kg %in% c("2/0.4","2 /0.4","2/0,4","2/ 0,4")] <- "2/0.4"
  v <- carobiner::replace_values(dd$min_fertiliser_amount_kg,c("2kg/ha","","N"),                     c("2","0","0"))
  dd$min_fertiliser_amount_kg <- v
  
  #split fertilizer amount column to separate urea amounts and SSP amounts
  dd$ssp_amt <- ifelse(dd$min_fertilizer_type != "urea", as.numeric(sub("/.*", "", dd$min_fertiliser_amount_kg)), 0)
  dd$urea_amt <- ifelse(dd$min_fertilizer_type == "urea", as.numeric(sub(".*?/", "", dd$min_fertiliser_amount_kg)), 0)
  dd$fertilizer_type <- dd$min_fertilizer_type
  #to get rates of N and P
  dd$P_rate_plot <-dd$ssp_amt*0.16
  v <- carobiner::replace_values(dd$area_harvested_m2, c(101,102,103,104),              c(100,100,100,100))
  v[is.na(v)] <- 100
  dd$area_harvested_m2 <-v
  dd$P_fertilizer <- (10000/dd$area_harvested_m2) * dd$P_rate_plot
  dd$N_rate_plot <- dd$urea_amt*0.467
  dd$N_fertilizer <-(10000/dd$area_harvested_m2) * dd$N_rate_plot
  dd$K_fertilizer <- 0
  #getting the yield
  dd$yield <- (10000/dd$area_harvested_m2) * dd$weight_kg
  
  #correcting mismatched rows in inoculated
  v <- 1379:1386
  dd$inoculant_used[v] <- ifelse(dd$inoculant_used[v] == "", dd$variety[v], dd$inoculant_used[v])
  dd$inoculated <- ifelse(dd$inoculant_used == "Y",TRUE,FALSE)
  
  #standardizing the crops and variety
  v <- 570:573
  dd$crop[v] <- ifelse(dd$crop[v]=="100",dd$variety[v],dd$crop[v])
  v <- carobiner::replace_values(dd$crop, c("Groundnuit","Groudnuit","","Soybean","Soybean (Farmer variety)","Soyben","Soybean PD1","Soybean PD2"),   c("groundnut","groundnut","groundnut","soybean","soybean","soybean","soybean","soybean"))
  
  dd$crop <- v
  w <- carobiner::fix_name(dd$variety, "title")
  dd$variety <- w
  dd$trial_id <- dd$farm_id
  dd$rep <- dd$plot_no
 
	
	dd <- dd[, c("trial_id","rep","crop","variety","inoculated","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","yield")]
	
	#get the spacing information
	d3$trial_id <- d3$farm_id
	d3$row_spacing <- as.numeric(d3$crop_1_spacing_row_to_row)
	d3$plant_spacing <- as.numeric(d3$crop_1_spacing_plant_to_plant)

	d3 <- d3[, c("trial_id","row_spacing","plant_spacing")]
	
	
	#standardizing the previous crop variable
	d5$trial_id <- d5$farm_id
	d5$previous_crop <- d5$main_crop_last_season
	p <- carobiner::fix_name(d5$previous_crop, "lower")
	p[grep("^grou", p)] <- "groundnut"
	
	p <- gsub("soyben", "soybean", p)
	p <- gsub("tobaco", "tobacco", p)
	p <- gsub("beans", "common bean", p)
	p <- gsub("irish potatoes", "potato", p)
	p <- gsub("sweet potatoes", "sweetpotato", p)
	p <- gsub(", ", "; ", p)
	p <- gsub(" ma$", " maize", p)

	d5$previous_crop <- p
	d5 <- d5[, c("trial_id","previous_crop")]
	
	#get the dates information
	d4$trial_id <- d4$farm_id

	#h <-  subset(d4, grepl("harvest", activity))
	
	p <- d4[grepl("planting", d4$activity), ]
	h <- d4[grepl("harvest", d4$activity), ]
	p$start_date <- with(p, paste(date_planting_yyyy, date_planting_mm, date_planting_dd, sep = "-"))
	h$end_date <- with(h, paste(date_planting_yyyy, date_planting_mm, date_planting_dd, sep = "-"))
	p <- p[, c("trial_id","start_date")]
	h <- h[, c("trial_id","end_date")]
    d4 <- merge(p,h, by = "trial_id")
  
  ## RH: do not remove data for that reason. But do try to fix.. 
  #remove rows with no planting and harvest info
  ##d4 <- subset(d4, !(start_date == "0-0-0" & end_date == "0-0-0"))
	d4$start_date[d4$start_date == "0-0-0"] <- NA
	d4$start_date[d4$end_date == "0-0-0"] <- NA
	d4$start_date <- as.character(as.Date(d4$start_date, format = "%Y-%m-%d"))
	d4$end_date <- as.character(as.Date(d4$end_date, format = "%Y-%m-%d"))
  
  
	#merge the data sets
	q1 <- merge(d, dd, by = "trial_id")
	q2 <- merge(q1, d3, by = "trial_id")
	q3 <- merge(q2, d4, by = "trial_id")
	q4 <- merge(q3, d5, by = "trial_id")
  
	q4$dataset_id <- dataset_id
	
	q4 <- q4[, c("trial_id","country","adm2","site", "latitude","longitude","rep","crop","variety","inoculated","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","start_date","end_date", "row_spacing","plant_spacing","yield","previous_crop","dataset_id")]
	
	
	# all scripts should end like this
	carobiner::write_files(dset, q4, path, dataset_id, group)

}
