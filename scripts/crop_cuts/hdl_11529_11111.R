

carob_script <- function(path) {
	
	"
	Rice crop cuts conducted in Odisha (Mayurbhanj, Balasore, Keonjhar, Bhadrak, Khorda, Puri districts)

"
	
	uri <- "hdl:11529/11111"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "crop_cuts"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Wasim Iftikar; Nabakishore Parida; Vivek Kumar; Narayan Chandra Banik; Amit Mishra, 2017, Odisha Rice Crop Cut Data 2013 - 2016, https://hdl.handle.net/11529/11111, CIMMYT Research Data & Software Repository Network, V2",
		publication= NA,
		data_institutions = "CIMMYT",
		data_type="experiment", # or, e.g. "on-farm experiment", "survey", "compilation"
		carob_contributor="Effie Ochieng'",
		carob_date="2023-09-25"
	)
	
	## download and read data 
	
	ff	<- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=0)
	dset$license <- "CIMMYT license"
	
	
	f <- ff[basename(ff) == "CSISA_OD_RiceCropCut_AllYearRawDataFinal.csv"]
	r <- read.csv(f)

	d <- r[, c("Season","District_Name","CEM","VAR","DOS","DOT","DOH","Latitude","Longitude")]
	colnames(d) <- c("season","adm2","treatment","variety","planting_date","transplanting_date","harvest_date","latitude","longitude")

	d$dataset_id <- dataset_id
	d$trial_id <- paste(1:nrow(d), r$FID, sep = "_")
	d$country <- "India"
	d$adm1 <- "Odisha"
	d$crop <- "rice"
	d$yield_part <- "grain"	
	d$on_farm <- TRUE
	d$is_survey <- FALSE

	dap <- r$DAP_Total
	mop <- apply(r[, grep("MoP_", names(r))], 1, sum)
	urea <- apply(r[, grep("Urea_", names(r))], 1, sum)
	ft <- cbind(dap>0, mop>0, urea>0)
	ft[is.na(ft)] <- FALSE
	ft <- apply(ft, 1, \(i) paste(c("DAP", "KCl", "urea")[i], collapse="; "))
	ft[ft == ""] <- "none"
	ft[is.na(dap)] <- NA
	
 # 2.4711 to convert from ? to kg/ha 
	d$P_fertilizer <- dap * 2.4711 * .201
	d$K_fertilizer <- mop * 2.4711 * .498
	d$N_fertilizer <- dap * 2.4711 * 0.180 + urea * 2.4711 * 0.46

	biom <- r[, c("TAGB_A", "TAGB_B", "TAGB_C")]
	biom[biom=="-"] <- NA
	biom <- sapply(biom, as.numeric)
	biom <- rowMeans(biom, na.rm=TRUE)

	yld <- r[, c("Yield_A", "Yield_B", "Yield_C")]
	yld[yld=="-"] <- NA
	yld <- sapply(yld, as.numeric)
	yld <- rowMeans(yld, na.rm=TRUE)
	
	# efyrouwa : from dataset description
	#Grain weight 
	#Year 2013 - 5X5 sq meter in Kg 
	#Year 2014 - 2X2.5 sq meter in Kg 
	#Year 2015 - 1X1 sq meter in gm 
	#Year 2016 - 2X2 sq meter in gm
	# to calculate in kg/ha
	d$yield <- ifelse(r$Year == 2013, yld * 400,
			 ifelse(r$Year == 2014, yld * 2000,
			 ifelse(r$Year == 2015, yld * 10,  
			 yld * 2.5))) 
	
	
	#Total above ground weight 
	#Year 2013 - 5X5 sq meter in Kg Year 
	#2014 - 2X2.5 sq meter in Kg 
	#Year 2015 - 1X1 sq meter in gm 
	#Year 2016 - 2X2 sq meter in Kg
	# to calculate in kg/ha
	d$biomass_total <- ifelse(r$Year == 2013, biom * 400,
					 ifelse(r$Year == 2014, biom * 2000,
					 ifelse(r$Year == 2015, biom * 10, 
					 biom * 2500)))
	
	# RH: all the values above 10000 are really suspect relative to grain yield
	d$biomass_total[d$biomass_total > 50000] <- NA
	
	d$planting_date <- as.character(as.Date(d$planting_date, format = "%d-%b-%y")) 
	i <- is.na(d$planting_date)
	d$planting_date[i] <- as.character(r$Year[i])
	
	d$harvest_date <- as.character(as.Date(d$harvest_date, format = "%d-%b-%y")) 
	d$transplanting_date <- as.character(as.Date(d$transplanting_date, format = "%d-%b-%y")) 

	

# merging last because the order changes
	x <- data.frame(adm2 = c("Bhadrak", "Mayurbhanj", "Puri"), 
					lon = c(86.498, 86.3957, 85.8391), 
					lat = c(21.0666, 21.9156, 19.8068))

	d$latitude[d$latitude == "-"] <- NA
	d$longitude <- as.numeric(d$longitude)
	d$latitude <- as.numeric(d$latitude)
	i <- which(d$longitude < 80)
	d$longitude[i] <- d$latitude[i] <- NA

	d <- merge(d, x, by="adm2")
	d$longitude[is.na(d$longitude)] <- d$lon[is.na(d$longitude)]
	d$latitude[is.na(d$latitude)] <- d$lat[is.na(d$latitude)]
	d$lon <- d$lat <- NULL
	i = which(d$latitude > 50)
	tmp <- d$latitude[i]
	d$latitude[i] <- d$longitude[i]
	d$longitude[i] <- tmp
		
#	d <- d[, c("trial_id","dataset_id","country","adm1","adm2","latitude","longitude","crop","variety","planting_date","transplanting_date","harvest_date","treatment","yield_part","on_farm","is_survey","fertilizer_type", "N_fertilizer","P_fertilizer","K_fertilizer","biomass_total","yield")]
		# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}


