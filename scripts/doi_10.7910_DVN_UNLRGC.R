# R script for "carob"

## ISSUES
# not clear what is meant with replicate. See  table(d$Rep) 
# not clear what is meant with "season" (1 or 2) needs to be changed into months


carob_script <- function(path) {

"
Description
	Kihara, J., Huising, J., Nziguheba, G. et al. Maize response to macronutrients and potential for profitability in sub-Saharan Africa. Nutr Cycl Agroecosyst 105, 171–181 (2016).

	Abstract: The objective of this study was to determine the attainable maize grain response to and potential of profitability of N, P and K application in SSA using boundary line approaches. Data from experiments conducted in SSA under AfSIS project (2009–2012) and from FAO trials database (1969–1996) in 15 countries and constituting over 375 different experimental locations and 6600 data points are used. 

	There are two datasets, AFSIS and FAO. 

	AFSIS has more detail than the public AFSIS data so we used these. 
	AFSIS has data for five countries, each with one or two sites. Sites have subsites, referred to as 'cluster' . 

Notes 
	citation could be improved to include the underlying data sources.
"


	## Process 
	uri <- "doi:10.7910/DVN/UNLRGC"
	cleanuri <- carobiner::clean_uri(uri)
	rawpath <- file.path(path, "data/raw")
	dataset_id <- paste0(cleanuri, "-afsis")
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   uri=uri,
	   publication="doi:10.1007/s10705-015-9717-2",
	   contributor="Camila Bonilla",
	   experiment_type="fertilizer",
	   has_weather=FALSE,
	   has_management=FALSE
	)

	## treatment level data 
	ff <- agro::get_data_from_uri(uri, path=rawpath)

	## read the json for license, terms of use  
	js <- carobiner::get_json(cleanuri, rawpath, major=1, minor=3)
	dset <- carobiner::get_license(js, dset)

	## the AFSIS data 

	f <- ff[basename(ff) == "Kihara et al.2015_AFSIS_Data.xlsx"]
	d <- as.data.frame(readxl::read_excel(f))
	#"Site', 'Cluster', 'Season', 'Field', 'FieldID', 'TrtDesc', 'TCrop', 'FLat', 'FLong', 'Plot', 'PlotID', 'Trt', 'Rep', 'HarvestArea', 'TGrainYld_Uncorr"
	
	# not clear what is meant with replicate. See  table(d$Rep) 
	
	d <- d[, c('Site', 'Cluster', 'FieldID', 'TrtDesc', 'Rep', 'TCrop', 'FLat', 'FLong', 'TGrainYld_Uncorr')]
	colnames(d) <- c('site', 'subsite', 'field', 'treatment', 'rep', 'crop', 'latitude', 'longitude', 'yield')
	d[d=="NA"] <- NA
	d$trial_id <- paste0("AFSIS_", d$field)

	lat <- as.numeric(d$latitude)
	lon <- as.numeric(d$longitude)
	i <- which(abs(lat) > 90)
	lat[i] <- as.numeric(paste0(substr(d$latitude[i], 1, 2), ".", substr(d$latitude[i], 3, 6)))
	lon[i] <- -1 * as.numeric(paste0(substr(d$longitude[i], 1, 1), ".", substr(d$longitude[i], 2, 6)))
	d$latitude <- round(lat, 5)
	d$longitude <- round(lon, 5)
	d$crop <- tolower(d$crop)

	d$start_date <- substr(d$field, 5, 8)	
	i <- d$start_date == '10LR' | d$start_date == '10SR'
	# months can be estimated from LR and SR 
	d$start_date[i] <- "2010"
	d$start_date <- as.numeric(d$start_date)

	d$yield <- round(d$yield*1000)
		
	d$K_fertilizer <- d$P_fertilizer <- d$N_fertilizer <- 0
	d$N_fertilizer[grep("N", d$treatment)] <- 100
	d$K_fertilizer[grep("K", d$treatment)] <- 60 
	d$P_fertilizer[grep("P", d$treatment)] <- 30
	#d$fertilizer <- ""

	d$country <- NA
	d$country[d$site=="Nkhata Bay" | d$site=="Thuchila" | d$site=="Kasungu"] <- "Malawi"
	d$country[d$site=="Kiberashi"| d$site=="Mbinga"] <- "Tanzania"
	d$country[d$site=="Finkolo"] <- "Mali"
	d$country[d$site=="Pampaida"] <- "Nigeria"
	d$country[d$site=="Sidindi"] <- "Kenya"

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$field <- NULL
	
	carobiner::write_files(dset, d, path, cleanuri, "afsis")

	## FAO data 

	f <- ff[basename(ff) == "Kihara et al.2015_All_Sites.xlsx"]
	crds <- as.data.frame(readxl::read_excel(f))
	crds <- crds[crds$Dataset=="FAO", c("TrialID", "Lat", "Long", "SoilType")]
	
	f <- ff[basename(ff) == "Kihara et al.2015_FAO_Data.xlsx"]
	z <- as.data.frame(readxl::read_excel(f))
	z <- merge(z, crds, by="TrialID")
	
	names(z) <- tolower(names(z))
	z$zone <- carobiner::capitalize_words(z$zone)
	z$site <- carobiner::capitalize_words(z$site)
	z$year[z$year=="87B"] <- "1987"
	z$year[z$year=="88A"] <- "1988"
	z$start_date <- z$year
	i <- grep("-", z$year)
	z$start_date[i] <- substr(z$year[i], 1, 4)
	z$end_date <- NA
	z$end_date[i] <- paste0("19", substr(z$year[i], 6, 8))
	
# 'trialid', 'country', 'zone', 'site', 'year', 'season', 'trial', 'nid', 'ycontrol_abs', 'yield', 'pcontrol', 'n control', 'n', 'p2o5', 'p', 'nae', 'pae', 'k2o', 'design', 'fym', 'classes', 'lat', 'long', 'soiltype', 'start_date', 'end_date'

	z$k <- z$k2o * 0.8301
	sel <- c('country', 'zone', 'site', 'year', 'season', 'nid', 'ycontrol_abs', 'yield', 'pcontrol', 'n control', 'n', 'p', 'k', 'fym', 'lat', 'long', 'soiltype', 'start_date', 'end_date')
	z <- z[, sel]
	z <- carobiner::change_names(z, "nid", "trial_id")
	
	fcontrol <- c('country', 'zone', 'site', 'year', 'season', "trial_id", 'ycontrol_abs', 'fym', 'lat', 'long', 'soiltype', 'start_date', 'end_date')
	ctr <- unique(z[, fcontrol])
	ctr <- carobiner::change_names(ctr, "ycontrol_abs", "yield")
	ctr$n <- 0
	ctr$p <- 0
	ctr$k <- 0

	pcontrol <- c('country', 'zone', 'site', 'year', 'season', "trial_id", 'pcontrol', 'n', 'k', 'fym', 'lat', 'long', 'soiltype', 'start_date', 'end_date')
	pctr <- unique(z[, pcontrol])
	pctr <- carobiner::change_names(pctr, "pcontrol", "yield")
	pctr$p <- 0

	ncontrol <- c('country', 'zone', 'site', 'year', 'season', "trial_id", 'n control', 'p', 'k', 'fym', 'lat', 'long', 'soiltype', 'start_date', 'end_date')
	nctr <- unique(z[, ncontrol])
	nctr <- carobiner::change_names(nctr, "n control", "yield")
	nctr <- nctr[!is.na(nctr$yield), ]
	nctr$n <- 0
	
	zctr <- unique(rbind(ctr, nctr, pctr))

	z$"pcontrol" <- NULL
	z$"n control" <- NULL
	z$ycontrol_abs <- NULL
	z <- z[!is.na(z$yield), ]

	zz <- unique(rbind(z, zctr))
	zz <- zz[order(zz$trial_id, zz$n, zz$p, zz$k), ]

	zz <- carobiner::change_names(zz, 
	c("zone", "year", "n", "p", "k", "fym", "lat", "long"), 
	c("region", "start_date", "N_fertilizer", "P_fertilizer", "K_fertilizer", "FYM", "latitude", "longitude"))

	dataset_id <- paste0(cleanuri, "-fao")

	dset$dataset_id <- dataset_id
	zz$dataset_id <- dataset_id
	zz$on_farm <- NA
	zz$is_survey <- FALSE

	carobiner::write_files(dset, zz, path, cleanuri, "fao")

	TRUE
}


