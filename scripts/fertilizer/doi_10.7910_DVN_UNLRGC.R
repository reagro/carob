# R script for "carob"

## ISSUES
# not clear what is meant with replicate. See  table(d$Rep) 
# not clear what is meant with "season" (1 or 2) needs to be changed into months


carob_script <- function(path) {

"Description
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
	cleanuri <- carobiner::simple_uri(uri)
	group <- "fertilizer"

	dataset_id <- paste0(cleanuri, "-afsis")
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		uri=uri,
		data_citation="Kihara, Job, 2016. Replication Data for: Maize response to macronutrients and potential for profitability in sub-Saharan Africa, https://doi.org/10.7910/DVN/UNLRGC",
		publication="doi:10.1007/s10705-015-9717-2",
		carob_contributor="Camila Bonilla",
		carob_date="2021-05-31",
		data_type="compilation",
		project=NA,
		data_institutions="CIAT"
	)

	## treatment level data 
	ff  <- carobiner::get_data(uri, path, group)

	## read the json for version, license, terms of use  
	js <- carobiner::get_metadata(cleanuri, path, major=1, minor=3, group)
	dset$license <- carobiner::get_license(js)

	## the AFSIS data 

	f <- ff[basename(ff) == "Kihara et al.2015_AFSIS_Data.xlsx"]
	d <- as.data.frame(readxl::read_excel(f))
	#"Site', 'Cluster', 'Season', 'Field', 'FieldID', 'TrtDesc', 'TCrop', 'FLat', 'FLong', 'Plot', 'PlotID', 'Trt', 'Rep', 'HarvestArea', 'TGrainYld_Uncorr"
	
	# not clear what is meant with replicate. See  table(d$Rep) 
	
	d <- d[, c('Site', 'Cluster', 'FieldID', 'TrtDesc', 'Rep', 'TCrop', 'FLat', 'FLong', 'TGrainYld_Uncorr')]
	colnames(d) <- c('location', 'site', 'field', 'treatment', 'rep', 'crop', 'latitude', 'longitude', 'yield')
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

	d$planting_date <- substr(d$field, 5, 8)	
	d$planting_date[d$planting_date %in% c('10LR', '10SR')] <- "2010"
	# months can be estimated from LR and SR 

	d$yield <- round(d$yield*1000)
		
	d$K_fertilizer <- d$P_fertilizer <- d$N_fertilizer <- 0
	d$N_fertilizer[grep("N", d$treatment)] <- 100
	d$K_fertilizer[grep("K", d$treatment)] <- 60 
	d$P_fertilizer[grep("P", d$treatment)] <- 30
	#d$fertilizer <- ""

	d$country <- NA
	d$country[d$location=="Nkhata Bay" | d$location=="Thuchila" | d$location=="Kasungu"] <- "Malawi"
	d$country[d$location=="Kiberashi"| d$location=="Mbinga"] <- "Tanzania"
	d$country[d$location=="Finkolo"] <- "Mali"
	d$country[d$location=="Pampaida"] <- "Nigeria"
	d$country[d$location=="Sidindi"] <- "Kenya"

	i <- d$country == "Malawi" & d$location == "Thuchila"
	d$longitude[i] <- 35.355
	d$latitude[i] <- -15.904
	i <- d$country == "Kenya" & d$location == "Sidindi"
	d$longitude[i] <- 34.389
	d$latitude[i] <- 0.154


	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$field <- NULL
	d$site <- as.character(d$site)

	# not clear what these mean
	d$rep <- NULL
	
	d$yield_part <- "grain"
	carobiner::write_files(dset, d, path=path, id="afsis")

###############################
	## FAO data 

	f <- ff[basename(ff) == "Kihara et al.2015_All_Sites.xlsx"]
	crds <- as.data.frame(readxl::read_excel(f))
	crds <- crds[crds$Dataset=="FAO", c("TrialID", "Lat", "Long", "SoilType")]
	
	f <- ff[basename(ff) == "Kihara et al.2015_FAO_Data.xlsx"]
	z <- as.data.frame(readxl::read_excel(f))
	z <- merge(z, crds, by="TrialID")
	
	names(z) <- tolower(names(z))
	z$soiltype  <- carobiner::fix_name(z$soiltype, "title")
	z$zone <- carobiner::fix_name(z$zone, "title")
	z$location <- carobiner::fix_name(z$location, "title")
	z$year[z$year=="87B"] <- "1987"
	z$year[z$year=="88A"] <- "1988"

	z$planting_date <- substr(z$year, 1, 4)

	z$harvest_date <- paste0("19", substr(z$year, 6, 8))
	z$harvest_date[z$harvest_date==19] <- NA
	
# 'trialid', 'country', 'zone', 'site', 'year', 'season', 'trial', 'nid', 'ycontrol_abs', 'yield', 'pcontrol', 'n control', 'n', 'p2o5', 'p', 'nae', 'pae', 'k2o', 'design', 'fym', 'classes', 'lat', 'long', 'soiltype', 'planting_date', 'harvest_date'

	z$k <- z$k2o * 0.8301
	sel <- c('country', 'zone', 'site', 'year', 'season', 'nid', 'ycontrol_abs', 'yield', 'pcontrol', 'n control', 'n', 'p', 'k', 'fym', 'lat', 'long', 'soiltype', 'planting_date', 'harvest_date')
	z <- z[, sel]
	z <- carobiner::change_names(z, c("nid", "site"), c("trial_id", "location"))
	
	fcontrol <- c('country', 'zone', 'location', 'year', 'season', "trial_id", 'ycontrol_abs', 'fym', 'lat', 'long', 'soiltype', 'planting_date', 'harvest_date')
	ctr <- unique(z[, fcontrol])
	ctr <- carobiner::change_names(ctr, "ycontrol_abs", "yield")
	ctr$n <- 0
	ctr$p <- 0
	ctr$k <- 0

	pcontrol <- c('country', 'zone', 'location', 'year', 'season', "trial_id", 'pcontrol', 'n', 'k', 'fym', 'lat', 'long', 'soiltype', 'planting_date', 'harvest_date')
	pctr <- unique(z[, pcontrol])
	pctr <- carobiner::change_names(pctr, "pcontrol", "yield")
	pctr$p <- 0

	ncontrol <- c('country', 'zone', 'location', 'year', 'season', "trial_id", 'n control', 'p', 'k', 'fym', 'lat', 'long', 'soiltype', 'planting_date', 'harvest_date')
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
	zz$year <- NULL

	zz <- carobiner::change_names(zz, 
	c("zone", "n", "p", "k", "fym", "lat", "long", "soiltype"), 
	c("adm1", "N_fertilizer", "P_fertilizer", "K_fertilizer", "OM_used", "latitude", "longitude", "soil_type"))
	zz$yield_part <- "grain"

	dataset_id <- paste0(cleanuri, "-fao")

	dset$dataset_id <- dataset_id
	zz$dataset_id <- dataset_id
	zz$on_farm <- NA
	zz$is_survey <- FALSE
	zz$crop <- "maize"
	zz$country[zz$country == "Guinea Biassu"] <- "Guinea-Bissau"
	zz$country[zz$country == "DR Congo"] <- "Democratic Republic of the Congo"

	zz$longitude <- as.numeric(gsub(",", ".", zz$longitude))

	i <- zz$country=="Guinea-Bissau" & zz$longitude > 0
	zz$longitude[i] <- -zz$longitude[i]

	i <- zz$country=="Guinea-Bissau" & zz$adm1=="Cacheu" & zz$location=="PASSANGUE" 
	zz$longitude[i] <- -16.1670
	zz$latitude[i] <- 12.2596 

	i <- zz$country=="Botswana" & zz$adm1=="Southern Region" & zz$location=="DITLHARAPA" 
	zz$longitude[i] <- 25.287
	zz$latitude[i] <- -25.750

	i <- zz$country=="Botswana" & zz$adm1=="Southern Region" & zz$location=="PELOTSHETLA" 
	zz$latitude[i] <- -25.6

	zz$location <- carobiner::fix_name(zz$location, "title")

	# most likely here
	zz$K_fertilizer[is.na(zz$K_fertilizer)] <- 0
 
	carobiner::write_files(dset, zz, path=path, id="fao")
}


