
doi_10.7910_DVN_UNLRGC <- function(path) {

	description <- "
		Kihara, J., Huising, J., Nziguheba, G. et al. Maize response to macronutrients and potential for profitability in sub-Saharan Africa. Nutr Cycl Agroecosyst 105, 171–181 (2016).

		Abstract: The objective of this study was to determine the attainable maize grain response to and potential of profitability of N, P and K application in SSA using boundary line approaches. Data from experiments conducted in SSA under AfSIS project (2009–2012) and from FAO trials database (1969–1996) in 15 countries and constituting over 375 different experimental locations and 6600 data points are used. 

		There are two datasets, AFSIS and FAO. 

		AFSIS has more detail than the public AFSIS data so we used these. 
		AFSIS has data for five countries, each with one or two sites. Sites have subsites, referred to as 'cluster' . 
	"


	notes <- " 
		citation could be improved to include the underlying data sources.
	"



	## Process 


	uri <- "doi:10.7910/DVN/UNLRGC"

	## dataset level data 
	dset <- data.frame(
	   uri=uri,
	   publication="doi:10.1007/s10705-015-9717-2",
	   citation="10.1007_s10705-015-9717-2-citation.ris",
	   experiment_type="fertilizer",
	   has_weather=FALSE,
	   has_management=FALSE
	)

	## treament level data 
	cleanuri <- Rcarob::clean_uri(uri)
	rawpath <- file.path(path, "data/raw")
	ff <- agro::get_data_from_uri(uri, path=rawpath)

	## read the json for license, terms of use and...  
	js <- Rcarob::get_json(cleanuri, rawpath, major=1, minor=3)
	dset <- Rcarob::get_terms(js, dset)

	jf <- file.path(rawpath, cleanuri, paste0(cleanuri, ".json"))
	x <- jsonlite::fromJSON(readLines(jf))
	if (x$data$latestVersion$versionNumber != 1) stop(paste("new major version of", cleanuri))
	if (x$data$latestVersion$versionMinorNumber != 3) warning(paste("new minor version of", cleanuri))

	## first the AFSIS data 
	f <- ff[basename(ff) == "Kihara et al.2015_AFSIS_Data.xlsx"]
	d <- as.data.frame(readxl::read_excel(f))
	#"Site', 'Cluster', 'Season', 'Field', 'FieldID', 'TrtDesc', 'TCrop', 'FLat', 'FLong', 'Plot', 'PlotID', 'Trt', 'Rep', 'HarvestArea', 'TGrainYld_Uncorr"
	d <- d[, c('Site', 'Cluster', 'FieldID', 'TrtDesc', 'TCrop', 'FLat', 'FLong', 'TGrainYld_Uncorr')]
	colnames(d) <- c('site', 'subsite', 'field', 'treatment', 'crop', 'latitude', 'longitude', 'yield')
	d[d=="NA"] <- NA


	lat <- as.numeric(d$latitude)
	lon <- as.numeric(d$longitude)
	i <- which(abs(lat) > 90)
	lat[i] <- as.numeric(paste0(substr(d$latitude[i], 1, 2), ".", substr(d$latitude[i], 3, 6)))
	lon[i] <- -1 * as.numeric(paste0(substr(d$longitude[i], 1, 1), ".", substr(d$longitude[i], 2, 6)))
	d$latitude <- round(lat, 5)
	d$longitude <- round(lon, 5)
	d$crop <- tolower(d$crop)

	d$year <- substr(d$field, 5, 8)	
	i <- d$year=='10LR' | d$year=='10SR'
	#d$season <- ??? based on above 
	d$year[i] <- "2010"
	d$year <- as.numeric(d$year)

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

		
	d$id <- cleanuri
	d$on_farm <- TRUE,
	d$is_survey <- FALSE,

	#Rcarob::write_files(dset, d, path, cleanuri) #, 1)


		
	## FAO data 
	f <- ff[basename(ff) == "Kihara et al.2015_FAO_Data.xlsx"]
	z <- as.data.frame(readxl::read_excel(f))
	names(z) <- tolower(names(z)
	z



	d$start_year <- d$year
	d$end_year <- NA
	d$start_month <- NA
	d$end_month <- NA

		d$uri <- uri

}
