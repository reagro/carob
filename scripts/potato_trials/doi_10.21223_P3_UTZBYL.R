
carob_script <- function(path) {

"
	Description:
	The genotype x environment interaction (GXE) does see the importance of environmental effect on the adaptation and varietal performance. Therefore, breeders must have a methodology for quantifying and interpreting the GXE interaction and thus help clarify areas where a genotype can be useful.
	New statistical methods allow the identification and recommendation of new clones with specific or broad adaptation. The combination of Geographic Information System (GIS) with an analysis of variance of AMMI models (Additive Main Effects and Multiplicative Interactions), SREG (Sites Regression Model) and PLS (Partial Least Squares Regression) offer a new possibility to predict potential areas for production of these materials. Regional yield trials are networks of experiments by which a set of cultivars is usually assessed to make genotype recommendation.
	
"
	uri <- "doi:10.21223/P3/UTZBYL"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "potato_trials"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication= NA,# 
	   data_citation ="Salas, Elisa; Juarez, Henry; Giraldo, Diana; Amoros, Walter; Simon, Reinhard; Bonierbale, Merideth, 2017, Dataset for: Models of analysis stability and definition of environments with GIS, 
	   https://doi.org/10.21223/P3/UTZBYL, International Potato Center, V1",
	   data_institutions = "IITA",
	   carob_contributor="Cedric Ngakou",
	   data_type="experiment",
	   project=NA,
	   carob_date="2023-10-30"
	)

	## download and read data 
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=2)
	dset$license <- carobiner::get_license(js)

	ff <- ff[grep("^PTYL200", basename(ff))]
	bn <- basename(ff)

	# read and process files
	proc_fun <- function(f) {
		r <- carobiner::read.excel(f,sheet="Fieldbook")
		# this is marketable yield, total tuber yield not reported
		r <- r[, c("REP", "INSTN", "MTYNA")]
		colnames(r) <- c("rep", "variety", "yield")
		m <- carobiner::read.excel(f, sheet="Minimal") 
		n <- as.list(m$Value)
		names(n) <- m$Factor
		r$adm1 <- n$Admin1
		r$adm2 <- n$Admin2
		r$adm3 <- n$Admin3
		r$planting_date <- n$`Begin date`
		r$harvest_date <- n$`End date`
		r$longitude <- as.numeric(n$Longitude)
		r$latitude <- as.numeric(n$Latitude)
		k <- carobiner::read.excel(f, sheet="Soil_analysis")
		k1<- as.list(k$Abbreviture)
		k<- t(k)
		colnames(k)<- k1
		m1 <- as.data.frame(k[!(row.names(k) %in% c("1","2")),])
		r$soil_pH<-  mean(as.double(m1$pH),na.rm=TRUE) 
		r$OM_applied<- mean(as.double(m1$MO),na.rm=TRUE) 
		r$soil_P_available<-  (mean(as.double(m1$P),na.rm=TRUE)) 
		r$soil_K<-  mean(as.double(m1$K),na.rm=TRUE) 
		r$soil_sand<-  mean(as.double(m1$Sand),na.rm=TRUE)
		r$soil_clay<-  mean(as.double(m1$Clay),na.rm=TRUE)
		r$soil_silt<-  mean(as.double(m1$Silt),na.rm=TRUE)
		
		r
		   
	}

	d <- lapply(ff, proc_fun) 
	d <- do.call(rbind, d)

	d$rep <- as.integer(d$rep)
	d$yield <- d$yield * 1000 ## kg/ha

	## add columns
	d$dataset_id <- dataset_id
	d$country <- "Peru"
	d$trial_id <- paste(d$adm3, d$planting_date, sep = "_")
	d$irrigated <- FALSE
	d$inoculated <- FALSE
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$crop <- "potato"
	d$yield_part <- "tubers" 

	# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

#m <- data.frame(
#	f = c('PTYL200205_CIPHQ', 'PTYL200208_CIPSRM-1', 'PTYL200211_CHIARA', 'PTYL200211_LAVICT', 'PTYL200308_CIPSRM-1', 'PTYL200309_CIPHQ'),
#	adm1 = c("Lima", "Junín", "Ayacucho", "Junín", "Junín", "Lima"),
#	adm2 = c("Lima", "Chanchamayo", "Huamanga", "Huancayo", "Chanchamayo", "Lima"),
#	adm3 = c("La Molina", "San Ramón", "Chiara", "San Agustin", "San Ramón", "La Molina"),	   
#   harvest_date=c("2002-05-01","2002-08-01","2002-11-01","2002-11-01","2003-08-01","2003-09-01"),
#    planting_date=c("2002-08-01","2002-10-01","2003-02-10","2003-01-01","2003-10-01","2003-12-10"))

### add lon and lat coordinate
#geo <- data.frame(adm3=c("La Molina","San Ramón","Chiara","San Agustin"),
#                 longitude=c(-76.948417,-75.356389,-74.206,-75.2449),
#                 latitude=c(-12.076289,-11.1275,-13.2734,-12.0264))

#mg <- merge(m, geo, by="adm3",all.x=TRUE)
