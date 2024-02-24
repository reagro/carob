# R script for "carob"

## ISSUES
# no location data available. I have asked the author.


carob_script <- function(path) {

"Description:
Under CSISA Phase II, Nutrient Omission Plot Technique (NOPT) trials were conducted in eight districts of Bihar, ten districts of Uttar Pradesh and ten districts of Odisha. Partner institutions include Bihar Agriculture University (BAU), Banaras Hindu University (BHU), Orissa University of Agriculture and Technology (OUAT), Central Rice Research Institute (CRRI), Odisha.
"

	uri <- "hdl:11529/10911"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication=NA,
	   data_citation = "Sheetal Sharma, 2017. Nutrient Omission Plot Technique Trials from Bihar, Uttar Pradesh and Odisha. https://hdl.handle.net/11529/10911, CIMMYT Research Data & Software Repository Network, V2",
	   data_institutions = "CIMMYT",
	   carob_contributor="Robert Hijmans",
	   carob_date="2024-01-27",
	   data_type= "experiment",
	   project=NA
 	)

## download and read data 

	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)
	dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)

	f1 <- ff[basename(ff) == "CSISA_NOPT_Rice_RawDataFINAL.csv"]
	f2 <- ff[basename(ff) == "CSISA_NOPT_Wheat_RawDataFINAL.csv"]

	r1 <- read.csv(f1, check.names=FALSE)
	r2 <- read.csv(f2, check.names=FALSE)
	r <- carobiner::bindr(r1, r2)
	
	d <- data.frame(
		country = "India",
		dataset_id = dataset_id,
		crop=tolower(r$Crop), previous_crop = tolower(r$PCRP),
		yield_part = "grain",
		season=tolower(r$Season), trial_id = r$SiteID, 
		soil_texture = r$STYP, soil_color =  r$SCLR, 
		variety = r$VAR, variety_type = r$VTYP, 
		planting_date = r$SDATE,
		transplanting_date = r$TDATE,
		harvest_date = r$HDATE,
		season_constraint = trimws(tolower(r$CSTR)),
		soil_constraint = trimws(tolower(r$SCON)),
		irrigation_source = trimws(tolower(r$IRRS))
	)
	d[d==""] <- NA
	adm1 <- substr(r$SiteID, 1, 2)
	adm1 <- gsub("UP", "Uttar Pradesh", adm1)
	adm1 <- gsub("BH", "Bihar", adm1)
	d$adm1 <- gsub("OD", "Odisha", adm1)

	d$latitude <- d$longitude <- as.numeric(NA)

	irn <- gsub("thrice", "3", tolower(r$IRRN))
	irn <- gsub("once", "1", irn)
	irn <- gsub("20-22", "21", irn)
	irn <- gsub("20-23", "21", irn)
	irn <- gsub("20-24", "22", irn)
	irn <- gsub("20-25", "22", irn)
	d$irrigation_number = suppressWarnings(as.integer(irn))
	d$land_prep_method[d$crop=="wheat"] <- r$ESTM[d$crop=="wheat"]
	d$transplanting_method[d$crop=="rice"] <- r$ESTM[d$crop=="rice"]
	d$transplanting_method[	d$transplanting_method == "" ] <- NA

	for (v in c("planting_date", "transplanting_date", "harvest_date")) {
		d[[v]] <- as.character(as.Date(d[[v]], "%m/%d/%y"))
	}

	getM <- function(v) {
		x <- r[, c(colnames(r)[grep(v, colnames(r))])]
		colnames(x) <- gsub(v, "", colnames(x))
		data.frame(id=1:nrow(x), x)
	}
		
	colnames(r)[colnames(r) == "YLDK"] <- "YLD+K"
	colnames(r)[colnames(r) == "YLDZN"] <- "YLD+ZN"
	n <- getM("-N$")
	p <- getM("-P$")
	k <- getM("-K$")
	kk <- getM("\\+K$")
	z <- getM("-ZN$")
	zz <- getM("\\+ZN$")
	b <- getM("B$")
	colnames(b) <- gsub("\\.$", "", colnames(b))
	colnames(b)[6] <- "B"
	nut <- rbind(n, p, k, z, kk, zz)
	nut <- carobiner::bindr(nut, b)
	nut <- carobiner::change_names(nut, 
			c("N", "P2O5", "K2O", "ZNSO4", "YLD", "B"),
			c(paste0(c("N", "P", "K", "Zn"), "_fertilizer"), "yield", "B_fertilizer"))
	nut$P_fertilizer <- nut$P_fertilizer / 2.29
	nut$K_fertilizer <- nut$K_fertilizer / 1.2051
	nut$Zn_fertilizer <- nut$Zn_fertilizer * 0.15
	# nut$B_fertilizer <- nut$B_fertilizer / ??
	nut$yield <- nut$yield * 1000
	nut$B_fertilizer[is.na(	nut$B_fertilizer )] <- 0 

	d <- cbind(d[nut$id, ], nut[,-1])
	d <- d[!is.na(d$yield), ]

	carobiner::write_files(dset, d, path=path)
}

