# R script for "carob"

## ISSUES
###reference file missing: doi_10.7910_DVN_LTS278

carob_script <- function(path) {

"Description
Title: Replication Data for: Response of Maize to blended fertilizer  
Abstract: Maize grain and bio-mass yield were increased by application of different rates of blended fertilizers (2020-12-02)"

	## Process 
	uri <- "doi:10.7910/DVN/LTS278"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		uri=uri,
		data_citation="Atnafu, Obsa, 2020. Replication Data for: Response of Maize to blended fertilizer, https://doi.org/10.7910/DVN/LTS278",
		publication=NA,
		carob_contributor="Siyabusa Mkuhlani",
		carob_date="2021-08-26",
		data_type="experiment",
		data_institutions=NA,
		project=NA
	)

	## treatment level data 
	ff <- carobiner::get_data(uri, path, group)

	## read the json for version, license, terms of use  
	js <- carobiner::get_metadata(dataset_id, path, major=1, minor=0, group)
	dset$license <- carobiner::get_license(js)[[1]]

	f <- ff[basename(ff) == "AGP II 2017.18 RAW DATA.xlsx"] 
	d <- carobiner::read.excel(f)

	##Skip early rows(Descriptive rows)
	dd <- carobiner::read.excel(f)[,-c(1:11)]

	#transfer columns
	dd$country <- "Ethiopia"

	#Transfer locations & year
	dd$adm3 <- NA
	dd$adm3 <- replace(dd$adm3,1:11, "Limmu Sekka")
	dd$adm3 <- replace(dd$adm3,12:27, "Omo Nada")
	dd$year <- NA
	dd$year <- replace(dd$year,1:11,"2016/17")
	dd$year <- replace(dd$year,12:27,"2017/18")

	#Dissagregate the data frame
	de <- dd[c(1:10),c(1:4,9:11)] #limmu sekka 2017/18
	de$year[de$year=='2016/17'] <- '2017/18'
	de <- de[-1,]
	#names(de)
	colnames(de) <- c("Rep","Trt","GY kg/ha","BM kg/ha","country", "adm3", "year")

	df <- dd[c(14:22),c(1:4,9:11)] #Omo Nada 2017/18
	colnames(df) <- c("Rep","Trt","GY kg/ha","BM kg/ha","country","adm3","year")
	dg <- dd[c(2:10),c(5:11)] #Limmu Sekka 2016/17
	colnames(dg) <- c("Rep","Trt","GY kg/ha","BM kg/ha","country","adm3","year")

	dh <- dd[c(14:22),c(5:11)] #Omo Nada 2016/17
	dh$year[de$year=='2017/18'] <- '2016/17'
	colnames(dh) <- c("Rep","Trt","GY kg/ha","BM kg/ha","country","adm3","year")

	dv <- rbind(dh,dg,df,de)
	colnames(dv) <- c("rep","treatment","yield","biomass_total","country","adm3","planting_date")

	#####Change Treatment
	dv$N_fertilizer <- 0
	dv$P_fertilizer <- 0
	dv$K_fertilizer <- 0
	dv$Zn_fertilizer <- 0
	dv$S_fertilizer <- 0

	i <- dv$treatment=='A'
	dv$treatment[i] <- 'Ctrl/FP'
	dv$N_fertilizer[i] <- 0
	dv$P_fertilizer[i] <- 0
	dv$fertilizer_type <- "unknown" # Unknown fertilizer

	i <- dv$treatment=='B'
	dv$treatment[i] <- 'Cal. P & rec. N'
	dv$N_fertilizer[i] <- 92
	dv$P_fertilizer[i] <- 0
	dv$fertilizer_type <- "unknown" # Unknown fertilizer

	i <- dv$treatment=='C'
	dv$treatment[i] <- '92 kg/ha N & 30 kg/ha P'
	dv$N_fertilizer[i] <- 92
	dv$P_fertilizer[i] <- 30
	dv$fertilizer_type <- "unknown" # Unknown fertilizer

	## RH: N_fertilizer, P_fertilizer and K_fertilizer need to be 
	## RH: updated based on this (if this includes any fertilizer) 
	## RH: see what I have done above for treatment "C"
	## RH: also, avoid non-standard abbreviations

	# message("    NPK treatments incomplete. SM please fix\n")
	# dv$treatment[dv$treatment=='A'] <- 'Ctrl/FP'
	# dv$treatment[dv$treatment=='B'] <- 'Cal. P & rec. N'


	##Correct date
	i <- dv$planting_date=='2016/17'
	dv$planting_date[i] <- '2016'
	dv$harvest_date[i] <- '2017'
	i <- dv$planting_date=='2017/18'
	dv$planting_date[i] <- '2017'
	dv$harvest_date[i] <- '2018'

	#Format data set
	dv$dataset_id <- "doi_10.7910_DVN_LTS278"
	dv$crop <- 'maize'
	dv$on_farm <- TRUE
	dv$is_survey <- FALSE
	dv$trial_id <- 'Blendedfert'

	dv$yield <- as.numeric(dv$yield)
	dv$dmy_total <- as.numeric(dv$dmy_total)

	dv$rep <- as.integer(dv$rep)

	dv$longitude <- 0
	dv$latitude <- 0

	dv$longitude[dv$adm3=='Limmu Sekka'] <- 36.945489
	dv$latitude[dv$adm3=='Limmu Sekka'] <- 8.191739
	dv$longitude[dv$adm3=='Omo Nada'] <- 37.25
	dv$latitude[dv$adm3=='Omo Nada'] <- 7.6333333
	dv$yield_part <- "grain"

	carobiner::write_files(dset, dv, path=path)
}

