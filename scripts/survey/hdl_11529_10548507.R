## ISSUES


carob_script <- function(path) { 
  
  "
  The objective of the Landscape Diagnostic Survey (LDS) for wheat is to bridge the existing data-gap around current production practices of wheat, and also to help in evidence-based planning. The LDS is designed in a way that data is collected from randomly selected farmers spread uniformly within a KVK (government extension system)  domain/district. Data has been collected from farmers largest wheat plot for winter season of 2018. Survey questionnaire captures all production practices applied by farmers from land preparation to harvesting, including detailed sections on fertilizer use, weed control and irrigation application. Data is captured through electronically enabled Open Data Kit
  (ODK) tool on mobile phone or tablet. (2019-12-31)
"

  uri <- "hdl:11529/10548507"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "survey"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project="CSISA",
    uri=uri,
    data_citation="Ajay, Anurag; Craufurd, Peter; Sharma, Sachin; Ranjan, Harshit; Poudel, Gokul; Malik, RK; Singh, Balwinder; Singh, AK; Samaddar, Arindam; Rai, Ashok; Keil, Alwin; McDonald, Andrew, 2020, Landscape diagnostic survey data of wheat production practices and yield of 2018 from eastern India, https://hdl.handle.net/11529/10548507, CIMMYT Research Data & Software Repository Network, V1, UNF:6:ACX3w1PnF4Otyf++Z6mO3g== [fileUNF]",
    publication= NA,
    data_institutions = "CIMMYT",
    data_type="survey", 
    carob_contributor="Robert Hijmans and Effie Ochieng'",
    carob_date="2024-01-22"
  )

  
  ## download and read data 
  
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	dset$license <- carobiner::get_license(js)
	dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)
	
	f <- ff[basename(ff) == "CSISA_IND_LDS_Whe_2018_Data.csv"]
	r <- read.csv(f)
	d <- data.frame(
		date = r$collectionDate,
		adm1 = r$A.q102_state,
		adm2 = r$A.q103_district,
		adm3 = r$A.q104_subDistrict,
		location = r$A.q105_village,
		crop = tolower(r$A.q116_crop),
		previous_crop = tolower(r$D.prevCrop),
		harvest_date = r$L.q601_harvestDate,  # L.harvDate is cleaner, but not in dictionary
		season = r$A.q117_season, # A.q118_harvYear,
		variety = r$D.q410_varName,
		variety_type = r$D.q409_varType,
		latitude = r$O.largestPlotGPS.Latitude,
		longitude = r$O.largestPlotGPS.Longitude,
		yield = r$L.tonPerHectare * 1000,
		insectide_product = r$I.q5508_insecticidesName,
		pesticide_product = r$I.q5511_pesticidesName
	)

#	country <- r$A.q101_country
	country <- "India"
	
	d$planting_date <- r$D.seedingSowingTransplanting
	if (is.null(d$planting_date)) d$planting_date <- r$D.q415_seedingSowingTransDate

	d$seed_source = ifelse(r$D.q421_seedSource == "other", 
							r$D.q422_otherSeedSource, r$D.q421_seedSource)

	d$OM_used <- r$E.q5101_FYM == "yes"
	d$OM_type <- paste0("farmyard manure (", r$E.q5102_typeFYM, ")")
	d$OM_type[!d$OM_used] <- NA

	d$herbicide <- apply(r[, c("J.q5601_1herbName", "J.q5603_2herbName", "J.q5605_3herbName")], 1, 
		\(i) {
			i <- gsub("\\+", "; ", unique(i))
			i <- gsub(", ", "; ", unique(i))
			i <- gsub("; NA|NA", "", paste(unique(i), collapse="; "))
		})
		
	d$herbicide[d$herbicide == "2,4-D, 24D"] <- "2,4-D"
	d$herbicide_times <- as.integer(rowSums(!is.na(r[, c("J.q5601_1herbName", "J.q5603_2herbName", "J.q5605_3herbName")]))) 
	d$weeding_times <- as.integer(r$J.manualWeedTimes)

	crop_cut_biomass <- data.frame(
		bm1 = r$B.q201_q1tagb, 
		bm2 = r$B.q204_q2tagb, 
		bm3 = r$B.q207_q3tagb
	)
	# biomass from 2*2 quadrants
	d$dmy_total <- 10000 * rowMeans(crop_cut_biomass) / 4

	crop_cut_yield <- data.frame(
		gw1 = r$B.q202_q1gWeight,
		gw2 = r$B.q205_q2gWeight,
		gw3 = r$B.q208_q3gWeight
	)
	crop_cut_yield[crop_cut_yield==0] <- NA
	crop_cut_yield <- 10000 * rowMeans(crop_cut_yield, na.rm=TRUE) / 4
	
	moist <- data.frame(
		m1 = r$B.q203_q1gMoist,
		m2 = r$B.q206_q2gMoist,
		m3 = r$B.q209_q3gMoist
	)
	moist[moist==0] <- NA
	crop_cut_moist <- rowMeans(moist, na.rm=TRUE)
	crop_cut_moist[is.na(crop_cut_moist)] <- 14
	crop_cut_yield <- crop_cut_yield * (100 - crop_cut_moist) / 86
	
					# note the spelling error
	plot_acres = r$C.q306_cropLarestAreaAcre 
# note that these "tot" variables are not in dictionary	
	fert <- data.frame(
		DAP = r$F.totAmtDAP, 
		NPK = r$F.totAmtNPK, 
		urea = r$F.totAmtUrea, 
		NPKS = r$F.otherGradeNPKS, 
		KCl = r$F.totAmtMoP, 
		SSP = r$F.totAmtSSP, 
		TSP = r$F.totAmtTSP, 
		ZnSO4 = r$F.totAmtZnSO4, 
		gypsum = r$F.totAmtGypsum, 
		H3BO3 = r$F.totAmtBoron  # ?
	) / (plot_acres * 0.404686)
	
	

	fix_date <- function(x) {
		x <- gsub(", ", "-", x)
		x <- gsub(" ", "-", x)
		x <- gsub("/", "-", x)
		for (y in 16:24) {
			x <- gsub(paste0("-", y, "$"), paste0("-20", y), x)
		}
		
		month.num <- paste0("-", formatC(1:12, width=2, flag = "0"), "-")
		for (i in 1:12) {
			x <- gsub(paste0("-", month.abb[i], "-"), month.num[i], x)
		}

		dat <- rep(as.Date(NA), length(x))
		i <- grepl("-", x)
		dat[!i] <- as.Date("1899-12-31") + as.numeric(x[!i])
		dat[i] <- as.Date(x[i], "%d-%m-%Y")
		as.character(dat)
	}
	
	
	d$date <- fix_date(d$date)
	d$planting_date <- fix_date(d$planting_date)
	d$harvest_date <- fix_date(d$harvest_date)
	d$is_survey <- TRUE
	d$dataset_id <- dataset_id
	d$previous_crop <- carobiner::replace_values(d$previous_crop, 
		c("fallow", "other", "bajra", "jowar", "greenmanure", "greengram", "pulses", "mungbean"), 
		c("no crop", NA, "pearl millet", "sorghum", "green manure", "mung bean", "pulse", "mung bean"))


	d$country[d$country == "8"] <- "India"
	d$yield_part <- "grain"
 
 
    # to get the fertilizer/ha
	ftab <- carobiner::get_accepted_values("fertilizer_type", path)
	ftab <- ftab[match(colnames(fert), ftab$name), c("name", "N", "P", "K", "S", "B", "Mg", "Ca", "Zn")]
## define NPK according to R script that comes with the data
	ftab[ftab$name=="NPK", c("N", "P", "K", "S")] <- c(12, 20, 13, 0)	
### none applied anyway
	ftab[ftab$name=="NPKS", c("N", "P", "K", "S")] <- c(12, 20, 13, 0)	

	fert[is.na(fert)] <- 0

  
 	# NPK percentages from the R script that was published with the data
	d$N_fertilizer <- colSums(t(fert) * ftab$N / 100)
	d$P_fertilizer <- colSums(t(fert) * ftab$P / 100)
	d$K_fertilizer <- colSums(t(fert) * ftab$K / 100)
	d$S_fertilizer <- colSums(t(fert) * ftab$S / 100)
	d$B_fertilizer <- colSums(t(fert) * ftab$B / 100)
	d$Zn_fertilizer <- colSums(t(fert) * ftab$Zn / 100)
	
    carobiner::write_files(path, dset, d)

}


#a = r[, c("L.q605_totalGrainYieldQUINTAL", "L.q606_largestPlotYieldQUNITAL", "L.quintalPerAcre", "L.tonPerHectare", "C.q306_cropLarestAreaAcre")]

