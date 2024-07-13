
do_LCAS <- function(r) {

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
		soil_texture = r$D.q401_soilTexture,
		soil_quality = r$D.q403_soilPerception,
		landscape_position = r$D.q402_drainClass,
		previous_crop_residue_perc = r$D.q407_cropResiduePcnt,
		previous_crop_burnt = r$D.q408_residueBurnt == "yes",
		land_prep_method = r$D.q411_LandPrep,
		is_survey = TRUE
	)
	
	d$trial_id <- as.character(1:nrow(d))
	d$country <- r$A.q101_country
	if (is.null(d$country)) d$country <- "India"
	d$country[d$country == "8"] <- "India"
#	d$country <- "India"
	d$yield_part <- "grain"

	d$planting_method = r$D.q413_CropEst
	## grep above to get d$trans_planting_method 

	plot_ha <- 0.404686 * r$C.q305_cropLargestArea / r$C.q302_acreConv

## ?? 
	d$flood_stress <- tolower(r$I.q5504_floodSeverity)
	d$drought_stress <- tolower(r$I.q5502_droughtSeverity)
	d$pest_severity <- tolower(r$I.q5506_insectSeverity)
	d$weed_severity <- tolower(r$I.q5505_weedSeverity)
	d$disease_severity <- tolower(r$I.q5509_diseaseSeverity)

	d$insecticide_product <- tolower(r$I.q5508_insecticidesName)
	d$insecticide_product[grep("remember", d$insecticide_product)] <- "unknown"
	d$insecticide_product[d$insecticide_product %in% c("chloripyriphos", "chloropyariphosh", "chloropyriphos", "chlorpyriphos")] <- "chlorpyrifos"
	d$insecticide_product[d$insecticide_product %in% c("firadon", "furadan", "furadon")] <- "carbofuran"
	d$insecticide_product <- carobiner::replace_values(d$insecticide_product, 
		c("dichlorophos", "imadiclorpid", "imidachloropid"),
		c("dichlorvos", "imidacloprid", "imidacloprid"), FALSE)

	d$biocide_product <- tolower(r$I.q5511_pesticidesName)
	d$biocide_product[grep("remember", d$biocide_product)] <- "unknown"

	d$herbicide_product <- apply(r[, c("J.q5601_1herbName", "J.q5603_2herbName", "J.q5605_3herbName")], 1, 
		\(i) {
			i <- tolower(i)
			i <- gsub("2,4-d|24d", "2,4-D", i)
			i <- gsub("clodinofop", "clodinafop", i)
			i <- gsub("idosulfuron", "iodosulfuron", i)
			i <- gsub("leader|traget", "sulfosulfuron", i)
			i <- gsub("propargyl", "clodinafop", i)
			i <- gsub("\\+", "; ", unique(i))
			i <- gsub(", ", "; ", unique(i))
			i <- gsub("; NA|NA", "", paste(unique(i), collapse="; "))
			gsub("", NA, i)
		})

	d$herbicide_times <- as.integer(rowSums(!is.na(r[, c("J.q5601_1herbName", "J.q5603_2herbName", "J.q5605_3herbName")]))) 
	d$herbicide_timing <- apply(r[, c("J.q5602_1herbAppDays", "J.q5604_2herbAppDays", "J.q5606_3herbAppDays"
)], 1, \(i) paste(na.omit(i), collapse=";"))

	d$herbicide_product[d$herbicide_product == ""] <- NA 
	d$herbicide_timing[d$herbicide_timing == ""] <- NA 
	
	d$weeding_times <- as.integer(r$J.manualWeedTimes)

	d$planting_date <- r$D.seedingSowingTransplanting
	if (is.null(d$planting_date)) d$planting_date <- r$D.q415_seedingSowingTransDate


	d$seed_amount = r$D.q420_cropSeedAmt / plot_ha
	d$seed_source = ifelse(r$D.q421_seedSource == "other", 
							r$D.q422_otherSeedSource, r$D.q421_seedSource)

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

	d$previous_crop <- carobiner::replace_values(d$previous_crop, 
		c("fallow", "other", "bajra", "jowar", "greenmanure", "greengram", "pulses", "mungbean"), 
		c("none", NA, "pearl millet", "sorghum", "green manure", "mung bean", "pulse", "mung bean"), 
		FALSE)


	get_fert <- function(x, product) {
		p <- paste0(c("_basal", "_1td", "_2td", "_3td"), product, "$")
		cn <- colnames(x)
		i <- sapply(p, \(v) grep(v, cn))
		stopifnot(length(i) == 4)
		rowSums(x[, i], na.rm=TRUE)
	}

	fert <- data.frame(
		DAP = get_fert(r, "DAP"), 
		NPK = get_fert(r, "NPK"), 
		urea = get_fert(r, "Urea"), 
		NPKS = get_fert(r, "NPKS"), 
		KCl = get_fert(r, "MoP"),
		SSP = get_fert(r, "SSP"), 
		TSP = get_fert(r, "TSP"), 
		ZnSO4 = get_fert(r, "ZnSO4"), 
		gypsum = get_fert(r, "Gypsum"), 
		H3BO3 = get_fert(r, "Boron")
	) / plot_ha
	
    # to get the fertilizer/ha
	ftab <- carobiner::accepted_values("fertilizer_type")
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

	d$OM_used <- r$E.q5101_FYM == "yes"
	d$OM_type <- paste0("farmyard manure (", tolower(r$E.q5102_typeFYM), ")")
	d$OM_type[!d$OM_used] <- NA
	d$OM_amount <- r$E.q5103_amtFYM / plot_ha


##	p <- r[, c("F.q5112_priceDAP", "F.q5113_priceNPK", "F.q5114_priceUrea", "F.q5127_priceMoP", "F.q5115_priceZnSO4", "F.q5116_priceGypsum", "F.q5117_priceBoron", "F.q5126_priceNPKS", "F.q5128_priceSSP", "F.q5129_priceTSP")]
##	nms <- c("DAP", "NPK", "urea", "KCl", "S", "gypsum", "B", "NPKS", "SSP", "TSP")

	crop_cut_biomass <- data.frame(
		bm1 = r$B.q201_q1tagb, 
		bm2 = r$B.q204_q2tagb, 
		bm3 = r$B.q207_q3tagb
	)
	# biomass from 2*2 quadrants
	if (nrow(crop_cut_biomass) > 0) {
		d$dmy_total <- 10000 * rowMeans(crop_cut_biomass) / 4
	}

	crop_cut_yield <- data.frame(
		gw1 = r$B.q202_q1gWeight,
		gw2 = r$B.q205_q2gWeight,
		gw3 = r$B.q208_q3gWeight
	)

	if (nrow(crop_cut_yield) > 0) {
		crop_cut_yield[crop_cut_yield==0] <- NA
		crop_cut_yield <- 10000 * rowMeans(crop_cut_yield, na.rm=TRUE) / 4
	}
	moist <- data.frame(
		m1 = r$B.q203_q1gMoist,
		m2 = r$B.q206_q2gMoist,
		m3 = r$B.q209_q3gMoist
	)
	if (nrow(moist) > 0) {
		moist[moist==0] <- NA
		crop_cut_moist <- rowMeans(moist, na.rm=TRUE)
		crop_cut_moist[is.na(crop_cut_moist)] <- 14
		crop_cut_yield <- crop_cut_yield * (100 - crop_cut_moist) / 86
	}
	
	d$yield <- 10 * r$L.q606_largestPlotYieldQUNITAL / plot_ha
	
	if (!is.null(d$crop_cut)) {
		d$crop_cut <- !is.na(crop_cut_yield)
		d$yield[d$crop_cut] <- crop_cut_yield[d$crop_cut] 
	}
	
	d$crop_price <- r$M.q706_cropSP
	d
}

