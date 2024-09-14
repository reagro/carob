# R script for "carob"

## NOTE more variables can be extracted but some are sparse
#round(100 * colSums(!is.na(r)) / nrow(r)) |> sort(decr=T)
#       id   variety       gid    source  seedkgha       rep    plotno     seeds     stand       dff    mature 
#      100       100       100       100        98        97        95        94        68        68        64 
#       bb    aphids    striga      smut    fodder   fodkgha     smut2     virus  plantinf     smut1      pods 
#       53        37        36        32        15        14        11        11        10         9         8 
# seed_pod   pod_pla   aphidss x_100sdwt   plantif     thrip   alectra  plantif2    thripp     yield    rating 
#        8         8         6         6         6         4         4         4         3         3         3 
#    dules  nodulewt  seedspod       ots  d_mature  standinf      dff1       trt        ot       and        ds 
#        3         3         3         3         3         2         2         2         1         1         1 
#  podkgha  drfdkgha    swt100  anthripc  plinfstr    blight  pltinstr    strplt   t_seeds     dummy    sttand 
#        1         1         1         1         1         1         1         1         1         1         1 


carob_script <- function(path) {
     
"The Cowpea Breeding Unit develops improved cowpea lines with high grain yield potential and resistance/tolerance to biotic and abiotic stresses and possessing farmers and consumers preferences. Data generated from multi-year evaluation trials carried out on different sets of materials developed within several projects conducted by the cowpea breeding unit are contained in the database."

	uri <- "doi:10.25502/20180817/1807/BO"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=2), 
		data_institute = "IITA", 
		publication= NA, 
		project="cowpea evaluation trials", 
		data_type= "experiment", 
		response_vars= "yield", 
		treatment_vars = "variety",
		carob_contributor= "Robert Hijmans", 
		carob_date="2024-07-24"
	)

	fcsv <- grep("\\.csv$", ff, value=TRUE)
	r <- lapply(fcsv, \(f) {
			x <- read.csv(f)
			cn <- tolower(names(x))
			cn <- gsub("aphhids|aphid|aphidss|aphidss", "aphids", cn)
			cn <- gsub("plot_no", "plotno", cn)
			cn <- gsub("sd_pod|num_s_pod", "seed_pod" , cn)
			cn <- gsub("podplant|pd_plt|pod_pla", "pod_plnt", cn)
			cn <- gsub("seedkha", "seedkgha", cn) # yield is in kg per plot
			cn <- gsub("viruse", "virus", cn)  
			cn <- gsub("dffl", "dff1", cn)  
			cn <- gsub("^x__..", "", cn)  
			cn <- gsub("x_50.dff", "dff", cn)  
			cn <- gsub("foderkgh", "fodkgha", cn) # fodder is in kg per plot
			cn <- gsub("thr.|thripp", "thrip", cn)  
			cn <- gsub("maturity", "mature", cn)  
			cn <- gsub("d_striga", "striga", cn)  
			names(x) <- cn
			x$source <- gsub(".csv$", "", basename(f))
			x
		}
	)
	
	r <- do.call(carobiner::bindr, r)
	r <- r[, colSums(is.na(r)) < nrow(r)]
	r[r == -32768] <- NA

	d <- data.frame(
		trial_id = r$source,
		country = "Nigeria",
		adm1 = "Kano", 
		longitude = 8.59196,
		latitude = 12.00218,
		crop = "cowpea",
		yield_part = "seed",
		#record_id = r$gid,
		rep = as.integer(gsub("REP|-", "", r$rep)),
		variety = r$variety,
		yield = r$seedkgha,
		fwy_storage = r$seedkgha,		
		fwy_residue = r$fodkgha,		
		flowering_days = r$dff,
		maturity_days = r$mature,
		plant_density = r$stand,
		seed_weight = as.numeric(r$x_100sdwt) * 10,
		seeds_pod = r$seeds,
		bacterial_blight_score = r$bb,
		smut_score = r$smut,
		striga_score = r$striga,
		aphid_score = r$aphids
	)

	d$on_farm <- d$is_survey <- FALSE
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d$irrigated <- NA
	d <- unique(d)

	carobiner::write_files(path, meta, d)
}


