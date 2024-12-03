# R script for "carob"

carob_script <- function(path) {

"Grain yield data collected from Conservation Agriculture (CA) systems across experiments of varying experimental duration, established in trial locations of Malawi, Mozambique, Zambia, and Zimbabwe under an increasingly variable climate. Data contains different agro-environmental yield response moderators such as type of crop diversifcation and amount of rainfall and aims to identify cropping systems that may provide both short-term gains and longer-term sustainability."

	uri <- "hdl:11529/10548832"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=1),
		data_institute = "CIMMYT",
		publication = "doi:10.1016/j.agee.2021.107812",
		project = NA,
		data_type = "experiment",
		response_vars = "yield",
		treatment_vars = "yield", 
		carob_contributor = "Blessing Dzuda",
		carob_date = "2024-05-31"
	)
	
	f <- ff[basename(ff) == "DATA SA 2005 to 2019.xls"]
	r <- carobiner::read.excel(f, sheet = "Raw Data")

	d <- data.frame(
		trial_id=r$Location,
		planting_date=as.character(r$Season),
		harvest_date=as.character(r$Season+1),
		rep=as.integer(r$Rep),
		rain=r$Rainfall,
		N_fertilizer=r$Nitrogen,
		P_fertilizer=r$Phosphorus,
		K_fertilizer=r$Potassium,
		soil_clay=r$Clay,
		soil_SOC=r$OrgC,
		fwy_total=r$Biomass,
		yield=r$Grain*1000,
		yield_part="grain",
		crop="maize"
	)
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	
	
	# from the paper, Table 1
	geo <- data.frame(
		trial_id = c("HRS", "MFTC", "CRS", "SRS", "DTC", "MRS"),
		location = c("Henderson Research Station (HRS)", "Monze Farmer Training Centre (MFTC)", "Chitedze Research Station (CRS)", "Sussundenga Research Station (SRS)", "Domboshawa Training Centre (DTC)", "Msekera Research Station (MRS)"),
		country = c("Zimbabwe", "Zambia", "Malawi", "Mozambique", "Zimbabwe", "Zambia"),
		#experimental_period  = c("2004–2019", "2005–2019", "2007–2019", "2006-2014", "2009–2019", "2011–201?"),
		#duration_of_experiment_years = c("15", "14", "12", "9", "10", "8"),
		latitude = c(-17.5727, -16.2402, -13.9732, -19.3169, -17.6091, -13.6450),
		longitude = c(30.9874, 27.4414, 33.6540, 33.2416, 31.1337, 32.5585),
		elevation  = c(1136, 1108, 1145, 608, 1543, 1018),
		soil_texture = c("loamy sand;sand", "sandy loam", "sandy loam", "sandy loam", "sandy loam", "sandy loam"),
		soil_type  = c("Arenosols and Luvisols", "Chromic Lixisols", "Ferruginous Latosols", "Haplic Lixisols", "Rhodic Lixisols", "Luvisols")
	)

	d <- merge(d, geo, by="trial_id", all.x=TRUE)	
	d$geo_from_source <- TRUE
	d$trial_id <- paste0(d$trial_id, "_", d$planting_date)
	d <- unique(d)
	
	carobiner::write_files(path, meta, d)
}



