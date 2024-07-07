# R script for "carob"

carob_script <- function(path) {

"Grain yield data collected from Conservation Agriculture (CA) systems across experiments of varying experimental duration, established in trial locations of Malawi, Mozambique, Zambia, and Zimbabwe under an increasingly variable climate. Data contains different agro-environmental yield response moderators such as type of crop diversifcation and amount of rainfall and aims to identify cropping systems that may provide both short-term gains and longer-term sustainability."

	uri <- "hdl:11529/10548832"
	group <- "conservation_agriculture"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=1),
		data_institute = "CIMMYT",
		publication = "doi.org/10.1016/j.agee.2021.107812",
		project = NA,
		data_type = "experiment",
		treatment_vars = "yield", 
		carob_contributor = "Blessing Dzuda",
		carob_date = "2024-05-31"
	)
	
	f <- ff[basename(ff) == "DATA SA 2005 to 2019.xls"]
	r <- carobiner::read.excel(f, sheet = "Raw Data")

	d <- data.frame(
		trial_id="1",
		country="Malawi",
		location=r$Location,
		planting_date=as.character(r$Season),
		harvest_date=as.character(r$Season+1),
		rep=as.integer(r$Rep),
		rain=r$Rainfall,
		N_fertilizer=r$Nitrogen,
		P_fertilizer=r$Phosphorus,
		K_fertilizer=r$Potassium,
		soil_clay=r$Clay,
		soil_SOC=r$OrgC,
		biomass_yield=r$Biomass,
		yield=r$Grain*1000,
		yield_part="grain",
		crop="maize"
	)
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	
	carobiner::write_files(path, meta, d)
}



