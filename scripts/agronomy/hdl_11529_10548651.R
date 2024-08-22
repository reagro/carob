# R script for "carob"

## ISSUES
# if there are remaining issues, please put these in "meta$notes"


carob_script <- function(path) {

"
copy and paste the abstract from the repository. Do not add line breaks
"

## Identifiers
	uri <- "hdl:11529/10548651"
	group <- "agronomy"

## Download data 
	ff  <- carobiner::get_data(uri, path, group)

## metadata 
	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=2),
		data_institute = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "herbicide_product",
		response_vars = "yield", 
		carob_contributor = "Blessing Dzuda",
		carob_date = "2024-08-20",
		notes = ""
	)
	
## read data 

	f <- ff[basename(ff) == "DAT-BV101_MAIZE-16.xlsx"]
	r <- carobiner::read.excel(f, sheet = "Calculations_Maize")

## select the variables of interest and assign them to the correct name
	d <- data.frame(
		country = "Mexico",
		adm3="El Batán",
		latitude=19.5275,
		longitude=-98.8559,
		trial_id=1,
		planting_date=2016,
		crop= "maize",
		treatment= r$TrT,
		rep= r$Rep,
		tassling_days=r$Tasseling,
		silking_days=r$Silking,
		maturity_days=r$Maturity,
		plant_height=r$Height,
		harvest_index=r$HI,
		dmy_stems=r$DryBiomass10,
		dmy_total=r$TotBiomass10,
		yield_moisture=r$`%HumGrain`,
		yield=r$`Yield_12%H2O`,
		fwy_storage=r$`Yield/BiomCobs`*1000,
		yield_part="grain",
		seed_weight=r$Thou,
		seed_density=r$`Grains/m²`,
		asi=r$ASI,
		flowering_days=r$Flowering
	)

## about the data (TRUE/FALSE)
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <-FALSE
	d$geo_from_source <- TRUE
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d$trial_id <- as.character(d$trial_id)
	d$planting_date <- as.character(d$planting_date)
	d$treatment <- as.character(d$treatment)
	d$rep <- as.integer(d$rep)
	
	# all scripts must end like this
	carobiner::write_files(path, meta, d)

}

