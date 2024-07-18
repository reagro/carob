# R script for "carob"

## ISSUES
# 1. Yield in the original dataset is in kg/ha and has high values.
# 2. 


carob_script <- function(path) {

"
Nine varieties of chickpeas were evaluated. Some of the variables measured were Total biomass weight (g), Fresh weigth of the subsample (g), Dry weight of the subsample (g), Dry biomass yield (kg/ha) and Harvest area (m²). The experiment was conducted in El Batán, México and the results will be used to make recommendations to farmers about sustainable diversification. (2017-12-30)
"

## Identifiers
	uri <- "hdl:11529/10548681"
	group <- "agronomy"

## Download data 
	ff  <- carobiner::get_data(uri, path, group)

## metadata 
	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=1),
		data_institute = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "crop_variety",
		# response variables of interest such as yield, residue_yield, disease incidence, etc. Do not include variable that describe management for all treatments or other observations that were not related to the aim of the trial (e.g. the presence of a disease).
		response_vars = "yield", 
		carob_contributor = "Blessing Dzuda",
		carob_date = "2024-07-18"
	)
	
## read data 

	f1 <- ff[basename(ff) == "DAT-BV106-17-Biomass_Chickpea.xlsx"]
	f2 <- ff[basename(ff) == "DAT-BV106-17-Yield_Chickpea.xlsx"]
  r1 <- carobiner::read.excel(f1, sheet ="Calculation")
  r2 <- carobiner::read.excel(f2, sheet = "Calculations")
  
	d <- data.frame(
	     country="Mexico",
	     longitude="-100.202839",
	     latitude="20.312523",
		   rep=r1$Rep,
		   treatment=r1$Trt,
		   dmy_total=r1$WTotal,
		   yield=r1$Yield_Dry,
		   flowering_days=r2$Flowering,
		   maturity_days=r2$Maturity,
		   seed_weight=r2$Thou,
		   yield_part="grain",
		   crop="chickpea",
		   planting_date="2017",
		   trial_id="1"
		   
	)

	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <-FALSE
	d$P_fertilizer <- NA
  d$K_fertilizer <- NA
  d$N_fertilizer <- NA
  d$S_fertilizer <- NA
  d$inoculated <- FALSE
  
  d$longitude <- as.numeric(d$longitude)
  d$latitude <- as.numeric(d$latitude)
  d$rep <- as.integer(d$rep)
  d$treatment <- as.character(d$treatment)
  d$P_fertilizer <- as.numeric(d$P_fertilizer)
  d$k_fertilizer <- as.numeric(d$K_fertilizer)
  d$N_fertilizer <- as.numeric(d$N_fertilizer)
  d$S_fertilizer <- as.numeric(d$S_fertilizer)


# all scripts must end like this
	carobiner::write_files(path, meta, d)
}


