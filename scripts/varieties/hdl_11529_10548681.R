# R script for "carob"

## ISSUES
# 1. Yield in the original dataset is in kg/ha and has high values.
# 2. 


carob_script <- function(path) {

"Nine varieties of chickpeas were evaluated. Some of the variables measured were Total biomass weight (g), Fresh weigth of the subsample (g), Dry weight of the subsample (g), Dry biomass yield (kg/ha) and Harvest area (m²). The experiment was conducted in El Batán, México and the results will be used to make recommendations to farmers about sustainable diversification. (2017-12-30)"

	uri <- "hdl:11529/10548681"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=1),
		data_institute = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "variety_code",
		response_vars = "yield", 
		carob_contributor = "Blessing Dzuda",
		carob_date = "2024-07-18"
	)
	
	f1 <- ff[basename(ff) == "DAT-BV106-17-Biomass_Chickpea.xlsx"]
	f2 <- ff[basename(ff) == "DAT-BV106-17-Yield_Chickpea.xlsx"]
	r1 <- carobiner::read.excel(f1, sheet ="Calculation")
	r2 <- carobiner::read.excel(f2, sheet = "Calculations")
  
    # r1 and r2 should have the same treatments/order
	stopifnot(all(r1[,1:4] == r2[,1:4]))
  
	d <- data.frame(
		country="Mexico",
		longitude= -100.202839,
		latitude= 20.312523,
		rep=r1$Rep,
		variety_code=as.character(r1$Trt),
		dmy_total=r1$Yield_Dry,
		fwy_total=r1$WTotal,		   
		yield=r2$WGrain,
		fwy_storage = r2$WGrain,
		dmy_storage = r2$WGrain * r2$WDry / r2$WFresh   ,
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
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	d$inoculated <- FALSE
  
	d$rep <- as.integer(d$rep)

	carobiner::write_files(path, meta, d)
}


