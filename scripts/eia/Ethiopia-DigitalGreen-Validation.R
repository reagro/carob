# R script for EiA version of"carob"

## ISSUES
# 1. DOI and much of the metadata is missing
# 2. License is missing (CC-BY)?
# 3. ...

carob_script <- function(path) {
	
  "
	SOME DESCRIPTION GOES HERE...

"
  
  uri <- "doi:Ethiopia-DigitalGreen-Validation"
  group <- "eia"
  
  dset <- data.frame(
    # Need to fill-in metadata...
    # carobiner::read_metadata(uri, path, group, major=2, minor=0),
    uri = carobiner::simple_uri(uri),
    dataset_id = uri,
    data_institutions = "Alliance Bioversity - CIAT",
    authors = "Lulseged Desta & Wuletawu Abera",
    title = "Digital Green Ethiopia Use Case Validations 2022",
    description = "Data for the use case validaton of Site-Specific Recommendations (SSR) for Ethiopia 2022",
    group = group,
    license = 'Some license here...',
    carob_contributor = 'IITA Biometric Unit',
    data_citation = '...',
    project = 'Excellence in Agronomy - Digital Green Ethiopia Validation',
    data_type = "on-farm experiment", # or, e.g. "on-farm experiment", "survey", "compilation"
    carob_date="2024-04-25",
    treatment_vars = "N_fertilizer;P_fertilizer;S_fertilizer"
  )
  
  # Manually build path (this can be automated...)
  ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("/home/jovyan/carob-eia/data/raw/eia/Ethiopia-DigitalGreen-Validation/", full.names = T))
  
  # Retrieve relevant file
  f <- ff[basename(ff) == "1 DG_CIAT_wheat_usecase_KPI_calculation_Sept_2023.xlsx"]
  
  # Read relevant file
  r <- carobiner::read.excel(f,
                             sheet = "data",
                             col_names = TRUE,
                             skip = 5)
  
  p <- carobiner::read.excel(f,
                             sheet = "price_RF",
                             col_names = TRUE)
  
  colnames(p) <- c("adm2", "crop_price", "NPS fertilizer price ETB per kg", "Urea fertilizer price ETB per kg", "rain")
  p$fertilizer_price <- paste0("NPS=", p$`NPS fertilizer price ETB per kg`, "; Urea=", p$`Urea fertilizer price ETB per kg`)
  p$currency <- "ETB"
  p <- p[, c("adm2", "crop_price", "fertilizer_price", "currency", "rain")]

  # Build initial DF ... Start from here
	d <- data.frame(
		country = "Ethiopia",
		crop = "Wheat",
		yield_part = "grain",	
		on_farm = TRUE,
		is_survey = TRUE,
		adm1=r$Region,
		adm2=r$District,
		trial_id = r$HHID, # Using HHID as trial_id
		latitude =r$LAT,
		longitude=r$LONG,
		elevation=r$ALT,
		date=r$Year,
		treatment=r$TRT,
		fertilizer_type = "urea;NPKS",
		N_fertilizer=r$`N (kg/ha)`,
		P_fertilizer=r$`P (kg/ha)`,
		S_fertilizer=r$`S (kg/ha)`,
		dmy_total=r$`BM (t/ha)`, #We assumed BM is dmy_total because the addition of GY & SW equals BM in the dataset
		yield=r$`GY(t/ha)`,
		residue_yield=r$`SW(t/ha)`, #The straw weight is assumed to be the residue of the yield
		harvest_index=r$HI  #New variable (harvest_index) created
	)

	# Convert the date to character
	d$date <- as.character(d$date)
	
	# Convert variables from t/ha to kg/ha
	d$dmy_total <- d$dmy_total * 1000
	d$yield <- d$yield * 1000
	d$residue_yield <- d$residue_yield * 1000
	
	
	# EGB:
	# # DO NOT DO THIS...
	
	# # Convert out of range values to NAs
	# d$N_fertilizer[d$N_fertilizer > 600] <- NA
	# d$P_fertilizer[d$P_fertilizer > 250] <- NA
	# d$S_fertilizer[d$S_fertilizer > 250] <- NA
	# 
	# d$dmy_total[d$dmy_total > 100000] <- NA
	# d$yield[d$yield > 150000] <- NA
	# d$residue_yield[d$residue_yield > 60000] <- NA
	
	# EGB:
	# # Add the actual treatment names
	d$treatment[d$treatment == "STC"] <- "Standard check"
	d$treatment[d$treatment == "LOC"] <- "local check"
	d$treatment[d$treatment == "SSR"] <- "Site-specific-rate"
	
	# EGB:
	# # Add ancillary information on price, rain
	d <- merge(d, p, by = "adm2")
	
	carobiner::write_files(dset, d, path=path)
}


