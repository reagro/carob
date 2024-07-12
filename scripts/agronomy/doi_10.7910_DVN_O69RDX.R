# R script for "carob"

carob_script <- function(path) {
  
"
The objective of the study is to test different plant arrangements between maize and Gliricidia sepium and evaluate its effects on soil quality and productivity. Below is the list of treatments applied during the experiment.
1. Traditional Maize- Groundnuts rotation [with half recommended fertilizer on maize, no fertilizer on groundnuts]
2. Maize-Groundnut rotation with Gliricidia [ Maize/Gliricidia (COMACO’s Gliricidia spacing: 5m x 1m) – Groundnuts/Gliricidia]
3. Doubled up Maize-Groundnut rotation with Gliricidia [Maize/Gliricidia (Dispersed shading spacing; 10m x 5m)/pigeonpea – Groundnuts/Gliricidia/Pigeonpea] "
  
  uri <- "doi:10.7910/DVN/O69RDX"
  group <- "agronomy"
  ff <- carobiner::get_data(uri, path, group)
 
  meta <- data.frame(
  	carobiner::read_metadata(uri, path, group, major=1, minor=2),
    project=NA,
    publication= NA,
    data_institute = "CIMMYT",
    data_type="experiment",
    carob_contributor="Fredy Chimire",
    carob_date="2024-01-16"
  )
   
 
  f <- ff[basename(ff) == "AR_ZAM_CIMMYT_Gliricidia_onstation_2020.csv"]
  
  # Select sheet with revised data from the excel file 
  r <- read.csv(f)
  
	d <- data.frame(
		country= r$Country,
		planting_date=as.character(NA),
		harvest_date=as.character(r$Year),
		rep= r$Rep,
		crop= tolower(r$Crop),
		intercrops=tolower(r$Intercrop),
		adm2=r$District,
		location=r$Location,
		dmy_total = r$biomass, 
		yield = r$grainyield,
		treatment = r$Treat
	)
  
	d$crop <- gsub("groundnuts|grou", "groundnut", d$crop)
	d$crop <- gsub("pigeonpea", "pigeon pea", d$crop)
	d$intercrops <- gsub("pigeonpea", "pigeon pea", d$intercrops)
	d$intercrops <- gsub("/", ";", d$intercrops)
	d$intercrops <- gsub("nil", "none", d$intercrops)

  # for first dataset
  
  
  d$is_survey <- FALSE
  d$on_farm <- TRUE
  # description of treaments
  d$treatment <- c(
	"Traditional Maize- Groundnuts rotation [with half recommended fertilizer on maize, no fertilizer on groundnuts]",
	"Maize-Groundnut rotation with Gliricidia [ Maize/Gliricidia (COMACO’s Gliricidia spacing: 5m x 1m) – Groundnuts/Gliricidia]",
    "Doubled up Maize-Groundnut rotation with Gliricidia [Maize/Gliricidia (Dispersed shading spacing; 10m x 5m)/pigeonpea – Groundnuts/Gliricidia/Pigeonpea]")[d$treatment]
  
  d$yield_part <- "grain"
  
  # https://www.findlatitudeandlongitude.com/l/Msekera+Chipata+Zambia/5548305/
  d$latitude <- -13.64451
  d$longitude <- 32.6447
  
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d$trial_id <- "1"
	d$irrigated <- FALSE
	
  carobiner::write_files(meta, d, path=path)
}



