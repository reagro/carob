# R script for "carob"

## ISSUES
# waring for Pieter Pypers to provide NPK quantities 


carob_script <- function(path) {

"
Nutrient omission trials cassava root yield data from Nigeria and Tanzania
ACAI is a 5 year Bill & Melinda Gates Foundation funded project in 5 countries in Africa (Nigeria and Tanzania) providing tailored agronomic advice to small scale cassava growers in the target countries. The project delivers agronomic solutions to improve cassava root yield and quality and the necessary knowledge base and applications for accessing this knowledge to cassava scaling partners and ultimately farmers in the target countries while instituting the necessary capacity and skills for national system scientists to engage in transformative cassava agronomy.
"
	uri <- "doi:10.25502/7XQN-BB55/D"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   project=NA,
	   uri=uri,
	   data_citation="Pypers, P., Vanlauwe, B., Tariku, M., Ampadu-Boakye, T., Kreye, C., Hauser, S., Baijukya, F., & Ogunsanmi, T. (2020). Nutrient omission trials cassava root yield data from Nigeria and Tanzania [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/7XQN-BB55/D",
	   publication= NA,
	   data_institutions = "IITA",
	   carob_contributor="Robert Hijmans",
	   data_type="experiment"
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)
	dset$license <- carobiner::get_license(js)

	f <- ff[basename(ff) == "ACAI_FR_forCKAN_2022.csv"]
	r <- read.csv(f)

	d <- data.frame(
		dataset_id = dataset_id,
		record_id =r$ID,
		trial_id= r$trial_ID,
		country=r$Country,
		adm1=r$State_Region,
		adm2=r$LGA_District,
		longitude=r$lon, 
		latitude=r$lat,
		crop="cassava",
		planting_date = as.character(as.Date(r$plantingDate, "%d/%m/%Y")),
		harvest_date = as.character(as.Date(r$Hdate, "%d/%m/%Y")),	
		treatment = r$treat,
		fertilizer_type="unknown",
		yield_part="roots",
		yield = r$rootYield_t_ha * 1000
	)


	x <- data.frame(
		treatment=c("CON", "half_NPK", "NK", "NP", "NPK", "NPK_micro", "PK"),
		N_fertilizer= as.numeric(NA),
		P_fertilizer=as.numeric(NA),
		K_fertilizer=as.numeric(NA)
	)
	x[x$treatment=="NK", "P_fertilizer"] <- 0
	x[x$treatment=="NP", "K_fertilizer"] <- 0
	x[x$treatment=="PK", "N_fertilizer"] <- 0
	x[grep("N", x$treatment), "N_fertilizer"] <- NA
	x[grep("P", x$treatment), "P_fertilizer"] <- NA
	x[grep("K", x$treatment), "K_fertilizer"] <- NA
	x[grep("half", x$treatment), 2:4] <- 0.5 * x[grep("half", x$treatment), 2:4]
	#x[grep("micro", x$treatment), "???"] <- NA
	x[x$treatment=="CON", 2:4] <- 0

	d <- merge(d, x, all.x=TRUE, by="treatment")
	carobiner::write_files(dset, d, path=path)
}

