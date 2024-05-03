# R script for "carob"

carob_script <- function(path) {

"Nutrient omission trials cassava root yield data from Nigeria and Tanzania
ACAI is a 5 year Bill & Melinda Gates Foundation funded project in 5 countries in Africa (Nigeria and Tanzania) providing tailored agronomic advice to small scale cassava growers in the target countries. The project delivers agronomic solutions to improve cassava root yield and quality and the necessary knowledge base and applications for accessing this knowledge to cassava scaling partners and ultimately farmers in the target countries while instituting the necessary capacity and skills for national system scientists to engage in transformative cassava agronomy."

	uri <- "doi:10.25502/7XQN-BB55/D"
	group <- "fertilizer"

	ff <- carobiner::get_data(uri, path, group)


	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=1),
		project=NA,
		publication= NA,
		data_institutions = "IITA",
		carob_contributor="Robert Hijmans",
		carob_date="2023-07-07",
		data_type="on-farm experiment"
	)

	f <- ff[basename(ff) == "ACAI_FR_forCKAN_2022.csv"]
	r <- read.csv(f)

	d <- data.frame(
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
		yield_part="roots",
		yield = r$rootYield_t_ha * 1000
	)

	# according to forthcoming paper; correspondence with P. Pypers 
	x <- data.frame(
		treatment = c("CON", "half_NPK", "NK", "NP", "NPK", "NPK_micro", "PK"), 
		N_fertilizer =  c(0, 75, 150, 150, 150, 150,   0), 
		P_fertilizer =  c(0, 20,   0,  40,  40,  40,  40), 
		K_fertilizer =  c(0, 90, 180,   0, 180, 180, 180), 
		Ca_fertilizer = c(0, 0, 0, 0, 0, 10, 0), 
		Mg_fertilizer = c(0, 0, 0, 0, 0, 10, 0), 
		S_fertilizer =  c(0, 0, 0, 0, 0, 17, 0), 
		Zn_fertilizer = c(0, 0, 0, 0, 0,  5, 0), 
		B_fertilizer =  c(0, 0, 0, 0, 0,  1, 0), 
		fertilizer_type = c("none", "urea; TSP; KCl", "urea; KCl", "urea; TSP", "urea; TSP; KCl", "urea; TSP; KCl; CaCO3; MgSO4; ZnSO4; H3BO3", "TSP; KCl")
	)
	
	d <- merge(d, x, all.x=TRUE, by="treatment")
	carobiner::write_files(dset, d, path=path)
}

