# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"The dataset contains experimental data from the San Juan Cotzocon research platform in 
Oaxaca, Mexico. The database contains data on maize yield, management, profitability and
phenology in relation to 8 evaluated treatments. The treatments differed in tillage
(disk harrow, zero tillage or permanent raised beds), residue management (removing all or 
leaving all), fertilization (conventional and soil analysis based) and crop rotation 
(monoculture maize or rotation with legume)

"
#### Identifiers
	uri <- "hdl:11529/10548976"
	group <- "conservation_agriculture"

# the script filename should be paste0(dataset_id, ".R")
	dataset_id <- carobiner::simple_uri(uri)

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)

##### dataset level metadata 
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group=group,dataset_id=dataset_id),
		data_citation=" Fonteyne, Simon; Guera, Ouorou Ganni Mariel; Villa Alcántara, 
		Jonatan; Núñez Peñaloza, Omar; Verhulst, Nele, 2023, Maize yield and profitability 
		in a 5 year conservation agriculture experiment in Papaloapan, Oaxaca, 
		https://hdl.handle.net/11529/10548976, CIMMYT Research Data & Software Repository 
		Network, V1",
		data_institutions = "CIMMYT",
		publication= NA,
		project=NA,
		data_type= "experiment",
		carob_contributor= "Shumirai Manzvera",
		carob_date="2024-03-14"
	)
	
##### PROCESS data records

# read data 

	f <- ff[basename(ff) == "DAT-SJCotzoconExperiments.xlsx"]
	r <- carobiner::read.excel(f, sheet="San Juan Cotzocon_OAX")

## process file(s)

## use a subset
	d <- data.frame(crop="maize",adm1=r$Estado,latitude=r$Latitud, longitude=r$Longitud, 
	                elevation=r$Altitud, treatment=r$Name_tr,rep=r$Num_Rep,crop_rotation="maize;velvet bean;kidney bean ",
	                variety=r$Variety,plant_density=r$Seed_dens,
	                K_fertilizer=r$Fert_K,P_fertilizer=r$Fert_P,N_fertilizer=r$Fert_N,row_spacing=r$Row_dist,
	                land_prep_method=r$Till,plant_height=r$Height,planting_date=r$Sowing_date,emergence_date=r$Emergence_date,
	                harvest_date=r$Harvest_date,yield=r$Yield_moist,trial_id="1", yield_part="grain" )
 
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- FALSE

	d$country <- "Mexico"

	
##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.


	d$planting_date <- as.character(d$planting_date)
	d$emergence_date<- as.character(d$emergence_date)
	d$harvest_date<-as.character(d$harvest_date)
  
	d$rep<-as.integer(d$rep)
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

