# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

" The experiment was conducted in 2016 and 2017 in three regions in Oaxaca, Mexico. The design of the study was to evaluate fifteen combinations of tillage and weed management practices at each of the three sites, with each trial consisting of three blocks with a different type of tillage: zero tillage (ZT), minimum tillage (MT) and conventional tillage (CT). The first site is in the “Sitio Experimental Mixteca” research station in Santo Domingo Yanhuitlán, the Mixteca Region, located at 2195 m above sea level (masl), it has Vertisol soils, a temperate subhumid climate. The second site is in the town of San Felipe Zihualtepec in the municipality of San Juan Cotzocón, Papaloapan Region, located at 60 masl and has Luvisol soils, a hot humid climate. The third location was in the town of Ciénega de Zimatlán in the municipality with the same name, Valles Centrales Region. The original site was changed in 2017 to a nearby field, because the collaborating farmer did not want to continue the trial. Both fields are located at 1552 masl, have Vertisol soils, a hot semi-arid climate. Weed management treatments were conducted similarly in all tillage treatments and reflected common local practices and available herbicides or equipment, as well as weather, soil moisture and weed species and comprised combinations of the following: 1) MEC, mechanical control. Weeds were mechanically controlled after reaching 20 cm in height approximately 20-25 days after sowing (DAS), as per the common practice. Weeding was carried out with a hand hoe by 4 to 10 workers per hectare, depending on the quantity of weeds, or using a tractor-drawn cultivator. 2)PRE, pre-emergent herbicide. Only a pre-emergent herbicide with residual effect was applied before sowing. 3) POST, post-emergent herbicide. When weeds reached 5-10 cm in height and based on soil moisture conditions and the types of weeds present, a selective herbicide or direct contact herbicide was applied. 4) PRE+POST, integrated weed management. A pre-emergent herbicide was applied, followed by post-emergence control as necessary, using either selective herbicides or manual controls. 5) CONT, control: No weed management was practiced. The dataset contains the data on maize yield, weed density, weed species (broadleaf or narrowleaf) and weed biomass from the experiment. (2021-04-08)
  
"

## Identifiers
	uri <- "hdl:11529/10548568"
	group <- "weeds"

## Download data 
	ff  <- carobiner::get_data(uri, path, group)

## metadata 
	meta <- data.frame(
		# change the major and minor versions if you see a warning
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institute = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		# treatment_vars has semi-colon separated variable names that represent the
		# treatments if the data is from an experiment. E.g. "N_fertilizer;P_fertilizer;K_fertilizer"
		treatment_vars = "land_prep;treatment", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-07-11"
	)
	
## read data 

	f <- ff[basename(ff) == "PUB-DAT-WeedsOaxaca.xlsx"]
	r <- read.csv(f)
  r <- carobiner::read.excel(f, sheet = "Yield data")
  r1<-carobiner::read.excel(f, sheet = "Weed data")
## select the variables of interest and assign them to the correct name
	d0 <- data.frame(
		adm3= r$Site,
		rep=r$Rep,
		plant_height=r$`Plant height (m)`,
		yield=r$`Yield (t/ha 14% moisture)`,
		land_prep=r$Tillage,
		treatment=r$`Weed Management`)
	d1<- data.frame(
	  adm3= r1$Site,
	  land_prep=r1$Tillage,
	  treatment=r1$Treatment,
	  rep= r1$Rep,
	  weed_biomass=r1$FW )
	
	  d1$land_prep<- gsub("CT","conventional",d1$land_prep)
	  d1$land_prep<- gsub("ZT","none",d1$land_prep)
	  d1$land_prep<- gsub("MT","reduced tillage",d1$land_prep)
		
	
    d1$trial_id[d1$planting_date=="2016"] <- "1"
    d1$trial_id[d1$planting_date=="2017"] <- "2"
    ## separate individual trials. For example trials in different locations or years. 
## do _not_ separate by treatments within a trial. For a survey, each row gets a unique trial_id
	d$trial_id <- as.character(as.integer(as.factor( ____ )))
	
## about the data (TRUE/FALSE)
	d$on_farm <- TRUE

### Location
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "Mexico"
	d$adm1 <- "Oaxaca"

## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d1$longitude [d1$adm3=="Mixteca"]<- -96.8578
	d1$latituded [d1$adm3=="Mixteca"]<-16.9294
	d1$longitude [d1$adm3=="Papaloapan"]<- -96.094722199
	d1$latitude [d1$adm3=="Papaloapan"]<-18.1591666
	d1$longitude [d1$adm3=="Valles Centrales"]<- -96.48651
	d1$latitude [d1$adm3=="Valles Centrales"]<-16.92554

### Crop 
## normalize variety names
## see carobiner::fix_name
	d$crop <- "maize"

### Time 
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- as.character(as.Date(d1$   ))
	d$harvest_date  <- as.character(as.Date(    ))

	d$yield_part <- "grain"
	
# all scripts must end like this
	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

