# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

" The experiment was conducted in 2016 and 2017 in three regions in Oaxaca, Mexico. The design of the study was to evaluate fifteen combinations of tillage and weed management practices at each of the three sites, with each trial consisting of three blocks with a different type of tillage: zero tillage (ZT), minimum tillage (MT) and conventional tillage (CT). The first site is in the “Sitio Experimental Mixteca” research station in Santo Domingo Yanhuitlán, the Mixteca Region, located at 2195 m above sea level (masl), it has Vertisol soils, a temperate subhumid climate. The second site is in the town of San Felipe Zihualtepec in the municipality of San Juan Cotzocón, Papaloapan Region, located at 60 masl and has Luvisol soils, a hot humid climate. The third location was in the town of Ciénega de Zimatlán in the municipality with the same name, Valles Centrales Region. The original site was changed in 2017 to a nearby field, because the collaborating farmer did not want to continue the trial. Both fields are located at 1552 masl, have Vertisol soils, a hot semi-arid climate. Weed management treatments were conducted similarly in all tillage treatments and reflected common local practices and available herbicides or equipment, as well as weather, soil moisture and weed species and comprised combinations of the following: 1) MEC, mechanical control. Weeds were mechanically controlled after reaching 20 cm in height approximately 20-25 days after sowing (DAS), as per the common practice. Weeding was carried out with a hand hoe by 4 to 10 workers per hectare, depending on the quantity of weeds, or using a tractor-drawn cultivator. 2)PRE, pre-emergent herbicide. Only a pre-emergent herbicide with residual effect was applied before sowing. 3) POST, post-emergent herbicide. When weeds reached 5-10 cm in height and based on soil moisture conditions and the types of weeds present, a selective herbicide or direct contact herbicide was applied. 4) PRE+POST, integrated weed management. A pre-emergent herbicide was applied, followed by post-emergence control as necessary, using either selective herbicides or manual controls. 5) CONT, control: No weed management was practiced. The dataset contains the data on maize yield, weed density, weed species (broadleaf or narrowleaf) and weed biomass from the experiment. (2021-04-08)"

	uri <- "hdl:11529/10548568"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institute = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method;weeding_method", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-07-11"
	)

	f <- ff[basename(ff) == "PUB-DAT-WeedsOaxaca.xlsx"]
	r1 <- carobiner::read.excel(f, sheet = "Yield data")
	r2 <- carobiner::read.excel(f, sheet = "Weed data")

	d1 <- data.frame(
		adm3= r1$Site,
		rep=r1$Rep,
		plant_height=r1$`Plant height (m)`,
		yield=r1$`Yield (t/ha 14% moisture)`,
		land_prep=r1$Tillage,
		treatment=r1$`Weed Management`
	)

	d2 <- data.frame(
		adm3= r2$Site,
		land_prep=r2$Tillage,
		treatment=r2$Treatment,
		rep= r2$Rep,
		weed_biomass=r2$FW 
	)
	
	d2$land_prep_method <- gsub("CT", "conventional", d2$land_prep)
	d2$land_prep_method <- gsub("ZT", "none", d2$land_prep)
	d2$land_prep_method <- gsub("MT", "reduced tillage", d2$land_prep)
		
	
    d2$trial_id <- d2$planting_date=="2016"] <- "1"
    d2$trial_id[d2$planting_date=="2017"] <- "2"


	d2$longitude[d2$adm3=="Mixteca"] <- -96.8578
	d2$latituded[d2$adm3=="Mixteca"] <- 16.9294
	d2$longitude[d2$adm3=="Papaloapan"] <- -96.094722199
	d2$latitude[d2$adm3=="Papaloapan"] <- 18.1591666
	d2$longitude[d2$adm3=="Valles Centrales"] <- -96.48651
	d2$latitude[d2$adm3=="Valles Centrales"] <- 16.92554

	d <- merge(d1, d2, by =
	
	d$on_farm <- TRUE

	d$country <- "Mexico"
	d$adm1 <- "Oaxaca"

	d$crop <- "maize"

	d$planting_date <- as.character(as.Date(d2$   ))
	d$harvest_date  <- as.character(as.Date(    ))

	d$yield_part <- "grain"
	
	carobiner::write_files(path, meta, d)
}

