# R script for "carob"

carob_script <- function(path) {

"We investigated the effect of conservation agriculture based cropping systems in the yield and profitability of maize in a semiarid region of central Mexico, under rainfed conditions. The database contains yield data, production cost and profitability for maize (Zea mays L.), bean (Phaseolus vulgaris L.), triticale (× Triticosecale Wittmack) and oats (Avena sativa) in two field experiments and 17 farmer's fields."

	uri <- "hdl:11529/10549071"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		# change the major and minor versions if you see a warning
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institute = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method;crop_rotation",
		response_vars = "yield", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-07-30"
	)
	
	f <- ff[basename(ff) == "ConsAgri__Yield_Mexico2013-2020.xlsx"]
	r0 <- carobiner::read.excel(f, sheet="Data field experiments")
	r1 <- carobiner::read.excel(f, sheet="Data farmer's fields")

	d0 <- data.frame(
		location = r0$Site,
		planting_date = as.character(r0$Year),
		rep =as.integer(r0$Repetition),
		crop=tolower(r0$Crop),
		crop_rotation=tolower(r0$Crop_rotation),
		land_prep_method=tolower(r0$Till),
    	 treatment =as.character(r0$Treatment),
		yield = r0$`Yield (kg/ha)`,
		on_farm = FALSE
	)

	d1 <- data.frame( 
		adm2 = r1$Municipality,
		planting_date = as.character(r1$Year),
		crop=tolower(r1$Crop),
		crop_rotation=tolower(r1$Crop_rotation),
		land_prep_method= tolower(r1$Till), 
		treatment =as.character(r1$Treatment ),
		rain=r1$PP,
		yield = r1$`Yield (kg/ha)`,
		on_farm = TRUE
	)

	d <- carobiner::bindr(d0, d1)
	
	d$yield_part  <-  "grain"
	d$country  <-  "Mexico"
	d$trial_id  <-  as.character(as.integer(as.factor(paste(d$planting_date, d$location, d$adm2))))
	
	d$is_survey <- FALSE 
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d$irrigated <- NA

	d$crop[d$crop=="bean"] <- "common bean"
	#conventional=conv till, wide permanent beds="Permanent wide beds"
	d$land_prep_method[d$land_prep_method=="conventional tillage"] <- "conventional"
	d$land_prep_method[d$land_prep_method=="permanent wide beds"] <- "wide permanent beds"
	d$crop_rotation <- gsub("-", ";", d$crop_rotation)
	d$crop_rotation <- gsub("bean", "common bean", d$crop_rotation)

	
	d$latitude[d$adm2=="Cadereyta de Montes"] <- 25.5859879
	d$longitude[d$adm2=="Cadereyta de Montes"]  <-  -99.99681670000001
	d$latitude[d$adm2=="San Juan del Río"] <-  20.3951106 
	d$longitude[d$adm2=="San Juan del Río"] <- -99.9856344
	d$latitude[d$adm2=="Corregidora"] <-  20.5334645 
	d$longitude[d$adm2=="Corregidora"] <-  -100.4462861
	d$latitude[d$adm2=="Pedro Escobedo"] <-   20.5010443 
	d$longitude[d$adm2=="Pedro Escobedo"] <-  -100.1395169
	d$latitude[d$adm2=="Ezequiel Montes"] <-   20.6713387
	d$longitude[d$adm2=="Ezequiel Montes"] <-  -99.8962279
	d$latitude[d$adm2=="El Marqués"] <-   16.7956 
	d$longitude[d$adm2=="El Marqués"] <-  -99.8206
	
	
	carobiner::write_files(path, meta, d)
}

