# R script for "carob"

## ISSUES
# Treatment contains numbers with no description on what the numbers mean


carob_script <- function(path) {

"We investigated the effect of conservation agriculture based cropping systems in the yield and profitability of maize in a semiarid region of central Mexico, under rainfed conditions. The database contains yield data, production cost and profitability for maize (Zea mays L.), bean (Phaseolus vulgaris L.), triticale (× Triticosecale Wittmack) and oats (Avena sativa) in two field experiments and 17 farmer's fields.

"

## Identifiers
	uri <- "hdl:11529/10549071"
	group <- "agronomy"

## Download data 
	ff  <- carobiner::get_data(uri, path, group)

## metadata 
	meta <- data.frame(
		# change the major and minor versions if you see a warning
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institute = "CIMMYT",
		# if there is a paper, include the paper's doi here
		# also add a RIS file in references folder (with matching doi)
		publication = NA,
		project = NA,
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		data_type = "experiment",
		# treatment_vars has semi-colon separated variable names that represent the
		# treatments if the data is from an experiment. E.g. "N_fertilizer;P_fertilizer;K_fertilizer"
		treatment_vars = "treatment",
		# response variables of interest such as yield, fwy_residue, disease incidence, etc. Do not include variable that describe management for all treatments or other observations that were not related to the aim of the trial (e.g. the presence of a disease).
		response_vars = "yield", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-07-30"
	)
	
## read data 

	f <- ff[basename(ff) == "ConsAgri__Yield_Mexico2013-2020.xlsx"]
  r <- carobiner::read.excel(f, sheet="Data field experiments")
  r1 <- carobiner::read.excel(f, sheet="Data farmer's fields")

## select the variables of interest and assign them to the correct name
	d0 <- data.frame(
	
## make sure that names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
## first use "location", then use "site"
	
		adm2 = r$Site,
    rep =as.integer(r$Repetition),
    crop=tolower(r$Crop),
    crop_rotation=tolower(r$Crop_rotation),
    land_prep_method=r$Till, #conventional=conv till, wide permanent beds="Permanent wide beds"
	## the treatment code/description (if any)	
		 treatment =as.character(r$Treatment), #there's no description of the treatment method,
	   rain=NA
	)
	d1 <- data.frame(
	  
	  ## make sure that names are normalized (proper capitalization, spelling, no additional white space).
	  ## you can use carobiner::fix_name()
	  ## first use "location", then use "site"
	  
	  adm2 = r1$Municipality,
	  rep=NA,
	  crop=tolower(r1$Crop),
	  crop_rotation=tolower(r1$Crop_rotation),
	  land_prep_method=r1$Till, #conventional=conv till, wide permanent beds="Permanent wide beds"
	  ## the treatment code/description (if any)	
	  treatment =as.character(r1$Treatment ),
	  rain=r1$PP
	)
d <- carobiner::bindr(d0, d1)
# fixing bad data
	d$crop[d$crop=="bean"]<-"common bean"
	d$land_prep_method[d$land_prep_method=="Conventional Tillage"]<-"conventional"
	d$land_prep_method[d$land_prep_method=="Permanent wide beds"]<-"wide permanent beds"
	d$crop_rotation[d$crop_rotation=="maize-maize"]<-"maize;maize"
	d$crop_rotation[d$crop_rotation=="maize-oats"]<-"maize;oats"
	d$crop_rotation[d$crop_rotation=="oats-maize"]<-"oats;maize"
	d$crop_rotation[d$crop_rotation=="maize-bean"]<-"maize;common bean"
	d$crop_rotation[d$crop_rotation=="bean-maize"]<-"common bean;maize"
	d$crop_rotation[d$crop_rotation=="maize-triticale"]<-"maize;triticale"
	d$crop_rotation[d$crop_rotation=="triticale-maize"]<-"triticale;maize"
	d$adm2[d$adm2=="San Juan del Río"]<-"San Juan del Rio"
	d$adm2[d$adm2=="Cadereyta de Montes"]<-"Cadereyta"
	
## separate individual trials. For example trials in different locations or years. 
## do _not_ separate by treatments within a trial. For a survey, each row gets a unique trial_id
	d$trial_id <- as.character(as.integer(as.factor("1")))
	
## about the data (TRUE/FALSE)
	d$on_farm <- TRUE
	
## each site must have corresponding longitude and latitude
## if the raw data do not provide them you can estimate them from the location/adm data 
## see carobiner::geocode
	d$latitude[d$adm2=="Cadereyta"]<-25.5859879
	d$longitude[d$adm2=="Cadereyta"] <- -99.99681670000001
	d$latitude[d$adm2=="San Juan del Rio"]<- 20.3951106 
	d$longitude[d$adm2=="San Juan del Rio"]<--99.9856344
	d$latitude[d$adm2=="Corregidora"]<- 20.5334645 
	d$longitude[d$adm2=="Corregidora"]<- -100.4462861
	d$latitude[d$adm2=="Pedro Escobedo"]<-  20.5010443 
	d$longitude[d$adm2=="Pedro Escobedo"]<- -100.1395169
	d$latitude[d$adm2=="Ezequiel Montes"]<-  20.6713387
	d$longitude[d$adm2=="Ezequiel Montes"]<- -99.8962279
	d$latitude[d$adm2=="El Marqués"]<-  16.7956 
	d$longitude[d$adm2=="El Marqués"]<- -99.8206
	
	

	d$planting_date <- "2013"
### in general, add comments to your script if computations are
### based on information gleaned from metadata, a publication, 
### or when they are not immediately obvious for other reasons

### Yield

	yield <- r$`Yield (kg/ha)`
	#what plant part does yield refer to?
	d$yield_part <- "grain"
	d$country <- "Mexico"
	
	
# all scripts must end like this
	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

