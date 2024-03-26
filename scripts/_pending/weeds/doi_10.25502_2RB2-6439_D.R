# R script for "carob"

## ISSUES
# ....

## RH the treatments and the temporal info need to be extracted

carob_script <- function(path) {
  
  "
	Description:
   The ‘Sustainable Weed Management Technologies for Nigeria’ 
   was a 5-year project that was developed and assessed with 
   smallholder farmer participation modern, relevant and appropriate
   cassava weed management technologies suitable for sustainable 
   intensification in major agro-ecological (humid rainforest,
   forest transition savanna and southern Guinea savanna) and
   socio-economic conditions of Nigeria. An important goal of 
   the project was to help smallholder cassava growers achieve 
   sustainable increases in their productivity and incomes through 
   the development and adoption of improved weed control methods
"
  
	uri <- "doi:10.25502/2RB2-6439/D"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "weeds"
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	## dataset level data 
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
		publication= NA, #"DOI:10.1564/v27_oct_04"
		Agriculture (IITA). https://doi.org/10.25502/2RB2-6439/D",
		data_institutions = "IITA",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-06-25",
		data_type="experiment",
		project=NA 
	)
	
	
	f <- ff[basename(ff) == "Agro2015_1st_Season_All_Locations_Cas_Datafile_Rft.csv"] 
	f1 <- ff[basename(ff) == "Agro2015_2nd_Season_All_Locations_Cas_Datafile_Rft.csv"] 
	
	# read the dataset
	r1 <- read.csv(f)
	r2 <- read.csv(f1)
	
	# process file(s)
### use names, not numbers for variables. numbers are not safe against updates.
#	d1 <- r1[, c(2,4,5,6,7,8,9,10,11,12,15,25,26,27,28)] 
#	colnames(d1) <- c("trial_id","season","location","site","rep","tillage","crop","treatment","variety","plant_density","yield","latitude","longitude","planting_date","harvest_date")

	sel <- c('UniqueID', 'Season', 'Loc', 'Site', 'Rep', 'Tillage', 'cropSystem', 'Fertilizer', 'Variety', 'Density', 'YLDOKfr_kgm2', 'Lat', 'Long', 'Date_Planted_Cas', 'Date_Harvested_Cas')
	d1 <- r1[,sel]
	d1 <- carobiner::change_names(d1, sel,
		c("trial_id", "season", "location", "site", "rep", "tillage", "crop", "treatment", "variety","plant_density","yield", "latitude", "longitude", "planting_date", "harvest_date"))

	# soil information
	d1$soil_pH <- (r1$pH_d10 + r1$pH_d20)/2
	d1$soil_SOC <- (r1$OC_d10 + r1$OC_d20)/2
#P_d10	Phosphorus (ppm) @ Soil Depth: 0-10 cm	
	d1$soil_P_available <- (r1$P_d10 + r1$P_d20)/2

	d1$soil_N <- (r1$N_d10 + r1$N_d20)/2
	d1$soil_K <- (r1$K_d10 + r1$K_d20)/2
	d1$soil_Ca <- (r1$Ca_d10 + r1$Ca_d20)/2
	d1$soil_Mg <- (r1$Mg_d10 + r1$Mg_d20)/2
	weeds1 <- r1[, c("biomass_04WAP_gm2", "biomass_08WAP_gm2", "biomass_12WAP_gm2", "biomass_24WAP_gm2")]
	# mean weed biomass in kg/ha	
	d1$weeds_biomass <- rowMeans(weeds1) * 10
		
	#d1$weed_biomass <- ((r1$dmy_04WAP_gm2+r1$dmy_08WAP_gm2+r1$dmy_12WAP_gm2+r1$dmy_24WAP_gm2)/2)*1000 # in kg/ha
	
	#N_d10	% Nitrogen
	# to convert Nitrogen in mg/kg (PPm) we use: 1g of soil contain n% of Nitrogen 
	d1$soil_N <- (d1$soil_N/100)*1000000 # mg/kg
	# #K_d10	Potassium (C mol/kg),#Mg_d10	Magnesium (C mol/kg) #Ca_d10	,Calcium (C mol/kg) 
	## to convert in mg/kg (ppm) we use molar atomic mass of each element
	d1$soil_K <- d1$soil_K*10*39.1 
	d1$soil_Ca <- d1$soil_Ca*10*40
	d1$soil_Mg <- d1$soil_Mg*10*24
	
	sel <- c('UniqueID', 'Season', 'Loc', 'Site', 'Rep', 'Tillage', 'cropSystem', 'Fertilizer', 'Variety', 'Density', "Yldokfr_Kgm2", 'Lat', 'Long', 'Date_Planted_Cas', 'Date_Harvested_Cas')
	d2 <- r2[,sel]
	d2 <- carobiner::change_names(d2, sel,
		c("trial_id", "season", "location", "site", "rep", "tillage", "crop", "treatment", "variety","plant_density","yield", "latitude", "longitude", "planting_date", "harvest_date"))

	weeds2 <- r2[, c("Biomass_04Wap_Gm2", "Biomass_08Wap_Gm2", "Biomass_12Wap_Gm2", "Biomass_24Wap_Gm2")]
	# mean weed biomass in kg/ha	
	d2$weeds_biomass <- rowMeans(weeds2) * 10 

	#d2$weed_biomass <- ((r2$dmy_04Wap_Gm2+r2$dmy_08Wap_Gm2+r2$dmy_12Wap_Gm2+r2$dmy_24Wap_Gm2)/2)*1000 # in kg/ha
	
	# fill soil information for second season base on $site.
	# I assume that soil information is the same for the same long and lat position
	
	d2$latitude[d2$site=="Ido"] <- 7.55140 #instead of 7.5517
	d2$longitude[d2$site=="Ido"] <- 3.66990 #instead of 3.6691
	u <- na.omit(unique(d1[, c("rep","variety","treatment","tillage","crop","longitude", "latitude", "soil_SOC", "soil_pH", "soil_P_available", "soil_K", "soil_N", "soil_Ca", "soil_Mg")]))
	d2 <- merge(d2, u,by=c("variety","treatment","tillage","crop","longitude", "latitude","rep"),all.x = TRUE)
	
	# combine d1 and d2
	d <- rbind(d1, d2)
	
	# Add columns
 
	d$dataset_id <- dataset_id
	d$country <- "Nigeria"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	# NPK Apply	75-20-90 means 75 kg/ha N, 20kg/ha of P2O5, 90kg/ha of K2O from #DOI:10.1564/v27_oct_04
	d$N_fertilizer <- ifelse(d$treatment == "NoFert", 0, 75) 
	d$K_fertilizer <- ifelse(d$treatment == "NoFert", 0, 90/1.2051 )
	d$P_fertilizer <- ifelse(d$treatment == "NoFert", 0, 20/2.29)
													 
	# fix crop names 
	d$intercrops <- ifelse(d$crop=="CasMz", "maize", "no crop") 
	d$crop <- "cassava"
	d$yield_part <- "roots"
	
	#fix Long and lat
	d$longitude[d$site=="Makurdi"] <- 7.6736
	d$latitude[d$site=="Makurdi"] <- 12.906337
	d$longitude[d$site=="Otobi"] <- 8.0701088
	d$latitude[d$site=="Otobi"] <- 7.103026
	#data type

	# convert to kg/ha 
	d$yield <- (as.numeric(d$yield))*10000 
	d$plant_density <- as.numeric(d$plant_density)
	#date format
	d$planting_date <- as.character(as.Date(d$planting_date, format = "%m/%d/%Y"))
	d$planting_date[is.na(d$planting_date)] <- "2015"
	d$harvest_date <- as.character(as.Date(d$harvest_date, format = "%m/%d/%Y"))
	d$harvest_date[is.na(d$harvest_date)] <- "2016" 
	d$tillage <- tolower(d$tillage)
	
	carobiner::write_files(dset, d, path=path)	
}

