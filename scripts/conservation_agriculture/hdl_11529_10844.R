# R script for "carob"

## ISSUES
# 1. some Legume yield values has blank spaces which R recognised as NA's 
# 2. the dataset variable: exp_treatments, is failing to be recognised by the
# write files function despite updating carobiner


carob_script <- function(path) {

"
	
The present data is from a long-term trial set at Msekera Research Station in 
Zambia to monitor and evaluate the the longer term effects of conservation agriculture
practices on soil quality, soil water dynamics, weeds, pests/diseases and crop yield.
The treatments set to investigate this are: T1: Control plot 1 (CPM1); traditional
farmers practice mouldboard plough on the flat, maize as a sole crop, no residue 
retention, stubble incorporated into the row for the following season. T2: Control
plot 2 (CPM2); ridge and furrow system dug by hand, maize as a sole crop, no residue
retention, stubble incorporated into the row for the following season T3: Basins (BAM),
residue retention on the surface, maize as a sole crop T4: Dibble stick (DISM), residue
retention on the surface, maize as a sole crop T5: Direct seeder (DSM), residue retention
on the surface, maize as a sole crop T6: Direct seeding maize/cowpea intercropping 
(DS-M/C), 90cm rows, residue retention on the surface T7: Direct seeding cowpea 
(Cowpea-maize rotation) (DS-MC), residue retention on the surface T8:Direct seeding
maize (Maize-cowpea rotation) (DS-CM), residue retention on the surface T9:Direct 
seeding soya (Soybean-maize r otation) (DS-MS), residue retention on the surface T10:
Direct seeding maize (Maize-soybean rotation) (DS-SM), residue retention on the surface
The present data set is from 2012 to 2016. (2016)
"

#### Identifiers
	uri <- "hdl:11529/10844"
	group <- "conservation_agriculture"

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)

##### dataset level metadata 
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=2, minor=1),
		data_institutions = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "on-farm experiment",
		exp_treatment = "T1: Control plot 1 (CPM1); traditional
farmers practice mouldboard plough on the flat, maize as a sole crop, no residue 
retention, stubble incorporated into the row for the following season. T2: Control
plot 2 (CPM2); ridge and furrow system dug by hand, maize as a sole crop, no residue
retention, stubble incorporated into the row for the following season T3: Basins (BAM),
residue retention on the surface, maize as a sole crop T4: Dibble stick (DISM), residue
retention on the surface, maize as a sole crop T5: Direct seeder (DSM), residue retention
on the surface, maize as a sole crop T6: Direct seeding maize/cowpea intercropping 
(DS-M/C), 90cm rows, residue retention on the surface T7: Direct seeding cowpea 
(Cowpea-maize rotation) (DS-MC), residue retention on the surface T8:Direct seeding
maize (Maize-cowpea rotation) (DS-CM), residue retention on the surface T9:Direct 
seeding soya (Soybean-maize r otation) (DS-MS), residue retention on the surface T10:
Direct seeding maize (Maize-soybean rotation) (DS-SM), residue retention on the surface
The present data set is from 2012 to 2016. (2016)", 
		carob_contributor = "Blessing Dzuda",
		carob_date = "2024-05-03"
	)
	
##### PROCESS data records

# read data 

	f <- ff[basename(ff) == "Msekera 2012.2016.xlsx"]
	r <- carobiner::read.excel(f, sheet ="All maize yield Msekera")
	r2 <- carobiner::read.excel(f, sheet = "All legume yield Msekera")
	
############################### MAIZE TRIAL ####################################

## process file(s)
	d0 <- data.frame(
		    treatment=r$Treatment,
		    crop=r$Crop,
		    rep=r$Replicate,
		    dmy_total=r$Biomass,
		    yield=r$Grain,
		    planting_date=r$Year
	)

	d0$on_farm <- TRUE
	d0$is_survey <- FALSE 
	d0$irrigated <- FALSE
	d0$country <- "Zambia" 
	d0$site <- "Msekera Research Station"
	d0$adm1 <- "Eastern Province"
	d0$adm2 <- "Chipata"
	d0$elevation <- "1018"
	d0$longitude <- "32.5585"
	d0$latitude <- "-13.645"
  d0$yield_part <- "grain" 
  d0$trial_id <- paste0(d0$site,"_",d0$rep)
  d0$crop <- gsub("Maize|MAIZE","maize",d0$crop)
  d0$treatment <- as.character(d0$treatment)
  d0$rep <- as.integer(d0$rep)
  d0$elevation <- as.numeric(d0$elevation)
	d0$longitude <- as.numeric(d0$longitude)
	d0$latitude <- as.numeric(d0$latitude)
	d0$planting_date <- as.character(d0$planting_date)
	
############################## LEGUME TRIALS ###################################
	d1 <- data.frame(
	  treatment=r2$Tmnt.,
	  crop=r2$Crop,
	  rep=r2$Rep,
	  dmy_total=r2$`Biomass yield (kg/ha)`,
	  yield=r2$`Grain/cotton yield (kg/ha)`,
	  planting_date=r2$Year
	)
	
	d1$on_farm <- TRUE
	d1$is_survey <- FALSE 
	d1$irrigated <- FALSE
	d1$country <- "Zambia" 
	d1$site <- "Msekera Research Station"
	d1$adm1 <- "Eastern Province"
	d1$adm2 <- "Chipata"
	d1$elevation <- "1018"
	d1$longitude <- "32.5585"
	d1$latitude <- "-13.645"
	d1$yield_part <- "grain" 
	d1$trial_id <- paste0(d1$site,"_",d1$rep)
	d1$crop <- gsub("SOYABEAN|SOYABEANS|SOYBEAN","soybean",d1$crop)
	d1$crop <- gsub("COWPEA","cowpea",d1$crop)
	d1$treatment <- as.character(d1$treatment)
	d1$rep <- as.integer(d1$rep)
	d1$elevation <- as.numeric(d1$elevation)
	d1$longitude <- as.numeric(d1$longitude)
	d1$latitude <- as.numeric(d1$latitude)
	d1$planting_date <- as.character(d1$planting_date)
	
	d <- rbind(d0,d1)
	d$latitude <- as.numeric(d$latitude)

	carobiner::write_files(path, dset, d)
}