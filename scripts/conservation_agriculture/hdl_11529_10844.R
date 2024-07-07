# R script for "carob"


carob_script <- function(path) {

"The present data is from a long-term trial set at Msekera Research Station in Zambia to monitor and evaluate the the longer term effects of conservation agriculture practices on soil quality, soil water dynamics, weeds, pests/diseases and crop yield. The treatments set to investigate this are: T1: Control plot 1 (CPM1); traditional farmers practice mouldboard plough on the flat, maize as a sole crop, no residue retention, stubble incorporated into the row for the following season. T2: Control plot 2 (CPM2); ridge and furrow system dug by hand, maize as a sole crop, no residue retention, stubble incorporated into the row for the following season T3: Basins (BAM), residue retention on the surface, maize as a sole crop T4: Dibble stick (DISM), residue retention on the surface, maize as a sole crop T5: Direct seeder (DSM), residue retention on the surface, maize as a sole crop T6: Direct seeding maize/cowpea intercropping (DS-M/C), 90cm rows, residue retention on the surface T7: Direct seeding cowpea (Cowpea-maize rotation) (DS-MC), residue retention on the surface T8:Direct seeding maize (Maize-cowpea rotation) (DS-CM), residue retention on the surface T9:Direct seeding soya (Soybean-maize r otation) (DS-MS), residue retention on the surface T10: Direct seeding maize (Maize-soybean rotation) (DS-SM), residue retention on the surface
The present data set is from 2012 to 2016. (2016)"

	uri <- "hdl:11529/10844"
	group <- "conservation_agriculture"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=2, minor=1),
		data_institute = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "on-farm experiment",
		carob_contributor = "Blessing Dzuda",
		carob_date = "2024-05-03"
	)

# read data 

	f <- ff[basename(ff) == "Msekera 2012.2016.xlsx"]
	r1 <- carobiner::read.excel(f, sheet ="All maize yield Msekera")
	r2 <- carobiner::read.excel(f, sheet = "All legume yield Msekera")


### MAIZE TRIAL
	
	d1 <- data.frame(
		trial_id = "1",
	    treatment=r1$Label,
	    crop=r1$Crop,
	    rep=as.integer(r1$Replicate),
	    dmy_total=r1$Biomass,
	    yield=r1$Grain,
	    planting_date=r1$Year
	)

	
### LEGUME TRIAL

	d2 <- data.frame(
		trial_id = "2",
		treatment=r2$Label,
		crop=r2$Crop,
		rep=as.integer(r2$Rep),
		dmy_total=r2$`Biomass yield (kg/ha)`,
		yield=r2$`Grain/cotton yield (kg/ha)`,
		planting_date=r2$Year
	)
	d2$crop <- gsub("SOYABEAN|SOYABEANS|SOYBEAN", "soybean", d2$crop)
	
	d <- rbind(d1, d2)

	d$crop <- tolower(d$crop)

	d$on_farm <- TRUE
	d$is_survey <- FALSE 
	d$irrigated <- FALSE
	d$yield_part <- "grain" 
	d$planting_date <- as.character(d$planting_date)


	d$country <- "Zambia" 
	d$location <- "Msekera Research Station"
	d$adm1 <- "Eastern Province"
	d$adm2 <- "Chipata"

	d$elevation <- 1018
	d$longitude <- 32.5585
	d$latitude <- -13.645

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

	carobiner::write_files(path, meta, d)
}

