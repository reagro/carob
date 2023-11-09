# R script for "carob"

## ISSUES
# ....

## should also be written to fertilizer

## review by Cedric Ngakou

carob_script <- function(path) {

"Description:
Farmer participatory on-farm trials with CA technologies comparing with farmers’ practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory validation trials were conducted comparing to existing practices and to find out suitable and more profitable crop production practices, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers.
"

	uri <- "hdl:11529/10548007"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "conservation_agriculture"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id, 
		group=group, 
		project=NA, 
		uri=uri, 
		data_citation='Gathala, Mahesh K.; Tiwari, Thakur P.; Islam, Saiful; Khan, A.S.M.; Anwar, Mazharul; Hossain, Illias; Siddique, Nur-E-A; Hossain, Shakhawat; Rahman, Mohammad Atiqur, 2018. 6.1-Rabi (winter) crops-all nodes-Validation trials-Rajshahi-Bangladesh.  https://hdl.handle.net/11529/10548007, CIMMYT Research Data & Software Repository Network, V2', 
		publication= NA, 
		data_institutions = "CIMMYT", 
		data_type="on-farm experiment", 
		carob_contributor="Mitchelle Njukuya", 
		carob_date="2023-08-17", 
		revised_by="Cedric Ngakou, Robert Hijmans",
		revision_date="2023-11-03"
	)

## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)

	
	ff <- ff[grep("Rajshahi", basename(ff))]
	## process file(s)
	## process Rabi maize 2015-16-DS&BP-all nodes Rajshahi files


	proc <- function(f) {
		r1 <- carobiner::read.excel.hdr(f, sheet ="2 - Site information", skip=5, hdr=3)
		d1 <- data.frame(
				season=r1$Season, location=r1$Node, 
				trial_id=r1$Site.No.Unique.farmer.ID,
				treatment=r1$Tmnt.Short.abbreviation.as.mentioned.in.protocol,
				soil_type=r1$Soil.texture.sand.silt.clay.etc)
		
		r2 <- carobiner::read.excel.hdr(f, sheet ="4- Stand counts & Phenology", skip=4, hdr=2)
		d2 <- data.frame(
				treatment=r2$Tmnt, trial_id=r2$Site.No, 
				variety=r2$Variety,
				row_spacing=r2$Row.spacing.cm,
				crop=tolower(r2$Types.of.Trial),
				planting_date=as.character(r2$Date.of.seeding.dd.mm.yy),
				harvest_date=as.character(r2$Datw.of.harvest.dd.mm.yy))
		
		r3 <- carobiner::read.excel.hdr(f, sheet ="6 - Fertilizer amounts ", skip=4, hdr=2)

		d3 <- data.frame(
				treatment=r3$Tmnt, trial_id=r3$Site.No,
				N_fertilizer=r3$N.kg.ha, 
				P_fertilizer=r3$P2O5.kg.ha / 2.29, #!
				gypsum=r3$Gypsum.Kg.ha,
				Zn_fertilizer=r3$ZnSO4.kg.ha) 

		if (!is.null(r3$K2O.kg.ha)) {
			d3$K_fertilizer <- r3$K2O.kg.ha / 1.2051
		} else { # assuming this is also K2O
			d3$K_fertilizer=r3$K.kg.ha / 1.2051
		}
	
		d3$fertilizer_type <- apply(r3[, grep("Application_Product.used", names(r3))], 1, 
									\(i) paste(unique(i), collapse="; "))



		r4 <- carobiner::read.excel.hdr(f, sheet ="14 - Grain Harvest ", skip=4, hdr=2)	
		colnames(r4) <- gsub("Calculation_", "", colnames(r4))
		d4 <- data.frame(
				treatment=r4$Tmnt, trial_id=r4$Site.No, 
				yield=r4$Grain.yield.t.ha * 1000,
				residue_yield=r4$Straw.yield.t.ha * 1000,
				biomass_total=r4$Biomass.t.ha * 1000)

		## merge all 
		dd <- merge(d1, d2, by=c("treatment", "trial_id"), all.x=TRUE)
		dd <- merge(dd, d3, by=c("treatment", "trial_id"), all.x=TRUE)
		dd <- merge(dd, d4, by=c("treatment", "trial_id"), all.x=TRUE)
		#dd <- unique(dd) if you need this, something is wrong
		dd
	}

	d <- lapply(ff, proc)
	d <- do.call(rbind, d)

	#add columns
	d$dataset_id <- dataset_id
	d$country <- "Bangladesh"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$inoculated <- FALSE
	d$yield_part <- "grain" 
	d$trial_id <- as.character(d$trial_id)
	d$fertilizer_type <- gsub("MOP", "KCl", d$fertilizer_type)
	d$fertilizer_type <- gsub("Urea", "urea", d$fertilizer_type)
	d$fertilizer_type <- gsub("Gypsum", "gypsum", d$fertilizer_type)
	
	geo= data.frame(location=c("Dharmapur", "Nabinagar", "Premtoli" , "Baduria", "Dharampur", "Laxmipur" ), 
						 latitude=c(23.3108087, 23.8846959, 25, 24.34255, 24.879025, 22.9445365), 
						 longitude=c(91.2876786, 90.9699944, 89, 88.72014, 89.3533518, 90.8276345))
	
	d <- merge(d, geo, by="location", all.x = TRUE)  # do not use "T"
	
##data type

	# all scripts must end like this
	carobiner::write_files(dset, d, path=path)	
}


