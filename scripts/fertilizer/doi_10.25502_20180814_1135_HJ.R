# R script for "carob"


carob_script <- function(path) {

"The AFSIS project aimed to establish an Africa Soil Information system. Data was collected in sentinel sites across sub-Saharan Africa using the Land Degradation Surveillance framework and included also multi-location diagnostic trials in selected sentinel sites to determine nutrient limitations and response to improved soil management practices (soil amendments)."

	uri <- "doi:10.25502/20180814/1135/HJ"
	group <- "fertilizer"
	
	ff <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		carobiner::read_metadata(uri, path, group),
		publication=NA,
		data_institute = "IITA",
		carob_contributor="Eduardo Garcia Bendito",
		carob_date="2021-10-06",
		data_type="on-farm experiments",
		project="AFSIS",
		treatment_vars = "N_fertilizer;K_fertilizer;P_fertilizer;Zn_fertilizer;S_fertilizer;lime"
 	)

	f1 <- ff[basename(ff) == "Kiberashi_DT2010_field.csv"]
	r1 <- read.csv(f1)
#	d <- d[complete.cases(d[ , 6:7]),]
	d1 <- data.frame(
		country = "Tanzania",
		location = r1$Site,
		site = r1$Village,
		trial_id = paste0(r1$ID),
		latitude = r1$Flat,
		longitude = r1$Flong,
#	planting_date = format(as.Date(js$result$coverage_start_date), "%Y-%m-%d")
#	harvest_date = format(as.Date(js$result$coverage_end_date), "%Y-%m-%d")
		on_farm = TRUE,
		is_survey = FALSE,
		crop = tolower(r1$TCrop),
		variety = r1$TCVariety,
	## should be crop names
	##intercrops = r1$CS
		previous_crop = r1$PCrop1,
		yield_part = "grain",
		FieldID = r1$FieldID
	)

	p <- carobiner::fix_name(gsub("/", ";", d1$previous_crop), "lower")
	p <- gsub("mangoes", "mango", p)
	p <- gsub("beans", "common bean", p)
	d1$previous_crop <- p

	# 2nd get agronomic data
	f2 <- ff[basename(ff) == "Kiberashi_DT2010_plot.csv"]
	r2 <- read.csv(f2)
	
	d2 <- data.frame(
		yield = r2$TGrainYld*1000,
		residue_yield = r2$Adj.TStoverYld*1000,
		treatment  = r2$TrtDesc,
		rep  = r2$Rep,
		FieldID = r2$FieldID
	)
## RH: this is the treatment, not the _type_ of fertilizer 
##	d2$fertilizer_type <- d1$TrtDesc
	d2$N_fertilizer <- ifelse(r2$TrtDesc == "Control", 0,
	                   ifelse(r2$TrtDesc == "PK", 0, 100))

## RH: this is nice, but the field only stores how many splits there were
##	d2$N_splits <- paste(d1$N_fertilizer*0.25,d1$N_fertilizer*0.375,d1$N_fertilizer*0.375, sep = " | ")

	d2$N_splits <- NA
	d2$N_splits[d2$N_fertilizer > 0] <- 3L
	
	d2$P_fertilizer <- ifelse(r2$TrtDesc == "Control", 0,
	                   ifelse(r2$TrtDesc == "NK", 0, 30))
	d2$K_fertilizer <- ifelse(r2$TrtDesc == "Control", 0,
	                   ifelse(r2$TrtDesc == "NP", 0, 60))
	d2$Zn_fertilizer <- ifelse(r2$TrtDesc == "NPK+MN", 3, 0)
	d2$S_fertilizer <- ifelse(r2$TrtDesc == "NPK+MN", 5, 0)
	d2$OM_used <- TRUE
	d2$OM_type <- "farmyard manure"
	d2$OM_amount <- 1000
	
	d <- merge(d1, d2, by = "FieldID", all.x = TRUE)
	d <- d[!is.na(d$yield), ]
	carobiner::write_files(meta, d, path=path)
}
