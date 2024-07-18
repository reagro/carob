# R script for "carob"

## ISSUES
# data had multiple columns for previous crops dating back to 5 seasons


carob_script <- function(path) {

"
This study contains yield data of the two crops from tow representative sections of a field on plots 10m by 10m. Data taken on each of the plots included agronomic practices undertaken including plant spacing, pest and disease control, organic/inorganic fertilizer applications and field history according to a protocol implemented in AfSIS. Each field was geo-referenced
"

## Identifiers
	uri <- "doi:10.7910/DVN/X6QS6M"
	group <- "survey"

## Download data 
	ff  <- carobiner::get_data(uri, path, group)

## metadata 
	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=2, minor=0),
		data_institute = "CIAT;SARI",
		publication = NA,
		project = NA,
		data_type = "survey",
		treatment_vars = "none", 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2024-07-18"
	)
	
## read data 

	f <- ff[basename(ff) == "001_agronomic-surveybabatiyr2013.csv"]
	r <- read.csv(f)
	
	
	d <- data.frame(
	  country="Tanzania",
	  location=r$Village,
	  adm1=r$Sub_Village,
		crop=tolower(r$TCrop), 
		variety_type=r$VarietyName,
		weeding_done=TRUE,
		weeding_times=r$WeedingFreq,
		weeding_implement=r$WeedMethod,
		weed_severity=as.character(r$WeedRating),
		weed_species=tolower(r$MainWeed),
		intercrops=tolower(r$CropsInter)
			)
	
	d$trial_id <- as.character(as.integer(as.factor(r$Code)))
	
	d$herbicide_use <- ifelse(r$Herbicides=="Yes",TRUE,FALSE)
	d$OM_used <- ifelse(r$ManType=="FYM",TRUE,FALSE)
	d$OM_type <- ifelse(r$ManType=="FYM","farmyard manure","none")
	d$OM_amount <- r$ManQty
	d$previous_crop <- tolower(r$S1crops) 
	d$e_rot <- r$PercRot
	d$plant_density <- r$Density
	d$residue_yield <- r$StoverYld
	d$yield <- r$GrainYld * 1000
	d$landscape_position <- r$Position
	
## about the data (TRUE/FALSE)
	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$irrigated <- FALSE
	d$yield_part <- "grain"
	
	d$previous_crop <- gsub(", |,",";",d$previous_crop)
	d$previous_crop <- gsub("bean|beans","common bean",d$previous_crop)
	d$previous_crop <- gsub("mize|mai ","maize",d$previous_crop)
	d$previous_crop <- gsub("irish potato","potato",d$previous_crop)
	d$previous_crop <- gsub("0",NA,d$previous_crop)
	d$previous_crop <- gsub("pigeonpea","pigeon pea",d$previous_crop)
	
	d$intercrops <- gsub("beans","common bean",d$intercrops)
	d$intercrops <- gsub("pigeonpea|peigeon pea","pigeon pea",d$intercrops)
	d$intercrops <- gsub("irish potato","potato",d$intercrops)
	d$intercrops <- gsub(",|, |and",";",d$intercrops)
	d$intercrops <- gsub("nil",NA,d$intercrops)
	d$intercrops <- gsub("pigeonpea","pigeon pea",d$intercrops)
	
	geodata <-data.frame(adm1 = c("Bashnet", "Long", "Endaw", "Sabilo", "Dactara B", 
	                                  "Haysam B", "Shamna", "Getalongo", "Gidmu B", "Bermi B", 
	                                  "Munmunang", "Bermi A", "Maganjwa", "Dabil", "Maganjwa B", 
	                                  "Maganjwa A", "Backchan", "Hayeda", "Semark B", "Magwanjwa B", 
	                                  "Loto A", "Dactara A", "Magwanjwa", "Lomuhong Pri School", 
	                                  "Daktara B"), longitude = c(35.4272, -51.1757, 35.468, 35.4766, 
	                                                        35.4735, 35.4735, 35.4735, 35.4735, 35.4735, 35.5182, 35.4735, 35.5182, 35.4754, 35.4921, 
	                                                        35.4754, 35.4754, 35.4735, 34.99, 35.4735, 35.4735, 35.4933, 35.4735, 35.4735, 35.4735, 
	                                                        35.4735), latitude = c(-4.2298, -23.282, -4.2061, -4.3458, -4.2402, -4.2402, 
	                                                                               -4.2402, -4.2402, -4.2402, -4.2332, -4.2402, -4.2332, -4.2735, -4.2448, -4.2735, 
	                                                                     -4.2735, -4.2402, -4.2683, -4.2402, -4.2402, -4.2454, -4.2402, -4.2402, -4.2402, -4.2402)) 
	d <- merge(d,geodata, by="adm1", all.x = TRUE)
	
	carobiner::write_files(path, meta, d)
}


