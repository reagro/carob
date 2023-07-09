# R script for "carob"

carob_script <- function(path) {

"Description:
    The dataset is meant for developing fertilizer management decision support tool for an effective crop-nutrient management. The dataset is developed on the basis of landscape targeting on-farm trials on crop-nutrient response and crop yield gap assessment across the Africa Rising target districts and other scaling up locations in the Ethiopian highlands.
"

	uri <- "doi:10.7910/DVN/ZXH0R8"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication="doi:10.1017/S1742170519000504",
	   data_citation = "International Crops Research Institute for the Semi-Arid Tropics (ICRISAT), 2021. Landscape Targeted Crop-Fertilizer Response in the Highlands of Ethiopia. https://doi.org/10.7910/DVN/ZXH0R8, Harvard Dataverse, V1",
	   data_institutions = "ICRISAT; ARARI; ILRI",
	   carob_contributor="Siyabusa Mkhulani & Eduardo Garcia Bendito",
	   data_type="on-farm experiment",
		project=NA	   
 	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
    dset$license <- gsub("-DEED.AST", "", carobiner::get_license(js))

	get_rep <- function(d) {
		drep <- rep(NA, nrow(d))
		drep[1] <- 1
		for (r in 2:nrow(d)) { 
			if (d$Treatment[r] == d$Treatment[r-1]) {
			# If the treatment is the same name, it is a replicate...
				drep[r] <- drep[r-1] + 1 
			} else {
			# a new treatment, therefore assign the 1st replicate...
				drep[r] <- 1 
			}
		}
		as.integer(drep)
	}

	get_df <- function(d) {
		# for rep
		d <- d[order(d$Treatment), ]
		
		data.frame(
			country = d$Country,
			adm1= d$Region.state,
			adm3 = d$LGA.District,
			location = ifelse(is.null(d$village.Kebele), d$village, d$village.Kebele),
			planting_date = as.character(as.Date(d$Planting.date)),
			harvest_date = as.character(as.Date(d$Harvest.date)),
			on_farm = TRUE,
			is_survey = FALSE,
			treatment = d$Treatment,
			crop = tolower(d$Crop),
			variety = d$Variety,
			previous_crop = tolower(d$Crop.system),
			rep = get_rep(d),
			soil_type = d$Soil.type,
			#There were two N_splits: Basal (50%) and top dressing (50%)
			N_splits = 2L,
			yield = d$Yield..kg.ha. * 1.13, # Yield data is measured as dry weight 
			residue_yield = d$Stover.yield..kg.ha, # Residue data is measured as dry weight
			fertilizer_type = 
				ifelse(d$Treatment == "0.33NP/farmer practice" | d$Treatment == "NP", "urea; DAP",
				ifelse(d$Treatment == "NPK", "urea; DAP; KNO",
				ifelse(d$Treatment == "NPKS", "urea; DAP; SOP", "urea; DAP; SOP; ZnSO4")))
		)
	}


## process 001_2014-2015_Wheat_ICRISAT-AR_ETH.xlsx
	f <- ff[basename(ff) == "001_2014-2015_Wheat_ICRISAT-AR_ETH.xlsx"] 
 	d <- carobiner::read.excel(f)

	d <- data.frame(d)
	d2 <- get_df(d)
	d2$trial_id = paste0('2014-2015_Wheat_', d$village.Kebele)
	d2$longitude = ifelse(d$village.Kebele == "Goshe Bado", 39.446,
					ifelse(d$village.Kebele == "Tsibet", 39.482,
					ifelse(d$village.Kebele == "Lemo", 37.851,
					ifelse(d$village.Kebele == "Selka", 40.289, 39.682))))
	d2$latitude <- ifelse(d$village.Kebele == "Goshe Bado", 9.740,
					ifelse(d$village.Kebele == "Tsibet", 12.860,
					ifelse(d$village.Kebele == "Lemo", 5.436,
					ifelse(d$village.Kebele == "Selka", 6.857, 9.799))))
	
	# Generate replicate column
	# d2$biomass <- d$Biomass..kg.ha. # Biomass is collected as fresh biomass
	# Type of fertilizer applied is not clear when several are applied
	d2$N_fertilizer <- d$N.fertilizer.amount...19
	d2$P_fertilizer <- ifelse(is.na(d$P.fertilizer.amount), 0, d$P.fertilizer.amount) 
	d2$K_fertilizer <- ifelse(is.na(d$K.fertilizer.amount..K2O.kg.ha..), 0, d$K.fertilizer.amount..K2O.kg.ha..)
	d2$Zn_fertilizer <- ifelse(is.na(d$Zn.fertilizer.amount..Zn.kg.ha..), 0, d$Zn.fertilizer.amount..Zn.kg.ha..)
	d2$S_fertilizer <- ifelse(is.na(d$S.fertilizer.amount..S.kg.ha..), 0, d$S.fertilizer.amount..S.kg.ha..)
	
	
## process 002_2016_Wheat_ ICRISAT-AR_ETH.xlsx
	f <- ff[basename(ff) == "002_2016_Wheat_ ICRISAT-AR_ETH.xlsx"]
	d <- carobiner::read.excel(f)

	d <- data.frame(d)
	d3 <- get_df(d)
	d3$trial_id = paste0("2016_Wheat_", d$village.Kebele)
	d3$longitude = ifelse(d3$location == "Lemo", 37.851,
					 ifelse(d3$location == "Tsibet", 39.482,39.460))
	d3$latitude = ifelse(d3$location == "Lemo", 5.436,
					ifelse(d3$location == "Tsibet", 12.860, 10.826))

	# d3$biomass <- d$Biomass..kg.ha. # Biomass is collected as fresh biomass

	# Type of fertilizer applied is not clear when several are applied
	d3$N_fertilizer <- d$N.fertilizer.amount..kg.ha.
	### not obs d3$observation_date <- d$N.topdressing.date # Date of the application of the 2nd split
	d3$P_fertilizer <- ifelse(is.na(d$P.fertilizer.amount..kg.ha.), 0, d$P.fertilizer.amount..kg.ha.) 
	d3$K_fertilizer <- ifelse(is.na(d$K.fertilizer.amount..kg.ha.), 0, d$K.fertilizer.amount..kg.ha.) 
	d3$Zn_fertilizer <- NA # Not present in the dataframe
	d3$S_fertilizer <- ifelse(is.na(d$S.fertilizer.amount..kg.ha.), 0, d$S.fertilizer.amount..kg.ha.) 

	
## process 003_2017_Sorghum+Tef_ ICRISAT-AR_ETH.xlsx
	f <- ff[basename(ff) == "003_2017_Sorghum+Tef_ ICRISAT-AR_ETH.xlsx"]
	d <- carobiner::read.excel(f)
	d <- data.frame(d)
	d4 <- get_df(d)
	d4$trial_id <- paste0("2017_Sorghum-Tef_", d$village.Kebele)
	d4$longitude <- ifelse(d4$location == "Sirinka", 39.607, 39.684)
	d4$latitude <- ifelse(d4$location == "Sirinka", 11.748, 11.316)

	# d4$biomass <- d$Biomass..kg.ha. # Biomass is collected as fresh biomass
	# Type of fertilizer applied is not clear when several are applied
	d4$N_fertilizer <- d$N.fertilizer.amount..kg.ha.
	d4$P_fertilizer <- ifelse(is.na(d$P.fertilizer.amount..kg.ha.), 0, d$P.fertilizer.amount..kg.ha.) 
	d4$K_fertilizer <- ifelse(is.na(d$K.fertilizer.amount..kg.ha.), 0, d$K.fertilizer.amount..kg.ha.)
	d4$Zn_fertilizer <- ifelse(is.na(d$Zn.fertilizer.amount..kg.ha.), 0, d$Zn.fertilizer.amount..kg.ha.) 
	d4$S_fertilizer <- ifelse(is.na(d$S.fertilizer.amount..kg.ha.), 0, d$S.fertilizer.amount..kg.ha.) 

	
## process 004_2019_Wheat_ ICRISAT-AR_ETH.xlsx
	f <- ff[basename(ff) == "004_2019_Wheat_ ICRISAT-AR_ETH.xlsx"]
	d <- carobiner::read.excel(f)
	d <- data.frame(d)
	d5 <- get_df(d)
	
	d5$trial_id <- paste0("2019_Wheat_", d$village)
	d5$longitude <- ifelse(d5$location == "Goshebado", 39.446,
					ifelse(d5$location == "Lemo", 37.851, 40.215))
	d5$latitude <- ifelse(d5$location == "Goshebado", 9.740,
					ifelse(d5$location == "Lemo", 5.436, 7.068))

	# d5$biomass <- d$Biomass..kg.ha.  # Biomass is collected as fresh biomass
	# Type of fertilizer applied is not clear when several are applied
	d5$N_fertilizer <- d$N.fertilizer.amount..kg.ha.
	###d5$observation_date <- d$N.topdressing.date # Date of the application of the 2nd split
	d5$P_fertilizer <- ifelse(is.na(d$P.fertilizer.amount..kg.ha.), 0, d$P.fertilizer.amount..kg.ha.)
	d5$K_fertilizer <- ifelse(is.na(d$K.fertilizer.amount..kg.ha.), 0, d$K.fertilizer.amount..kg.ha.) 
	d5$Zn_fertilizer <- ifelse(is.na(d$Zn.fertilizer.amount..kg.ha.), 0, d$Zn.fertilizer.amount..kg.ha.) 
	d5$S_fertilizer <- ifelse(is.na(d$S.fertilizer.amount..kg.ha.), 0, d$S.fertilizer.amount..kg.ha.) 
	
## Append the tables together
	d <- rbind(d2, d3, d4, d5)
	
## Filter only relevant variables
## RH: no need, and you were using d2!
#	d <- d2[,c("country", "adm1", "adm3", "location", "trial_id", "longitude", "latitude", "planting_date", "harvest_date", "on_farm", "is_survey", "treatment", "rep", "crop", "variety", "previous_crop", "yield", "residue_yield", "fertilizer_type", "N_fertilizer", "N_splits", "P_fertilizer", "K_fertilizer", "Zn_fertilizer", "S_fertilizer","soil_type")]
## Add dataset ID

	d$dataset_id <- dataset_id
	d$yield_part <- "grain"

	d$crop <- gsub("tef", "teff", d$crop)

	d$previous_crop <- gsub("tef", "teff", d$previous_crop)
	d$previous_crop <- gsub("fieldpea", "pea", d$previous_crop)
	d$previous_crop <- gsub(",", ";", d$previous_crop)
	d$previous_crop <- gsub("fababean", "faba bean", d$previous_crop)
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

