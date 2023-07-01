# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:
  Secondary and micronutrients are important in enhancing crop productivity; yet, they are hardly studied in sub-Sahara Africa. In this region, the main focus has been on macronutrients but there is emerging though scattered evidence of crop productivity limitations by the secondary and micronutrients. Elsewhere, widespread deficiencies of these nutrients are associated with stagnation of yields. In total, 530 rows of yield data were extracted from the 40 papers of which 49.4% were on sulfur (S) response, 23.0% on Zn, 7.4% on Cu, 3.0% on Mo, 4.5% on Fe, 1.1% on boron (B), and 11.5% involved two or more, i.e., S and micronutrient combinations. Here, we undertake a meta-analysis using 40 articles reporting crop response to secondary and micronutrients to (1) determine the productivity increase of crops and nutrient use efficiency associated with these nutrients, and (2) provide synthesis of responses to secondary nutrients and micronutrients in sub-Sahara Africa. This study used 757 yield data rows (530 from publications and 227 from Africa Soil Information Service) from field trials carried out in SSA between 1969 and 2013 in 14 countries. Data from publications constituted response to S (49.4%), Zn (23.0%), S and micronutrient combinations (11.5%), and <10% each for Cu, Mo, Fe, and B. Data from Africa Soil Information Service were all for S and micronutrient combinations. Of the two sources, most yield data are for maize (73.6%), followed by sorghum (6.7%) and wheat (6.1%) while rice, cowpea, faba bean, tef, and soybean each accounted for less than 5%. The major points are the following: (1) application of S and micronutrients increased maize yield by 0.84 t ha−1 (i.e., 25%) over macronutrient only treatment and achieved agronomic efficiencies (kilograms of grain increase per kilogram of micronutrient added) between 38 and 432 and (2) response ratios were >1 for S and all micronutrients, i.e., the probability of response ratio exceeding 1 was 0.77 for S and 0.83 for Zn, 0.95 for Cu, and 0.92 for Fe, and indicates positive crop response for a majority of farmers. We conclude that S and micronutrients are holding back crop productivity especially on soils where response to macronutrients is low and that more research is needed to unravel conditions under which application of S and micronutrients may pose financial risks.

"

	uri <- "doi:10.7910/DVN/8AJQJJ"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		data_citation="Kihara, Job; Sileshi, Gudeta W.; Nziguheba, Generose; Kinyua, Michael; Zingore, Shamie; Sommer, Rolf, 2017. Replication Data for: Application of secondary nutrients and micronutrients increases crop yields in sub-Saharan Africa. https://doi.org/10.7910/DVN/8AJQJJ, Harvard Dataverse, V1",
		uri=uri,
		publication= "doi:10.1007/s13593-017-0431-0",
		data_institutions = "CIAT",
		carob_contributor="Eduardo Garcia Bendito",
		data_type="compilation",
		has_weather=FALSE
 	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=2)
	dset$license <- carobiner::get_license(js)


	f <- ff[basename(ff) == "02. Micronutrients_SSA_Publication data.xlsx"]

	r <- readxl::read_excel(f) |> as.data.frame()
	
	# Wide to long, since yield for different treatments is spread wide
	rr <- reshape(r, direction='long', 
	              varying=c('Mn_yld', 'Control_Yld', 'Absolute_Ctrl'), 
	              timevar='treatment_type',
	              times=c('all_nutrient', 'macro_nutrient', 'zero_nutrient'),
	              v.names=c('yield'),
	              idvar='observation')

# Removing this observations, since they are not properly recorded from the original source (doi:10.1080/00380768.2011.593482).				  
	rr <- rr[rr$`DATA SOURCE` != "Haileselassie et al. 2011",] 
	
## process file(s)
	
#### about the data #####
## (TRUE/FALSE)

#	d <- data.frame("irrigated" = as.logical(ifelse(rr$`Watering Regime` == 'Irrigated', TRUE, FALSE)))
	d <- data.frame("irrigated" = rr$`Watering Regime` == 'Irrigated')
	d$dataset_id <- dataset_id
	# d$on_farm <- 
	d$is_survey <- FALSE
## the treatment code	
	d$treatment <- paste0(ifelse(is.na(rr$N), "N0", paste0("N", as.integer(rr$N))),
	               ifelse(is.na(rr$P), "P0", paste0("P", as.integer(rr$P))),
	               ifelse(is.na(rr$K), "K0", paste0("K", as.integer(rr$K))),
	               ifelse(is.na(rr$`MicroN amount`), "mic0", paste0("mic", as.integer(rr$`MicroN amount`))))
	d$rep <- rr$observation # Review this
	d$trial_id <- paste(rr$`DATA SOURCE`, rr$treatment_type, sep = " - ")
	

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- ifelse(rr$COUNTRY %in% c("Cote dIvoire", "Cote d'Ivoire"), "Côte d'Ivoire", rr$COUNTRY)
	d$site <- as.character(rr$SITE)
## each site must have corresponding longitude and latitude
	rr$X[rr$X == "NA"] <- NA
	rr$Y[rr$Y == "NA"] <- NA
	rr$Y[grep(" and ", rr$Y)] <- NA
	d$longitude <- as.numeric(rr$X)
	d$latitude <- as.numeric(rr$Y)
	xy <- c("longitude", "latitude")
	i <- apply(is.na(d[, xy]), 1, any)
	
	crds = data.frame(site = c("Sidindi", "Thuchila", "Calabar", "Manjawira",  "Amoutchou", "Sarakawa"), 
					lon = c(34.38, 35.57, 8.33, 34.85, 1.08, 1.01), 
					lat = c(0.15, -15.86, 4.97, -14.99, 7.46, 9.63))

	m <- na.omit(cbind(1:nrow(d), match(d$site, crds$site)))
	d$longitude[m[,1]] <- crds[m[,2], 2]
	d$latitude[m[,1]] <- crds[m[,2], 3]

##### Crop #####
## normalize variety names
	d$crop <- tolower(rr$CROPTYPE)
	d$variety_type <- tolower(rr$VARIETY)
	d$variety_type[d$variety_type == "indigenous"] <- "landrace"

##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- as.character(format(as.Date(substr(rr$YEAR, start = 1, stop = 4), "%Y"), "%Y"))
	d$harvest_date  <- as.character(format(as.Date(substr(rr$YEAR, (nchar(rr$YEAR)+1) - 4, nchar(rr$YEAR)), "%Y"), "%Y"))

##### Fertilizers #####
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- as.numeric(rr$P)
   d$K_fertilizer <- as.numeric(rr$K)
   d$N_fertilizer <- as.numeric(rr$N)
   d$Zn_fertilizer <- as.numeric(ifelse(grepl("Zn", rr$Micronutrient, fixed = TRUE), rr$`MicroN amount`, 0))
   d$S_fertilizer <- as.numeric(ifelse(grepl("S", rr$Micronutrient, fixed = TRUE), rr$`MicroN amount`, 0))
   d$OM_used <- ifelse(rr$`Organic resource` == "Yes", TRUE,
                       ifelse(rr$`Organic resource` == "None", FALSE, NA))
   
## normalize names 
   d$fertilizer_type <- as.character(rr$P_Source)
   d[!is.na(d$fertilizer_type) & d$fertilizer_type == "Compound D", "fertilizer_type"] <- "D-compound"
   d[!is.na(d$fertilizer_type) & d$fertilizer_type == "23:21:0+4S", "fertilizer_type"] <- "NPS"
   d[!is.na(d$fertilizer_type) & d$fertilizer_type == "PKS Blend", "fertilizer_type"] <- "PKS"
   d[!is.na(d$fertilizer_type) & d$fertilizer_type == "composite", "fertilizer_type"] <- "unknown"
   d[!is.na(d$fertilizer_type) & d$fertilizer_type == "composite", "fertilizer_type"] <- "unknown"
   d$inoculated <- FALSE
   

##### in general, add comments to your script if computations are
##### based in information gleaned from metadata, publication, 
##### or not immediately obvious for other reasons

##### Yield #####

	d$yield <- as.numeric(rr$yield)*1000 # Yield in ton/ha

#### SOIL INFORMATION ######
	d$soil_type <- rr$`WRB Soiltype`
	d$soil_pH <- as.numeric(rr$`SOIL pH`)
	d$soil_SOC <- as.numeric(rr$SOC)
	d$soil_sand <- as.numeric(rr$Sand)
	d$soil_clay <- as.numeric(rr$Clay)
	# Seems to be in mg/kg, but the range of values in carob do not fit the observations here
	d$soil_P_available <- rr$`Avail P` 
	
	#### OTHER ######
	 # coerced NAs due to character types. Could be suppressed.
	d$uncertainty <- suppressWarnings(as.numeric(rr$Error))
	d$uncertainty_type <- as.character(rr$`Error Type`)
	d$rain <- as.integer(rr$Rainfall)
	
	  
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

