# R script for "carob"

## ISSUES
# These data, or at least the control treatments, seem unreliable as the values of the controls are sometimes too high, and constant across treatments where they should not be

# to do: "seasons" need to be expressed as months


carob_script <- function(path) {

	"
	Description:
		Kihara, Job; Tibebe, Degefie; Gurmensa, Biyensa; Lulseged, Desta, 2017, Towards understanding fertilizer responses in Ethiopia, https://doi.org/10.7910/DVN/RKUMXB, Harvard Dataverse, V2, UNF:6:GyS4YNBnn5DjzEC7fY80yw== [fileUNF]

	# This is a comprehensive dataset specifically on crop response to fertilizers and is obtained from published journal articles, thesis and proceedings spanning at least 5 decades. It represents all the agriculturally productive regions of Ethiopia. The data contains information on region, crop type and soil type under which experiments were conducted, as well as application rates of nutrients (N, P, K, and other nutrients) as well as yields of the control and fertilized treatment on which crop response ratios are derived.


	Towards understanding fertilizer responses in Ethiopia

	These is a data extracted from 98 other sources
	It has an odd database design with control treatments in separate columns; as in FAOs FERTIBASE.
	This is practical to compute fertilizer use efficiency, but it is not good for data storage/distribution.

	The control is where fertilizer application of a particular element of interest is zero. The absolute control is where there is no fertilizer application. 

	Some of the sources included
	Amare Aleminew and Adane Legas. 2015. Grain quality and yield response of malt barley varieties to nitrogen fertilizer on brown soils of Amhara region Ethiopia. World Journal of Agricultural Sciences, 11 (3): 135–143.

	Minale Liben, Alemayehu Assefa and Tilahun Tadesse. 2011. Grain yield and malting quality of barley inrelation to nitrogen application at mid- andhigh altitude in Northwest Ethiopia. Journal of Science and Development 1 (1) 

	K. Habtegebrial & B. R. Singh (2009) Response of Wheat Cultivars to Nitrogen and Sulfur for Crop Yield, Nitrogen Use Efficiency, and Protein Quality in the Semiarid Region, Journal of Plant Nutrition, 32:10, 1768-1787, DOI: 10.1080/01904160903152616
"

	uri <- "doi:10.7910/DVN/RKUMXB"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group = group,
	   uri=uri,
	   publication=NA,
	   carob_contributor="Camila Bonilla",
	   experiment_type="fertilizer",
	   has_weather=FALSE,
	   has_management=FALSE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	f <- ff[basename(ff) == "02. ET_data_June2017.csv"]
	## read the json for version, license, terms of use  
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=2)
	# No License specified in metadata. Only Terms of use available. It is registered as "CC BY 4.0"
	# dset$license <- carobiner::get_license(js)
	dset$license <- "CC BY 4.0"

## Select and fix column names
	# ft <- c("DATASOURCE", "reference", "SITE", "location", "ADMIN_REGION", "adm1", "CODE", "trial_id", "CodeSE", "drop", "X", "longitude", "Y", "latitude", "CoordType", "drop", "CROPTYPE", "crop", "VARIETY", "variety", "VARIETYTYPE", "variety_type", "TRIALTYPE", "trial_type", "SOILTYPE", "soil_type", "Sand", "soil_sand", "Clay", "soil_clay", "SOC", "soil_SOC", "pH", "soil_pH", "Avail_P", "soil_P_available", "CroppingSystem", "crop_system", "Organicresource", "OM_used", "Inoculation", "inoculated", "OrgR_type", "OM_type", "OrgR_Amount", "OM_applied", "OrganicN", "OM_N", "OrganicK", "OM_K", "OrganicP", "OM_P", "Prev_crop", "previous_crop", "YEAR", "year", "Season", "season", "Response", "response", "N", "N", "N_Timing", "drop", "N_splits", "N_splits", "P", "P", "P_Appl", "drop", "P_Source", "fertilizer_type_1", "K", "K", "Other_Nutrient", "Other_Nutrient", "NutrientSource", "fertilizer_type_2", "Nutrientamount", "Nutrientamount", "AvailableSoilNutrient_OtherthanNPK", "drop", "TrtDesc", "drop", "Treatment_yld", "yield", "Control_Yld", "Control_Yld", "Absolute_Ctrl_Yld", "Absolute_Ctrl_Yld", "Error", "uncertainty", "ErrorType", "uncertainty_type", "Replications", "drop", "Treatments", "drop", "SDEV", "drop", "Application_ForOtherNutrients", "drop", "Rainfall", "drop", "WateringRegime", "irrigated", "Tillage", "tillage", "COMMENTS", "comments", "RR", "drop")
	## EGB: There is an error with the XY coordinates. They are flipped...
	ft <- c("DATASOURCE", "reference", "SITE", "location", "ADMIN_REGION", "adm1", "CODE", "trial_id", "CodeSE", "drop", "Y", "longitude", "X", "latitude", "CoordType", "drop", "CROPTYPE", "crop", "VARIETY", "variety", "VARIETYTYPE", "variety_type", "TRIALTYPE", "trial_type", "SOILTYPE", "soil_type", "Sand", "soil_sand", "Clay", "soil_clay", "SOC", "soil_SOC", "pH", "soil_pH", "Avail_P", "soil_P_available", "CroppingSystem", "crop_system", "Organicresource", "OM_used", "Inoculation", "inoculated", "OrgR_type", "OM_type", "OrgR_Amount", "OM_applied", "OrganicN", "OM_N", "OrganicK", "OM_K", "OrganicP", "OM_P", "Prev_crop", "previous_crop", "YEAR", "year", "Season", "season", "Response", "response", "N", "N", "N_Timing", "drop", "N_splits", "N_splits", "P", "P", "P_Appl", "drop", "P_Source", "fertilizer_type_1", "K", "K", "Other_Nutrient", "Other_Nutrient", "NutrientSource", "fertilizer_type_2", "Nutrientamount", "Nutrientamount", "AvailableSoilNutrient_OtherthanNPK", "drop", "TrtDesc", "drop", "Treatment_yld", "yield", "Control_Yld", "Control_Yld", "Absolute_Ctrl_Yld", "Absolute_Ctrl_Yld", "Error", "uncertainty", "ErrorType", "uncertainty_type", "Replications", "drop", "Treatments", "drop", "SDEV", "drop", "Application_ForOtherNutrients", "drop", "Rainfall", "drop", "WateringRegime", "irrigated", "Tillage", "tillage", "COMMENTS", "comments", "RR", "drop")


	ft <- matrix(ft, ncol=2, byrow=TRUE)

	d <- read.csv(f, encoding="latin1") 
	d <- carobiner::change_names(d, ft[,1], ft[,2])
	d <- d[, colnames(d) != "drop"]

	d <- data.frame(lapply(d, function(i) if (is.character(i)) trimws(i) else i))
	
# remove trailing empty rows
	d <- d[!is.na(d$yield), ]

## add some columns
	d$country <- "Ethiopia"
	d$dataset_id <- dataset_id
	
	d$on_farm <- FALSE	
	i <- d$trial_type == "Farmer managed"
	d$on_farm[i] <- TRUE
	
	d$is_survey <- FALSE

	## NA to zero for some values
	d$N[is.na(d$N)] <- 0
	d$P[is.na(d$P)] <- 0	
	d$K[is.na(d$K)] <- "0"
	d$K <- suppressWarnings(as.numeric(d$K))

	d$Other_Nutrient[is.na(d$Other_Nutrient)] <- 0

	## Add Zn and S columns and extract from "Other_Nutrient"
	d$Zn <- 0
	d$S  <- 0
	d$Zn[d$Other_Nutrient == "Zn"] <- d$Nutrientamount[d$Other_Nutrient == "Zn"]
	d$S[d$Other_Nutrient == "S"] <- d$Nutrientamount[d$Other_Nutrient == "S"]
	d$Other_Nutrient <- NULL
	d$Nutrientamount <- NULL

	## extract data from control treatments
	dd <- d
	for (e in unique(d$response)) {
		dd[dd$response==e, e] <- 0
	}
	dd$yield <- dd$Control_Yld
	dd$Absolute_Ctrl_Yld <- 0
	ud <- unique(dd)
	ud <- ud[!is.na(ud$yield), ]

	ddd <- d
	ddd$N <- ddd$P <- ddd$K <- ddd$S <- ddd$Zn <- 0
	ddd$yield <- ddd$Absolute_Ctrl_Yld
	ddd$Control_Yld <- 0
	udd <- unique(ddd)
	udd <- udd[!is.na(udd$yield), ]

	d <- rbind(d, ud, udd)

	d$Control_Yld <- NULL
	d$Absolute_Ctrl_Yld <- NULL

	## ton to kg per ha
	d$yield <- d$yield * 1000
	d$OM_applied <- d$OM_applied * 1000

	## irrigated to true/false
	d$irrigated <- tolower(d$irrigated)
	d$irrigated[d$irrigated == ""] <- NA
	d$irrigated <- d$irrigated == "irrigated"

	## year to start year / end year
	d$start_date <- NA
	d$end_date <- NA
	i <- nchar(d$year) == 4
	d$start_date[i] <- d$year[i]
	d$end_date[i] <- d$year[i]

	i <- nchar(d$year) == 9
	d$start_date[i] <- substr(d$year[i], 1, 4)
	d$end_date[i] <- substr(d$year[i], 6, 9)

	i <- d$year == "2007-8"
	d$start_date[i] <- 2007
	d$end_date[i] <- 2008

	i <- d$year == "2008-09"
	d$start_date[i] <- 2008
	d$end_date[i] <- 2009
	
	d$year <- NULL

	## Georeferencing --- more to be done
	d <- d[order(d[,1]), ]
	d[d$location == "Laie-Gaient Woreda", "adm1"] <- "Laie-Gaient Woreda"
	d$longitude[d$location=="Nejo"] <- 35.5
	d$longitude[d$location=="Nedjo, West Wollega"] <- 35.5

#	d$sub_source_id <- as.integer(as.factor(d$reference))
	d$crop <- tolower(d$crop)
	d$crop[d$crop == "haricot bean"] <- "common bean"
	d$crop[d$crop == "field pea"] <- "pea"

	f1 <- carobiner::fix_name(d$fertilizer_type_1)
	f1 <- gsub(" \\+ ", "; ", f1)
	f1 <- gsub(" & ", "; ", f1)
	f1 <- gsub(" and ", "; ", f1)
	f1 <- gsub("Ethiopian rock phosphate \\(ERP)", "ERP", f1)
	f1 <- gsub("Gafsa rock Phosphate \\(GRP)", "GRP", f1)
	f1 <- gsub("GRP mixture \\(1:4)", "GRP", f1)
	f1 <- gsub("Basic slag", "basic slag", f1)
	f1 <- gsub("Bone meal", "bone meal", f1)
	f1 <- gsub("Bone meal", "bone meal", f1)
	f1 <- gsub("Organic P", "bone meal", f1) # assigning "Organic P" as "bone meal"
	
	f2 <- carobiner::fix_name(d$fertilizer_type_2)
	f2 <- gsub("Ammonium sulphate and Potassium sulphate", "DAS; SOP", f2)	
	f2 <- gsub("Ammonium sulphate", "DAS", f2)
	f2 <- gsub("Potassium Sulphate", "SOP", f2)

	i = !is.na(f2)
	f1[i] <- paste0(f1[i], "; ", f2[i])
	
	d$fertilizer_type <- f1
	d$fertilizer_type_2 <- NULL
	d$fertilizer_type_1 <- NULL

	d$trial_type <- NULL

	p <- carobiner::fix_name(d$previous_crop, "lower")
	p <- gsub("/", "; ", p)
	p <- gsub("tef$", "teff", p)
	p <- gsub("tef;", "teff;", p)
	p <- gsub("soybean\\(scs-1)", "soybean", p)
	p <- gsub("oats-vetch mixture", "oats; vetch", p)
	p <- gsub("dolichos", "lablab", p)
	p <- gsub("barely", "barley", p)
	p <- gsub("none", "no crop", p)
	
	d$previous_crop <- p

	i <- grep("loam", d$comments)
	d$soil_type[i] <- d$comments[i]
	d$comments[i] <- ""

	i <- grep("Source", d$comments)
	src <- gsub("Source:", "", d$comments[i])
	src <- gsub("Review : ", "", src)
	src <- trimws(src)
	d$reference[i] <- paste0(d$reference[i], "; ", src)
	d$comments[i] <- ""

	i <- grep("\\(19", d$comments)
	d$reference[i] <- paste0(d$reference[i], "; ", d$comments[i])
	d$comments[i] <- ""
	
	i <- d$comments %in% c("Alemayehu et al 2006 ARARI Proc", "Mesfin: T.: G.B. Tesfahunegn: C.S. Wortmann: M. Mamo: and O. Nikus. 2010. Skip-row planting and tie-ridging for sorghum production in semi-arid areas of Ethiopia. Agron J. 102:745-750.", "Tilahun: et al.: ARARI Proc 2007", "Alemayehu Assefa et al.: ARARI Proc 2006")
	d$reference[i] <- d$comments[i]
	d$comments[i] <- ""
	
	i <- (d$comments == "Minale Liben et al.: ARARI Proc 2006") & (d$reference != "Tilahun Tadesse")
	d$reference[i] <- d$comments[i]
	i <- d$comments == "Minale Liben et al.: ARARI Proc 2006"
	d$comments[i] <- ""

	i <- d$comments == "Anon. 1998–2000. Progress Reports of BARC. Bako: Ethiopia." 
	d$reference[i] <- paste0(d$reference[i], "; ", d$comments[i])
	d$comments[i] <- ""
	
	
	i <- d$comments == "The control also received some N (about 18 kgs) through the DAP" & d$P > 0 & d$N == 0
	d$N[i] <- 18
	i <- d$comments == "The control also received some N (about 18 kgs) through the DAP"
	d$comments[i] <- ""
	
	d$plant_spacing <- ""
	i <- grep("Plant density", d$comments)
	d$plant_spacing[i] <- d$comments[i]
	d$comments[i] <- ""
	i <- grep("spacing", d$comments)
	d$plant_spacing[i] <- d$comments[i]
	d$comments[i] <- ""

	# unique(d$comments)
	d$comments <- NULL
	d$crop_system <- NULL
	
	d <- carobiner::change_names(d, c("response", "N", "P", "K", "Zn", "S"), 
	c("treatment", "N_fertilizer", "P_fertilizer", "K_fertilizer", "Zn_fertilizer", "S_fertilizer"))

	d$OM_used <- d$OM_used == "Yes"
	d$inoculated <- d$inoculated == "Yes"

	vv = c("adm1", "trial_id", "variety", "variety_type", "soil_type", "OM_type", "season", "tillage", "uncertainty_type", "uncertainty")
	for (v in vv) d[[v]] <- carobiner::fix_name(d[[v]])

	d$uncertainty[d$uncertainty=="NS"] <- NA
	d$uncertainty <- as.numeric(d$uncertainty)
	
	ps <- d$plant_spacing
	d$plant_spacing <- NULL
	i <- ps == "Inter-row spacing (55cm)"
	d$row_spacing[i] <- 55
	i <- ps == "Inter-row spacing (65cm)"
	d$row_spacing[i] <- 65
	i <- ps == "Inter-row spacing (75cm)"
	d$row_spacing[i] <- 75

	i <- grep("Plant density \\(n perha)", ps)
	d$plant_density <- NA
	d$plant_density[i] <- as.numeric(gsub("Plant density \\(n perha)", "", ps[i]))
	
	d$N_splits <- as.numeric(d$N_splits)
	
	carobiner::write_files(dset, d, path, dataset_id, group)

}

