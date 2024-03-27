# R script for "carob"


carob_script <- function(path) {
  
"The objective of this work set is to demonstrate the best options currently available for the management of conservation agriculture (CA) practices in different communities in Mozambique. Eleven communities were selected from the districts of Sofala, Tete and Manica (approximately 100 to 200 families in each community) to host these demonstration sites and six demo fields were installed in each community from 2006-2015 (9 seasons). The treatments in each community were as follows: 1. Farmers' practice (control)- Traditional management with removal of stubble. 2. Conservation agriculture- The stubble
is kept in the ground, there is no preparation of the ground, and the sowing is done manually in covachos previamento open (see the management of the covachos) and with SULCADOR in Nhamatiquite. 3. Direct sowing (SD): The stubble is kept in the soil, the direct sowing is done with Matraca or sharp bread. (2016-12-08)" 
  
	uri <- "hdl:11529/10830" 
	group <- "conservation_agriculture"
	ff <- carobiner::get_data(uri, path, group)
	## dataset level data 
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=1),
		project=NA,
		publication= "https://doi.org/10.1017/S1742170515000332",
		data_institutions = "IITA",
		data_type="experiment", 
		carob_contributor="Effie Ochieng'",
		carob_date="2023-11-21"
	)
	
	f <- ff[basename(ff) == "Summary Mozambique On-farm Demonstration 2006-2015.xlsx" ]
	r <- readxl::read_excel(f, sheet = 1)
	r1 <- readxl::read_excel(f, sheet = 2)
	
	## use a subset
	d <- r[, c("Harvest Year","District","Village","Farmer","Crop grown","Tmnt.","Variety","Final stand (pl/ha)", "Stalk yield (kg/ha)" ,"Grain yield (kg/ha)")]
	colnames(d) <- c("harvest_date","adm2","site","farmer","crop","treatment","variety","plant_density","residue_yield","yield")
	
	d1 <- r1[, c("Harvest Year","District","Village","Farmer","Tmnt.","Crop grown","Final stand (pl/ha)","Stalk yield (kg/ha)","Grain yield (kg/ha)")]
	colnames(d1) <- c("harvest_date","adm2","site","farmer","treatment","crop","plant_density","residue_yield","yield")
	
	dd1 <- carobiner::bindr(d, d1)
	
	trmt <- carobiner::fix_name(dd1$treatment, "lower")
	trmt[trmt %in% c("conventional control","tradicional","conventional","conevntional","convencional","check")] <- "control"
	trmt[trmt %in% c("basins","bacio","bacia", "matraca","sulcador","coavacho","couvacho","covacho")] <- "conservation_agriculture"
	trmt[trmt %in% c("direct seeding","ds", "direct seeder","siembra directa","pao")] <- "direct_sowing"
	dd1$treatment <- trmt
	# efyrouwa: "ripper treatment meaning??"
	
	fam <-carobiner::fix_name(dd1$farmer, "lower") 
	fam <- gsub("\\.", " ", fam)
	fam <- gsub("	"," ",fam)
	fam <- gsub(" ", "_",fam)
	fam <- gsub("\\/","",fam)
	dd1$farmer <- fam
	
	crop <- dd1$crop
	crop[crop %in% c( "Maize","1","2","3")] <- "maize"
	crop[crop %in% c("Beans","Feijao")] <- "common bean"
	crop[crop == "Soybean"] <- "soybean"
	dd1$crop <- crop
	
	dd1$trial_id <- ifelse(!is.na(dd1$variety),paste(dd1$farmer,dd1$variety,dd1$treatment,sep ="_"),paste(dd1$farmer,dd1$crop,dd1$treatment, sep ="_"))
	dd1$farmer <- NULL
	dd1$country <- "Mozambique"
	dd1$yield_part <- ifelse(dd1$crop == "maize", "grain","seed")
	dd1$on_farm <- TRUE
	dd1$is_survey <- FALSE
	dd1$harvest_date <- as.character(dd1$harvest_date)
	dd1$site <- carobiner::replace_values(dd1$site, c("Nhamizhinga","Maguai","Madjiga","Belia","Gimu","Lamego Ndeja","Mussianharo","Malomwe","Lamego John Segredo","Madjigo","Guaraguara, Belia","Nhaufo","Madgiga","Mussinharo"),c("Nhamizinga","Tsangano","Magiga","Buzi","Tsangano","Lamego","Barue","Malomue",	"Lamego","Magiga","Tsangano","Buzi","Magiga","Barue"))
	
	LL <- data.frame(
					 site = c("Pumbuto", "Nhanguo", "Puanda", "Guro", "Malomue","Ruaca", "Nhamizinga", "Nzewe","Nhamatiquite", "Tsangano", "Magiga","Lamego", "Buzi", "Ulongue", "Nharuchonga", "Barue", "lamego","Gimo"), 
					 latitude = c(-19.0025, -21.195, -19.8500034, -16.95262, -18.17028,	-13.11722, -17.06833, -14.519, -19.24278, -15.20012, -13.74806, -19.33251,-20.03925, -14.72278, -19.23972, -17.81047, -19.33251, -18.60111), 
					 longitude = c(33.75028, 34.94222, 34.17028, 33.51865, 33.3075, 38.21194, 34.83083, 34.304, 33.76694, 34.32685, 35.26222, 34.31678, 34.37237, 34.36083, 34.12306, 33.17267,34.31678, 34.545))
	
	dd2 <- merge(dd1,LL, by = "site", all.x = TRUE)
	dd2 <- dd2[complete.cases(dd2$yield),] 
	carobiner::write_files(dset, dd2, path=path)
}

# carob_script(path)

