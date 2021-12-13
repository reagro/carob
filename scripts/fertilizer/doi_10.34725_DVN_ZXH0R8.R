# R script for "carob"

## ISSUES
# Planting date and harvesting dates, fertilizer application dates, cropping system, altutude are important. They are not in the records section.but some data sets have. How do you identify if these are on-station trials?
#"  unknown records variable names:  residue_yield"
#"   crop contains invalid terms:  Wheat, Sorghum, Tef"

library(carobiner)
carob_script <- function(path) {

"
Description:
Title: Landscape Targeted Crop-Fertilizer Response in the Highlands of Ethiopia
Short description: The dataset is meant for developing fertilizer management decision support tool for an effective crop-nutrient management. The dataset is developed on the basis of landscape targeting on-farm trials on crop-nutrient response and crop yield gap assessment across the Africa Rising target districts and other scaling up locations in the Ethiopian highlands.
Related publications: Amede, Tilahun; Gashaw, Tadesse; Legesse, Gizachew; Tamene, Lulseged; Mekonen, Kindu; Thorne, Peter; and Schultz, Steffen. 2020. Landscape Positions Dictating Crop Fertilizer Responses in Wheat-Based Farming Systems of East African Highlands. Renewable Agriculture and Food Systems, pp.1-13. doi: 10.1017/S1742170519000504 https://doi.org/10.1017/S1742170519000504

ICRISAT. 2018. Feeding Degraded Soils in Ethiopia to Feed the People and the Environment. Patancheru, India: ICRISAT. handle: https://hdl.handle.net/10568/91676 https://hdl.handle.net/10568/91676 
  "

	uri <- "doi:10.7910/DVN/ZXH0R8"
	dataset_id <- agro::get_simple_URI(uri)
	group <- "fertilizer"
	dset <- data.frame(
		dataset_id = dataset_id,
		group = group,
		uri = uri,
		publication = "doi:10.1017/S1742170519000504",
		carob_contributor = "Siyabusa",
		experiment_type = "fertilizer"
		#has_weather = FALSE,
		#has_management = FALSE
	)

## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, major=1, minor=0, group)
	dset$license <- carobiner::get_license(js)

	
	#######2014 & 2015 Wheat Yields
	f <- ff[basename(ff) == "001_2014-2015_Wheat_ICRISAT-AR_ETH.xlsx"]
	d <- suppressMessages(as.data.frame(readxl::read_excel(f)))
	d <- d[,-c(2,3,4,9,10,14,15,16,17,24,26,28,29,31,32,33,34,35,37,38,41,42,43,44)]

	#Combine Basal & top Dressing ammounts(Sum of Rows)
	d$totalN <- rowSums(d[,c(10,15,16)])
	
	#remove the extra fertilizer columns
	d <- d[,-c(10,15,16)]
	
	#Add/format first column
	d$dataset_id <- "doi_10.7910_DVN_ZXH0R8"
	d$trial_id<-"ICRISAT-AR_2014-2015"
	d$on_farm<-FALSE
	d$is_survey<-FALSE
	d$end_date <-"2014"
	d$N_splits<-2
	##Replace certain values in the data frame
	d[311:550,'end_date']<-2015
	
	#rename headers
	d<-d[,c("Country","Region/state","LGA/District","village/Kebele","Year","Crop","Variety","Treatment","P fertilizer amount","K fertilizer amount (K2O(kg/ha))","S fertilizer amount (S(kg/ha))","Zn fertilizer amount (Zn(kg/ha))","Soil type","Yield (kg/ha)","Biomass (kg/ha)","Stover yield (kg/ha","totalN","trial_id","dataset_id","end_date","N_splits")]                      
	colnames(d)<-c("country","region","adm1","adm2","start_date","crop","variety","treatment","P_fertilizer","K_fertilizer","S_fertilizer","Zn_fertilizer","soil_type","yield","biomass_total","residue_yield","N_fertilizer","trial_id","dataset_id","end_date","N_splits")

#Re-order
	d<-d[c("dataset_id","trial_id","country","region","adm1","adm2","start_date","end_date","crop","variety","treatment","N_fertilizer","N_splits","P_fertilizer","K_fertilizer","S_fertilizer","Zn_fertilizer","soil_type","yield","biomass_total","residue_yield")]

	
	#######2016 Wheat Yields
	g <- ff[basename(ff) == "002_2016_Wheat_ ICRISAT-AR_ETH.xlsx"]
	e <- suppressMessages(as.data.frame(readxl::read_excel(g)))
	e<-e[,-c(1,2,3,4,9,10,14,15,16,17,23,25,27,28,30,31,32,33,34,36,37,39,40,41,42,43)]
	
	#Combine Basal & top Dressing amounts(Sum of Rows)
	e$totalN<-rowSums(e[,c(9,13,14)])
	
	#remove the extra fertilizer columns
	e<-e[,-c(9,13,14)]

	#Add/format first column
	e$dataset_id <- "doi_10.7910_DVN_ZXH0R8"
	e$trial_id<-"ICRISAT-AR_2016"
	e$on_farm<-FALSE
	e$is_survey<-FALSE
	e$end_date <-"2016"
	e$N_splits<-2
	e$Zn_fertilizer<-NA
	
	#remove negative values
	e$stoveryield<-e$`Biomass (kg/ha)`-e$`Yield (kg/ha)`
	#names(e)
	s <- e[,23]
	t <- abs(s)
	e <- cbind(e,t)
	e <- e[,-23]
	
	#rename headers
	e <- e[,c("Country","Region/state","LGA/District","village/Kebele","Year","Crop","Variety","Treatment","P fertilizer amount (kg/ha)","K fertilizer amount (kg/ha)","S fertilizer amount (kg/ha)","Soil type","Yield (kg/ha)","Biomass (kg/ha)","totalN","dataset_id","trial_id","on_farm","is_survey","end_date","N_splits","Zn_fertilizer","t")]     
	colnames(e)<-c("country","region","adm1","adm2","start_date","crop","variety","treatment","P_fertilizer","K_fertilizer","S_fertilizer","soil_type","yield","biomass_total","N_fertilizer","dataset_id","trial_id","on_farm","is_survey","end_date","N_splits","Zn_fertilizer","residue_yield")
	
	#Re-order
	e <- e[c("dataset_id","trial_id","country", "region","adm1","adm2","start_date","end_date","crop","variety","treatment","N_fertilizer","N_splits","P_fertilizer","K_fertilizer","S_fertilizer","Zn_fertilizer","soil_type","yield","biomass_total","residue_yield")]  
	

	#######2017 Sorghum+Teff Yields
	h <- ff[basename(ff) == "003_2017_Sorghum+Tef_ ICRISAT-AR_ETH.xlsx"]
	k <- suppressMessages(as.data.frame(readxl::read_excel(h)))
	k <- k[,-c(1:4,9:10,14:17,24,26,28:29,31:35,37:38,41:47)]
	
	#Combine Basal & top Dressing ammounts(Sum of Rows)
	k$totalN<-rowSums(k[,c(9,14,15)])

	#remove the extra fertilizer columns
	k<-k[,-c(9,14,15)]
	
	#Add/format first column
	k$dataset_id <- "doi_10.7910_DVN_ZXH0R8"
	k$trial_id<-"ICRISAT-AR_2017"
	k$on_farm<-FALSE
	k$is_survey<-FALSE
	k$end_date <-"2017"
	k$N_splits<-2
	
	#rename headers
	k<-k[,c("Country","Region/state","LGA/District","village/Kebele","Year","Crop","Variety","Treatment","P fertilizer amount (kg/ha)","K fertilizer amount (kg/ha)","S fertilizer amount (kg/ha)","Zn fertilizer amount (kg/ha)","Soil type","Yield (kg/ha)","Biomass (kg/ha)","Stover yield (kg/ha","totalN","dataset_id","trial_id","on_farm","is_survey","end_date","N_splits")]
	colnames(k)<-c("country","region","adm1","adm2","start_date","crop","variety","treatment","P_fertilizer","K_fertilizer","S_fertilizer","Zn_fertilizer","soil_type","yield","biomass_total","residue_yield","N_fertilizer","dataset_id","trial_id","on_farm","is_survey","end_date","N_splits")        

	#Re-order
	 k<-k[c("dataset_id","trial_id","country","region","adm1","adm2","start_date","end_date","crop","variety","treatment","N_fertilizer","N_splits","P_fertilizer","K_fertilizer","S_fertilizer","Zn_fertilizer","soil_type","yield","biomass_total","residue_yield")]

	  
	  
	  #######2019 Sorghum+Teff Yields
	  m <- ff[basename(ff) == "004_2019_Wheat_ ICRISAT-AR_ETH.xlsx"]
	  n <- suppressMessages(as.data.frame(readxl::read_excel(m)))
	  
	  #d<-d[,-c("Data set", "Identifier no.","Altitude(m)","Nearest climate station","Planting date","Harvest date","Crop system","Landscape strata","N fertilizer date", "N topdressing date","Weeding","Rainfall","Soil organic content", "Soil mineral N at sowing","Soil moisture at sowing","Rooting Depth","Soil description","37","% moisture yield data","41","Grain nutrient content","Stover nutrient content","Harvest Index")]
	  n <- n[,-c(1:4,9:10,14:17,24,26,28:29,31:35,37:38,41:60)]
	  
	  #Combine Basal & top Dressing ammounts9Sum of Rows)
	  n$totalN <- rowSums(n[,c(9,14,15)])
	  
	  #remove the extra fertilizer columns
	  n<-n[,-c(9,14,15)]
	  
	  #Add/format first column
	  n$dataset_id <- "doi_10.7910_DVN_ZXH0R8"
	  n$trial_id<-"ICRISAT-AR_2019"
	  n$on_farm<-FALSE
	  n$is_survey<-FALSE
	  n$end_date <-"2020"
	  n$N_splits<-2
	  
	  #rename headers
	  n<-n[,c("Country","Region/state","LGA/District","village","Year","Crop","Variety","Treatment","P fertilizer amount (kg/ha)","K fertilizer amount (kg/ha)","S fertilizer amount (kg/ha)","Zn fertilizer amount (kg/ha)","Soil type","Yield (kg/ha)","Biomass (kg/ha)","Stover yield (kg/ha","totalN","dataset_id","trial_id","on_farm","is_survey","end_date","N_splits")]    
	  colnames(n)<-c("country","region","adm1","adm2","start_date","crop","variety","treatment","P_fertilizer","K_fertilizer","S_fertilizer","Zn_fertilizer","soil_type","yield","biomass_total","residue_yield","N_fertilizer","dataset_id","trial_id","on_farm","is_survey","end_date","N_splits")        

	  #Re-order
	  n<-n[c("dataset_id","trial_id","country","region","adm1","adm2","start_date","end_date","crop","variety","treatment","N_fertilizer","N_splits","P_fertilizer","K_fertilizer","S_fertilizer","Zn_fertilizer","soil_type","yield","biomass_total","residue_yield")]
	
####Merge Data Sets Across All Seasons
	 #names(d)
	 #names(e)
	 #names(k)
	 #names(n)
	  
	ds<-rbind(d,e,k,n)
	ds$crop <- tolower(ds$crop)
	ds$crop[ds$crop=="tef"] <- "teff"
	
	carobiner::write_files(dset, ds, path, dataset_id, group)
}	  
	  
	  
