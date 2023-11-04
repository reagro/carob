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
		data_citation='Gathala, Mahesh K. and Tiwari, Thakur P. and Islam, Saiful and Khan, A.S.M. and Anwar, Mazharul and Hossain, Illias and Siddique, Nur-E-A and Hossain, Shakhawat and Rahman, Mohammad Atiqur,2018,"Rabi (winter) crops-all nodes-Validation trials-Rajshahi-Bangladesh",
		https://hdl.handle.net/11529/10548007, CIMMYT Research Data & Software Repository Network, V2',
		publication= NA,
		data_institutions = "CIMMYT",
   	data_type="on-farm experiment",
		carob_contributor="Mitchelle Njukuya",
		carob_date="2023-11-03"
	  )

## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)

	
	ff <- ff[grep("Rajshahi", basename(ff))]
	bn <- basename(ff)
	## process file(s)
	## process Rabi maize 2015-16-DS&BP-all nodes Rajshahi files
	f<- ff[1] 
	   r <- carobiner::read.excel(f,sheet ="2 - Site information",skip=3)
	   d1 <- r[,c("Season","Node")]
	   d1$treatment <- r$`Tmnt (Short abbreviation as mentioned in protocol)`
	   d1$soil_type<- r$`Soil texture (sand, silt, clay etc.)`
	   colnames(d1)<- c("season","location","treatment","soil_type")
	   d1<-d1[-c(1,2),] 
	   row.names(d1) <- NULL
	   r11 <- carobiner::read.excel(f,sheet ="4- Stand counts & Phenology", skip=3 )
	   
	   d2 <- r11[,c("Season","Tmnt","Node","Variety")]
	   d2$row_spacing<- r11$`Row spacing (cm)`
	   d2$crop<- r11$`Types of Trial`
	   d2$planting_date<- r11$`Date of seeding (dd-mm-yy)`
	   d2$harvest_date<- r11$`Datw of harvest (dd-mm-yy)`
	   colnames(d2)<- c("season","treatment","location","variety","row_spacing","crop","planting_date","harvest_date")
	   d2<-d2[-c(1),] 
	   row.names(d2) <- NULL
	   
	   r12 <- carobiner::read.excel(f,sheet ="6 - Fertilizer amounts ", skip=3 )
	   r12$fetilizer_type <- paste(r12$`Fertilizer 1 Application`,r12$`Fertilizer 2 Application`,r12$`Fertilizer 3 Application`, r12$`Fertilizer 4 Application`, r12$`Fertilizer 5 Application`,sep ="; ")
	   d3<- r12[,c("Season","Node","Tmnt","fetilizer_type")]
	   colnames(d3)<- c("season","location","treatment","fertilizer_type")
	   colnames(r12)[c(12:length(r12))] <- as.list(r12[1,][,c(12:length(r12))])
	   #names(r12)<- gsub("Gypsum ","Gypsum",names(r12))
	   d3$N_fertilizer<- r12$`N    (kg/ha)`  
	   d3$P_fertilizer<- r12$`P2O5 (kg/ha)`
	   d3$K_fertilizer<- r12$`K    (kg/ha)`
	   d3$gypsum<- r12$`Gypsum(Kg/ha)`
	   d3$Zn_fertilizer<- r12$`ZnSO4 (kg/ha)` 
	   d3<-d3[-c(1),] 
	   row.names(d3) <- NULL
	   
	   r14 <- carobiner::read.excel(f,sheet ="14 - Grain Harvest ", skip=3 )
	   colnames(r14)[c(26:30)] <- as.list(r14[1,][,c(26:30)])
	   d4 <- r14[,c("Season","Node","Tmnt")]
	   colnames(d4)<- c("season","location","treatment")
	   d4$yield <- r14$`Grain yield (t/ha)`
	   d4$residue_yield  <- r14$`Straw yield (t/ha)`
	   d4$biomass_total  <- r14$`Biomass (t/ha)` 
	   d4<-d4[-c(1),] 
	   row.names(d4) <- NULL
	   ## merge all 
	   dd <- merge(d1,d2,by=c("season","location","treatment"),all.x  = T)
	   dd<- merge(dd,d3,by=c("season","location","treatment"),all.x = T)
	   dd<- merge(dd,d4,by=c("season","location","treatment"),all.x = T)
	   dd<- unique(dd)
	  
	
	   ## process Wheat 2014-15-DS&BP-all nodes Rajshahi files
	   f<- ff[2] 
	   r <- carobiner::read.excel(f,sheet ="2 - Site information", skip=3 )
	   d1 <- r[,c("Season","Node")]
	   d1$treatment <- r$`Tmnt (Short abbreviation as mentioned in protocol)`
	   d1$soil_type<- r$`Soil texture (sand, silt, clay etc.)`
	   colnames(d1)<- c("season","location","treatment","soil_type")
	   d1<-d1[-c(1,2),] 
	   row.names(d1) <- NULL
	   r11 <- carobiner::read.excel(f,sheet ="4- Stand counts & Phenology", skip=3 )
	   
	   d2 <- r11[,c("Season","Tmnt","Node","Variety")]
	   d2$row_spacing<- r11$`Row spacing (cm)`
	   d2$crop<- r11$`Types of Trial`
	   d2$planting_date<- r11$`Date of seeding (dd-mm-yy)`
	   d2$harvest_date<- r11$`Datw of harvest (dd-mm-yy)`
	   colnames(d2)<- c("season","treatment","location","variety","row_spacing","crop","planting_date","harvest_date")
	   d2<-d2[-c(1),] 
	   row.names(d2) <- NULL
	   
	   r12 <- carobiner::read.excel(f,sheet ="6 - Fertilizer amounts ", skip=3)
	   r12$fetilizer_type <- paste(r12$`Fertilizer 1 Application`,r12$`Fertilizer 2 Application`,r12$`Fertilizer 3 Application`, r12$`Fertilizer 4 Application`, r12$`Fertilizer 5 Application`,sep ="; ")
	   d3<- r12[,c("Season","Node","Tmnt","fetilizer_type")]
	   colnames(d3)<- c("season","location","treatment","fertilizer_type")
	   colnames(r12)[c(12:length(r12))] <- as.list(r12[1,][,c(12:length(r12))])
	   d3$N_fertilizer<- r12$`N    (kg/ha)`  
	   d3$P_fertilizer<- r12$`P2O5 (kg/ha)`
	   d3$K_fertilizer<- r12$`K    (kg/ha)`
	   d3$gypsum<- r12$`Gypsum(Kg/ha)`
	   d3$Zn_fertilizer<- r12$`ZnSO4 (kg/ha)` 
	   d3<-d3[-c(1),] 
	   row.names(d3) <- NULL
	   
	   r14 <- carobiner::read.excel(f,sheet ="14 - Grain Harvest ", skip=3 )
	   colnames(r14)[c(26:30)] <- as.list(r14[1,][,c(26:30)])
	   d4 <- r14[,c("Season","Node","Tmnt")]
	   colnames(d4)<- c("season","location","treatment")
	   d4$yield <- r14$`Grain yield (t/ha)`
	   d4$residue_yield  <- r14$`Straw yield (t/ha)`
	   d4$biomass_total  <- r14$`Biomass (t/ha)` 
	   d4<-d4[-c(1),] 
	   row.names(d4) <- NULL
	   
	   ## merge all 
	   ##CN 
	 
	   dd1 <- merge(d1,d2,by=c("season","location","treatment"),all.x  = T)
	   dd1<- merge(dd1,d3,by=c("season","location","treatment"),all.x = T)
	   dd1<- merge(dd1,d4,by=c("season","location","treatment"),all.x = T)
	   dd1<- unique(dd1)
	   
	   
	   ## process Wheat 2015-16-DS&BP-all nodes Rajshahi files
	   f<- ff[3] 
	   r <- carobiner::read.excel(f,sheet ="2 - Site information", skip=3 )
	   d1 <- r[,c("Season","Node")]
	   d1$treatment <- r$`Tmnt (Short abbreviation as mentioned in protocol)`
	   d1$soil_type<- r$`Soil texture (sand, silt, clay etc.)`
	   colnames(d1)<- c("season","location","treatment","soil_type")
	   d1<-d1[-c(1,2),] 
	   row.names(d1) <- NULL
	   r11 <- carobiner::read.excel(f,sheet ="4- Stand counts & Phenology", skip=3 )
	   
	   d2 <- r11[,c("Season","Tmnt","Node","Variety")]
	   d2$row_spacing<- r11$`Row spacing (cm)`
	   d2$crop<- r11$`Types of Trial`
	   d2$planting_date<- r11$`Date of seeding (dd-mm-yy)`
	   d2$harvest_date<- r11$`Datw of harvest (dd-mm-yy)`
	   colnames(d2)<- c("season","treatment","location","variety","row_spacing","crop","planting_date","harvest_date")
	   d2<-d2[-c(1),] 
	   row.names(d2) <- NULL
	   
	   r12 <- carobiner::read.excel(f,sheet ="6 - Fertilizer amounts ", skip=3 )
	   r12$fetilizer_type <- paste(r12$`Fertilizer 1 Application`,r12$`Fertilizer 2 Application`,r12$`Fertilizer 3 Application`, r12$`Fertilizer 4 Application`, r12$`Fertilizer 5 Application`,sep ="; ")
	   d3<- r12[,c("Season","Node","Tmnt","fetilizer_type")]
	   colnames(d3)<- c("season","location","treatment","fertilizer_type")
	   colnames(r12)[c(12:length(r12))] <- as.list(r12[1,][,c(12:length(r12))])
	   d3$N_fertilizer<- r12$`N    (kg/ha)`  
	   d3$P_fertilizer<- r12$`P2O5 (kg/ha)`
	   d3$K_fertilizer<- r12$`K2O    (kg/ha)`
	   d3$gypsum<- r12$`Gypsum(Kg/ha)`
	   d3$Zn_fertilizer<- r12$`ZnSO4 (kg/ha)` 
	   d3<-d3[-c(1),] 
	   row.names(d3) <- NULL
	   
	   r14 <- carobiner::read.excel(f,sheet ="14 - Grain Harvest ", skip=3 )
	   colnames(r14)[c(26:30)] <- as.list(r14[1,][,c(26:30)])
	   d4 <- r14[,c("Season","Node","Tmnt")]
	   colnames(d4)<- c("season","location","treatment")
	   d4$yield <- r14$`Grain yield (t/ha)`
	   d4$residue_yield  <- r14$`Straw yield (t/ha)`
	   d4$biomass_total  <- r14$`Biomass (t/ha)` 
	   d4<-d4[-c(1),] 
	   row.names(d4) <- NULL
	   ## merge all 
	   dd2 <- merge(d1,d2,by=c("season","location","treatment"),all.x  = T)
	   dd2<- merge(dd2,d3,by=c("season","location","treatment"),all.x = T)
	   dd2<- merge(dd2,d4,by=c("season","location","treatment"),all.x = T)
	   dd2<- unique(dd2)
	  
	 ###CN
	   ## could we create a function to do that?
 ## Append all the data
	d<- rbind(dd,dd1,dd2) 
	#add columns
	d$dataset_id <- dataset_id
	d$trial_id <- paste(d$location, d$treatment, sep = "_")
	##CN
	## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
	d$country <- "Bangladesh"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$inoculated <- FALSE
	d$yield_part <- "grain" 
	### fix crop name
	d$crop<- carobiner::fix_name(d$crop,"lower")
	p<- carobiner::fix_name(d$fertilizer_type)
	p<- gsub("Urea","urea",p)
	p<- gsub("Gypsum","gypsum",p)
	p<- gsub("Gypsum","gypsum",p)
	d$fertilizer_type <- p
	#add lon and lat coordinate 
	geo= data.frame(location=c("Dharmapur","Nabinagar","Premtoli" ,"Baduria","Dharampur","Laxmipur" ),
	                lat=c(23.3108087,23.8846959,25,24.34255,24.879025,22.9445365),
	                lon=c(91.2876786,90.9699944,89,88.72014,89.3533518,90.8276345))
	
	d<- merge(d,geo,by="location",all.x = T)
	d$longitude<- d$lon
	d$latitude <- d$lat
	d$lon <- d$lat  <- NULL
	

   
##data type

	d$N_fertilizer<- as.double(d$N_fertilizer)
	d$P_fertilizer<- as.double(d$P_fertilizer)
	d$K_fertilizer<- as.double(d$K_fertilizer)
	d$Zn_fertilizer<- as.double(d$Zn_fertilizer)
	d$gypsum<- as.double(d$gypsum)
	d$yield<- (as.double(d$yield))*1000
	d$residue_yield<- (as.double(d$residue_yield))*1000
	d$biomass_total<- as.double(d$residue_yield)
	d$planting_date<-  as.character(as.Date(d$planting_date))
	d$harvest_date<-  as.character(as.Date(d$harvest_date))
	 
	
	# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
	
}


