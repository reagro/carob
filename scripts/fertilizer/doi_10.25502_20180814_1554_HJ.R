# this is already included in doi_10.7910_DVN_UNLRGC


carob_script <- function(path) {
"The AFSIS project aimed to establish an  Africa Soil Information system. Data was collected in sentinel sites across sub-Saharan Africa using the Land Degradation Surveillance framework and included also multi-location diagnostic trials in selected sentinel sites to determine nutrient limitations and response to improved soil management practices (soil amendments)
"

	uri <- "doi:10.25502/20180814/1554/HJ"
	group <- "fertilizer"

	ff <- carobiner::get_data(uri, path, group)

  ## dataset level data 
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=2, minor=1),
		publication=NA,
		#data_citation = "Huising, J. (2018). Africa Soil Information System - Phase 1, Tuchila S2 [Data set]. International Institute of Tropical Agriculture (IITA). doi:10.25502/20180814/1554/HJ",
		project = NA,
		data_institutions = "IITA",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-02-15",
		data_type="experiment"
	)
    
	f1 <- ff[basename(ff) == "Tuchila_S2_Field.csv"] ## get Field dataset 
	f2 <- ff[basename(ff) == "Tuchila_S2_Plot.csv"] ## get plot dataset
	#fx <- ff[basename(ff) == "Tuchila_S2_Plant.csv"] ## get plant dataset
 
   # read the dataset
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	#rx <- read.csv(fx)
  
	d1 <- data.frame(
		trial_id = r1$ID,
		location = r1$Village,
		latitude = r1$Flat,
		longitude = r1$Flong, 
		variety_type = r1$TCVariety,
		previous_crop = r1$PCrop1,
		OM_type = r1$MType1,
  #fertilizer_type = r1$FType1
		site = r1$Site,
		country = "Malawi",
		crop = "maize",
		yield_part = "grain",
		FieldID = r1$FieldID
	)

	d1$OM_used= ifelse(d1$OM_type %in% c("", "None"), FALSE, TRUE)
  
	p <- carobiner::fix_name(gsub("/", ";", d1$previous_crop), "lower")
	p <- gsub("maize\\+pigion peas", "maize;pigeon pea", p)
  ### RH should this also be pigeon pea?
	p <- gsub("maize\\+peas", "maize;peas", p)
	p <- gsub("pigion pea", "pigeon pea", p)
	d1$previous_crop <- p
### ??? please do not remove data. If you do not know how to fix it, ask for help. 
### d1 <- replace(d1,d1=='maize+peas',NA)
  
  #process plot data 
   
	d2 <- data.frame(
		rep = r2$Rep,
		treatment = r2$TrtDesc,
		yield = r2$TGrainYld_adj * 1000,
		residue_yield = r2$AdjTStoverYld * 1000,
		season = as.character(r2$Season),
		FieldID = r2$FieldID
	)
  	d2$N_fertilizer <- ifelse(r2$TrtDesc=="Control", 0,
                        ifelse(r2$TrtDesc=="PK", 0, 100))
  
	d2$K_fertilizer <- ifelse(r2$TrtDesc=="Control", 0,
                        ifelse(r2$TrtDesc=="NP", 0, 60))
  
	d2$P_fertilizer <- ifelse(r2$TrtDesc=="Control", 0,
                        	ifelse(r2$TrtDesc=="NK", 0, 30))
  
	d2$Zn_fertilizer <- ifelse(r2$TrtDesc=="NPK+MN", 3, 0)
  
	d2$S_fertilizer <- ifelse(r2$TrtDesc=="NPK+MN", 5, 0)
  
	d2$N_splits <- ifelse(d2$N_fertilizer > 0, 3L, 0L)



  #merge all the data
## ??What now??
##	d <- merge(d1,d3,by=all.x = TRUE)
#	data type
# 	d$OM_type <- as.character(d$OM_type)
 #	d$OM_used <- as.logical(d$OM_used)

	d <- merge(d1, d2, by="FieldID")
	d$FieldID <- NULL
	#fill whitespace in observation 
# 	d <- replace(d, d=='', NA)

	carobiner::write_files(dset, d, path=path)
}
