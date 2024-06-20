# R script for EiA version of"carob"

## ISSUES
# 1. DOI and much of the metadata is missing
# 2. Data reads are still unstable and user needs to have access
# 3. License is missing (CC-BY)?
# 4. Many valuable variables that need to be integrated still...
# 5. ...

carob_script <- function(path) {
   
   "
	SOME DESCRIPTION GOES HERE...

"
   
   uri <- "doi:Rwanda-RAB-AddOn"
   group <- "eia"
   
   dset <- data.frame(
      # Need to fill-in metadata...
      # carobiner::read_metadata(uri, path, group, major=2, minor=0),
      uri = carobiner::simple_uri(uri),
      dataset_id = uri,
      authors =NA,
      data_institute =NA,
      title = NA,
      group = group,
      license = 'Some license here...',
      carob_contributor = 'Cedric Ngakou',
      project = 'Excellence in Agronomy - Rwanda RAB Add On',
      data_type = "on-farm experiment", # or, e.g. "on-farm experiment", "survey", "compilation"
      carob_date="2024-06-18",
      treatment_vars = "longitude;latitude"
   )
   
   # Manually build path (this can be automated...)
   ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("/home/jovyan/carob-eia/data/raw/eia/Rwanda-RAB-AddOn/", full.names = T))
   
   # Retrieve relevant file
   f <- ff[basename(ff) == "EiA_AddOn_Full_Survey_RAB_Rwanda_2023_11_08.xlsx"]
   # Read relevant file
   ## Process geo data and some crop managment practices 
   r <- carobiner::read.excel(f,sheet = "EiA_AddOn_Survey_SNSRwanda_Fina")
   r <- r[-1,] ## remove the first row of r
   names(r) <- gsub("_index","index",names(r))
   d <- data.frame(
      country = r$country,
      on_farm = FALSE,
      is_survey = TRUE,
      adm1=r$admin_1,
      adm2=r$admin_2,
      adm3=r$admin_3,
      adm4=r$admin_4,
      location=r$village,
      longitude=as.numeric(r$longitude),
      latitude=as.numeric(r$latitude),
      currency=r$local_currency,
      trial_id = r$barcodehousehold,
      technology_use= r$use_case_technology, ## Not a carob variable
      #previous_crop=r$crop_cultivated_all,
      fertilizer_amount= as.numeric(r$fertiliser_amount),
      fertilizer_type= r$fertiliser_type,
      irrigation_dates= r$Irrigation_months, 
      irrigation_method= r$irrigation_technique,
      irregation_source= r$Irrigation_source,
      irrigated= r$land_irrigated, 
      land_prep_method=r$tillage_power,
      index=r$index,
      fertilizer_constraint= r$constraint_fertilizers ## Not a carob variable
      
   )
   
   d$irrigated[d$irrigated=="No"] <- FALSE
   d$irrigated[d$irrigated=="Yes"] <- TRUE 
   d$irrigated[d$irrigated=="no_answer"] <- NA
   
   ## Fixing names
   p <- carobiner::fix_name(d$fertilizer_type)
   p <- gsub(" ","; ",p)
   p <- gsub("NPK15:9:20","NPK",p)
   p <- gsub("NPK17","NPK",p)
   p <- gsub("NPK23:10:5","NPK",p)
   p <- gsub("NPK5:7:5","NPK",p)
   p <- gsub("MOP","none",p)
   p <- gsub("NA","none",p)
   p <- gsub("40N-55S","none",p)
   p <- gsub("other","none",p)
   d$fertilizer_type <- p
   
   ## Fixing names
   ##CN
   ## irrigation method contains terms that are not in carob  
   p <- carobiner::fix_name(d$irrigation_method)
   p <- gsub("furrow can bucket","furrow",p)
   p <- gsub("can furrow hose","furrow",p)
   p <- gsub("bucket flooding","flooding",p)
   p <- gsub("can furrow","furrow",p)
   p <- gsub("can sprinkler furrow","sprinkler; furrow",p)
   p <- gsub("micro-basin hose","basin",p)
   p <- gsub("micro-basin","basin",p)
   d$irrigation_method <- p
   
   ### Process yield data 
   r1 <- carobiner::read.excel(f,sheet = "crop_repeat")
   r1<- r1[-1,] ## remove the first row of r1
   names(r1) <- gsub("_index","index",names(r1))
   d1 <- data.frame(
      crop= r1$crop_name,
      rep= as.integer(r1$crop_rep_number),
      season=r1$season_grown,
      yield= as.numeric(r1$crop_yield),
      crop_system= r1$crop_intercrop, ## Not a carob variable
      crop_price= as.numeric(r1$crop_sold_price),
      #OM_type= r1$crop_residue_use,
      index=r1$index
      
   )
   
   ## merge geo data and  yield data
   
   d <- merge(d,d1,by="index", all.x=TRUE)
   d$index <- NULL
   
   ### Fixing crop content ###
   C <- carobiner::fix_name(d$crop)
   C <- gsub("potatoIrish","potato",C)
   C <- gsub("potatoSweet","sweetpotato",C)
   C <- gsub("beansBush","common bean",C)
   C <- gsub("other1","none",C)
   C <- gsub("bananas","banana",C)
   C <- gsub("carrots","carrot",C)
   C <- gsub("eggplants","eggplant",C)
   C <- gsub("groundnuts","groundnut",C)
   C <- gsub("chickpeas" ,"chickpea",C)
   C <- gsub("beansClimbing" ,"common bean",C)
   C <- gsub("passionfruit" ,"passion fruit",C)
   C <- gsub("peas" ,"pea",C)
   d$crop <- C
   
   ### 
   d$land_prep_method[d$land_prep_method=="manual" | d$land_prep_method=="mechanical manual" | d$land_prep_method=="mechanical" | d$land_prep_method=="manual mechanical"
                      | d$land_prep_method=="manual 4wheel_tractor" | d$land_prep_method=="2wheel_tractor manual"] <- "conventional"
   
   ### Correct longitude and latitude errors
   
   geo<- data.frame(location=c("Cyanika", "Biziguro", "Muti", "Cyabirego"),
                    lon=c(30.0846653, 29.8954815, 30.0757, 29.1845603),
                    lat=c(-1.8682399, -1.9967811, -2.1074, -2.5889755))
   
   d <- merge(d,geo,by="location",all.x=TRUE)
   
   d$longitude[!is.na(d$lon)] <- d$lon[!is.na(d$lon)]
   d$latitude[!is.na(d$lat)] <- d$lat[!is.na(d$lat)]
   d$lon <- d$lat <- NULL
   
   message("yield is given in kg,bacs and tonnes instead of kg/ha")
   
   carobiner::write_files(dset, d, path=path)
}

# carob_script(path)
