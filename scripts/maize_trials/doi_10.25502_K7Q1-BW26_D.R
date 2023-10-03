

carob_script <- function(path) {
   
   "
	Description:
	Availability of multiple-stress tolerant maize is critical for improvement in maize production in West and Central Africa (WCA). A study was carried out to (i) assess a set of inbred lines for combining ability under stressed and optimal conditions, (ii) determine the performance of the testcrosses under different conditions, and (iii) identify outstanding hybrids across the conditions. Two hundred and five testcrosses were planted with five hybrid checks under Striga-infested, low soil nitrogen, drought and optimal conditions between 2015 and 2016 in Nigeria.
	The grain yield inheritance under optimal condition was largely regulated by additive gene effect whereas non-additive gene effects largely regulated grain yield under the three stresses. Four of the inbreds had significant positive general combining ability effects each under low N and drought, and three under Striga infestation for grain yield. The inbreds could be vital sources of beneficial alleles for development and improvement of tropical yellow maize hybrids and populations. Hybrids TZEI 443 x ENT 13 and TZEI 462 x TZEI 10 were high yielding and stable; 
	they out-performed the three early maturing released hybrids in WCA. The new hybrids should be extensively assessed and released in the sub-region to improve food security.
	
"
   
   uri <-  "doi:10.25502/K7Q1-BW26/D"
   dataset_id <- carobiner::simple_uri(uri)
   group <- "maize_trials" 
   ## dataset level data 
   dset <- data.frame(
      dataset_id = dataset_id,
      group=group,
      uri=uri,
      publication= NA,#DOI: 10.1038/s41598-019-50345-3
      data_citation ="BADU-APRAKU, B., Annor, B., Nyadanu, D., Akromah, R., & Fakorede, M. A. B. (2020). Evaluation of testcrosses involving early maturing yellow maize inbred lines and elite testers [dataset]. International Institute of Tropical Agriculture (IITA).
      https://doi.org/10.25502/K7Q1-BW26/D",
      data_institutions = "IITA",
      carob_contributor="Cedric Ngakou",
      data_type="experiment",
      project=NA 
   )
   
   ## download and read data 
   ff <- carobiner::get_data(uri, path, group)
   js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=3)
   dset$license <- carobiner::get_license(js)
   
   bn <- basename(ff)
   
   # read the dataset
   r<- read.csv(ff[bn=="BA15188 ACROSS.csv"])  
   
   d<- r[,c("ID","LOC","Rep","Entry","Pedigree","YIELD","PLST","POLLEN","DYSK","ASI","PLHT","EHT","PASP","PHARV","EHARV","EROT","FWT","CO1","CO2","RAT1","RAT2")]#
     colnames(d)<- c("ID","location","rep","variety_code","variety","yield","pl_st","dy_poll","dy_sk","asi","pl_ht","e_ht","p_asp","p_harv","e_harv","e_rot","fwtun","str_co1","str_co2","str_rat1","str_rat2")#,
   
   
   # add columns
   d$country<- "Nigeria"
   d$crop <- "maize" 
   d$dataset_id <- dataset_id
   d$trial_id <- paste(d$ID,d$location,sep = "-")
   d$yield_part<- "grain" 
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$irrigated <- FALSE
  d$borer_trial <- FALSE
   d$striga_infected <- FALSE
   d$ID<- NULL
   d$treatment <- d$location
   d$striga_trial <- FALSE
   i<-grepl("STR",d$treatment)
   d$striga_trial <- TRUE
   #add season 
   d$season<- "2015"
   d$planting_date<- "2015"
   j<- grepl("IK16OPT" ,d$location)|  grepl("MK16STR",d$location) |  grepl("AB16STR",d$location) |  grepl( "MK16LN"  ,d$location) |grepl("MK16OPT" ,d$location)|grepl("AB16OPT" ,d$location)
   d$season[j]<- "2016"
   d$planting_date[j]<- "2016"
   j<-grepl("KDW16DT" ,d$location) 
   d$season[j]<- "2015-2016"
   d$planting_date[j]<- "2015"
   j<-grepl("IK15DT" ,d$location) | grepl("MI15DT",d$location)
   d$season[j]<- "2014-2015"
   d$planting_date[j]<- "2014"
# fix location 
   j<- grepl("MI15DT",d$location)
   
   d$location[j]<- "Minjibir"
   
   j<- grepl("IK15DT" ,d$location)|  grepl("IK15OPT",d$location) |  grepl("IK16OPT",d$location)
   d$location[j]<- "Ikenne"
   
   j<- grepl("KDW16DT",d$location)
   d$location[j]<- "Kadawa"
   
   j<- grepl("MK15LN" ,d$location)|  grepl("MK16LN",d$location) |  grepl("MK15STR",d$location) |  grepl("MK16OPT",d$location)|  grepl("MK16STR" ,d$location)|  grepl("MK15OPT"  ,d$location)
   d$location[j]<- "Mokwa"
   
   j<- grepl("IF15LN",d$location) | grepl("IF15HN",d$location)
   d$location[j]<- "Ile-Ife"
   
   j<- grepl("AB15STR",d$location) | grepl("AB16STR",d$location) | grepl("AB15OPT",d$location) | grepl("AB16OPT",d$location)
   d$location[j]<- "Abuja"
   ### add long and lat coordinate
   Geo<- data.frame(location=c("Minjibir","Ikenne","Kadawa","Mokwa","Ile-Ife","Abuja"),
                    lat=c(12.1771,6.9010051,11.6331619,11.6331619,6.760397,9.0643305),
                    lon=c(8.65866,3.6977469,8.4340146,8.4340146,6.206915,7.4892974))
   
   d<- merge(d,Geo,by="location")
   d$longitude <- d$lon
   d$latitude <- d$lat
   d$lon <- d$lat <- NULL
   # fill whitespace in str_co1, str_rat1, str_rat2
   d$str_co1[d$str_co1==""]<- NA
   d$str_rat1[d$str_rat1==""]<- NA
   d$str_rat2[d$str_rat2==""]<- NA
   #data type
   d$variety_code<- as.character(d$variety_code)
   

   # all scripts must end like this
   carobiner::write_files(dset, d, path=path)
   
}


