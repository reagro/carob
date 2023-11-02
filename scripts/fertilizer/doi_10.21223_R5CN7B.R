

carob_script <- function(path) {
   
   "
	Description:
	The purpose of this study was to estimate the genetic gains for yield and quality traits in sweetpotatoes. The field evaluation was conducted in San Ramon 2016, 2018A, 2018B; Huaral 2016, 2019A, 2019B; Ica 2016, 2019A, 2019B and Satipo 2016, 2018A, 2018B evaluating 17 clones (Abigail, Adriano, Alexander, Arne, Atacama, Benjamin, Caplina, Costanero, Huambachero, INA-100, Isabel, Jonathan, Milagrosa, PZ06.120, Sumi, Tacna, Yarada) and 3 checks (Cemsa, Dagga, Salyboro). 
	Each of the trials was harvested at 90 and then 120 days.
"
   
   uri <-  "doi:10.21223/R5CN7B"
   dataset_id <- carobiner::simple_uri(uri)
   group <- "fertilizer"
   ## dataset level data 
   dset <- data.frame(
      dataset_id = dataset_id,
      group=group,
      uri=uri,
      publication= NA,# 
      data_citation ="Gruneberg, Wolfgang; Eyzaguirre, Raul; Diaz, Federico, 2021, Dataset for: Modified Demonstration Trials for Genetic Gain Data for Yield and Quality Traits,
      https://doi.org/10.21223/R5CN7B, International Potato Center, V1, UNF:6:JC+l2q8p2lvvocGYZ0351A== [fileUNF]",
      data_institutions = "CIP",
      carob_contributor="Cedric Ngakou",
      carob_date="2023-11-02",
      data_type="experiment",
      project=NA 
   )
   
   ## download and read data 
   ff <- carobiner::get_data(uri, path, group)
   js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
   dset$license <- carobiner::get_license(js)
   
   bn <- basename(ff)
   
   # read file
   r <- carobiner::read.excel(ff[bn=="Genetic_Gain_Trials_Peru_Cost_Amazon_90_120_days_full_data_.xlsx"])  
   
   d<- r[,c("trial_name","planting_date","season","loc","cipno","geno","harvest","rep","rytha","fe","zn","ca","mg","bytha","fytha")]
   colnames(d)<- c("trial_id","planting_date","season","location","variety","treatment","harvest","rep","yield","leaf_Fe","leaf_Zn","leaf_Ca","leaf_Mg","residue_yield","biomass_leaves")
   
   d$yield<- d$yield*1000 # in kg/ha
   
   ##CN
### The unit of residue_yield and biomass_leaves is unclear: it is given in tons/ha, but after conversion to kg/ha, almost all values are  out of bounds.
   #d$residue_yield<- d$residue_yield*1000 # in kg/ha
   #d$biomass_leaves<- d$biomass_leaves*1000
   d<- d[!is.na(d$yield),] ## remove NA in yield
   ## add columns
   d$crop <- "sweetpotato" 
   d$dataset_id <- dataset_id
   d$country <- "Peru"
   d$yield_part <- "tubers"
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$irrigated <- FALSE
   ##Add fertilizer
   d$N_fertilizer <- 0
   d$P_fertilizer <- 0
   d$K_fertilizer <- 0
   ### add long and lat coordinate
   geo<- data.frame(location=c("San Ramon","Huaral","Ica","Satipo"),
                  lon=c(-78.4544042,-76.916667,-75.499722,-74.1181641),
                  lat=c(-5.9768774,-11.25,-14.3325,-11.5538237))
   ## fix name
   p<- carobiner::fix_name(d$harvest)
   p<- gsub("90 days",90,p)
   p<- gsub("120 days",120,p)
   d$harvest <- p
   d$planting_date<- gsub("'","",d$planting_date)
   d <- merge(d,geo,by="location",all.x = T)
   d$longitude<- d$lon
   d$latitude<- d$lat  
   d$lon <- d$lat <- NULL
   #data type
   d$rep <- as.integer(d$rep)
   d$harvest <- as.numeric(d$harvest)
   d$harvest_date<- as.character(as.Date(d$planting_date) +d$harvest)
   # all scripts must end like this
   carobiner::write_files(dset, d, path=path)
   
}


