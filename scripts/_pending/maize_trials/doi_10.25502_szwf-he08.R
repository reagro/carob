# R script for "carob"

## ISSUES


carob_script <- function(path) {
   
"
Description
Yield gains and associated changes in an early yellow bi-parental maize population following Genomic Selection for Striga resistance and drought tolerance.
	
"
   ## Process 
   uri <- "doi:10.25502/SZWF-HE08"
   dataset_id <- carobiner::simple_uri(uri)
   group <- "maize_trials"
   dataset_id <- dataset_id
   ## dataset level data 
   dset <- data.frame(
      dataset_id = dataset_id,
      group=group,
      uri=uri,
      data_citation="Badu-Apraku Baffour, R. Asiedu, A.O. Talabi, M.A.B. Fakorede, Y. Fasanmade, M. Gedil, & C. Magorokosho. (2018). Yield gains and associated changes in an early yellow bi-parental maize population following Genomic Selection for Striga resistance and drought tolerance [dataset]. International Institute of Tropical Agriculture (IITA).
      https://doi.org/10.25502/SZWF-HE08",
      publication=NA,
      carob_contributor="Cedric Ngakou",
      carob_date="2023-11-25",
      data_type="Experiment",
      project=NA,
      data_institutions="IITA"
   )

   ## download and read data 
   ff <- carobiner::get_data(uri, path, group)
   js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
   dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)
   
   bn <- basename(ff)
   
   ## process file(s)
   bN<- bn[grepl("E",bn)]
  
   process<- function(f){
      r <- read.csv(ff[bn==f])
      p<- carobiner::fix_name(names(r))
      p<- gsub("PEDIGREE","Pedigree",p)
      p<- gsub("SL_perc","SL",p)
      p<- gsub("RL_perc","RL",p)
      p<- gsub("HUSK","HC",p)
      p<- gsub("Rep","REP",p)
      names(r)<- p
      d <- r[,c("YEAR","LOC","Pedigree","REP","YIELD","ASI","PLHT","EHT","HC","EASP","E_ROT","POLLEN","DYSK","SL","RL")]
      colnames(d)<- c("planting_date","location","variety","rep","yield","asi","pl_ht","e_ht","husk","p_asp","erot","dy_poll","dy_sk","sl","rl")
      d$treatment<- f
      ## remove .CSV in treatment
      d[c('treatment',"csv")] <- str_split_fixed(d$treatment, '.csv',2)
      d$csv<- NULL
      d$N<- 1
      i<- grepl("KDWHS",d$location)| grepl("KADAWA",d$location)
      d$location[i]<- "kadawa"
      d$N[i]<- 2
      d$location[d$location=="IKDS"]<- "Ikenne"
      d$planting_date<- as.character(d$planting_date)
      
      d
   }
   
   dd<- lapply(bN,  process)
   d<- do.call(rbind,dd)
   ## add columns
   d$country <- "Nigeria"
   d$crop <- "maize" 
   d$dataset_id <- dataset_id
   d$trial_id <- paste(d$location,d$variety,sep = "_")
   d$yield_part <- "grain"
   d$on_farm <- TRUE
   d$irrigated <- FALSE
   d$borer_trial <- FALSE
   d$striga_infected <- FALSE
   d$striga_trial <- TRUE
   
## add long and lat coordinate 
   d$longitude<- c(3.71051, 8.48973)[d$N] 
   d$latitude<- c(6.87174, 12.07353)[d$N]
   d$N<- NULL
 
   carobiner::write_files(dset, d, path=path)
}


