
carob_script <- function(path) {
   
"Experiments were installed in La Libertad, with the objective of identifying clones with high potential for being varieties applying the Participatory Varietal Selection methodology. For the period 2016-2017, 18 clones with high resistance to late blight were planted, belonging to the B population and developed in the International Potato Center together with Two control varieties, Amarilis and Yungay (susceptible). Finally, in the harvest 5 clones with high yield, low glycoalkaloid content and good organoleptic quality were selected as a result of the Participatory Variety Selection of the farmers and the analysis of mixed models and BLUPs for the yield data. The 5 selected clones were planted again in the period 2017-2018 and through the Participatory Varietal Selection three promising clones were selected (CIP308488.92, CIP308495.227 and CIP308478.59).
"
   uri <- "doi:10.21223/GZI7PD"
   group <- "potato_trials"
   ff <- carobiner::get_data(uri, path, group)
  
   dset <- data.frame(
   	carobiner::read_metadata(uri, path, group, major=1, minor=2),
      publication= NA,#"DOI:10.1007/s11540-021-09495-z",
      data_institutions = "CIP",
      carob_contributor="Cedric Ngakou",
      data_type="experiment",
      project=NA,
      carob_date="2023-12-09"
   )
   
   
   ff <- ff[grep("PTPVS", basename(ff))]
   process <- function(f) {
      r <- carobiner::read.excel(f, sheet="F4_harvest_mother")
      r <- r[, c("REP", "INSTN", "TTYNA")]
      colnames(r) <- c("rep", "variety", "yield")
      r$location<- basename(f)
      r
   }

   d <- lapply(ff, process) 
   d <- do.call(rbind, d)
   d$rep <- as.integer(d$rep)
   d$yield <- d$yield * 1000 ## kg/ha
   
 ## most of the information is coming from DOI:10.1007/s11540-021-09495-z
   i1<-grepl("ARCOPAMPA",d$location)
   d$location[i1]<- "Arcopampa"
   d$season[i1]<- "2017-2018"
   i1<-grepl("LSOLE",d$location)
   d$location[i1]<- "La Soledad"
   d$season[i1]<- "2016-2017"
   i1<-grepl("MACULL",d$location)
   d$location[i1]<- "Macullida"
   d$season[i1]<- "2016-2017"
   i1<-grepl("AURORITA",d$location)
   d$location[i1]<- "La Aurorita"
   d$season[i1]<- "2017-2018"
   d$row_spacing<- 100  
   d$plant_spacing<- 30 
   d$harvest<- 120
   ## add columns
   
   d$country <- "Peru"
   d$adm1<- "La Libertad"
   d$adm2<- "Sanchez Carrion" # from DOI:10.1007/s11540-021-09495-z
   d$trial_id <- paste(d$location, d$variety, sep = "_")
   d$irrigated <- FALSE
   d$inoculated <- FALSE
   d$is_survey <- FALSE
   d$on_farm <- TRUE
   d$crop <- "potato"
   d$yield_part <- "tubers" 
   ## add lon and lat
   geo<- data.frame(location=c("Arcopampa","La Soledad","Macullida","La Aurorita"),
                    longitude=c(-77.89139,-77.87619,-77.883,-77.87444),
                    latitude=c(-7.976944,-7.923611,-7.913889,-7.829167))
   
   d<- merge(d,geo,by="location",all.x = TRUE)
   
   d$planting_date<- "2016" 
   d$planting_date[d$season=="2017-2018"]<- "2017"
   carobiner::write_files(dset, d, path=path)
   
}


