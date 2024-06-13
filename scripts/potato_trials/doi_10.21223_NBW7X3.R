# R script for "carob"

carob_script <- function(path) {
   
"Evaluation of yield under late blight and viral disease presence, in Kenya. In the first experiment,16 clones were evaluated under RCBD with 3 repetitions. In the second experiment, 32 clones were evaluated in RCBD with 3 reps. Both trials were established at University of Nairobi, Kabete Campus, Nairobi-Kenya (1800 m a.s.l.). All late blight evaluations were performed 30, 40, 50, 60, 70, 80 and 90 days after planting. The plots were set with a distance of 0.75 m between rows and 0.20 m plants"

   uri <- "doi:10.21223/NBW7X3"
   group <- "lateblight"
   ff  <- carobiner::get_data(uri, path, group)
   
   dset <- data.frame(
      carobiner::read_metadata(uri, path, group, major=2, minor=0),
      data_institute = "CIP",
      publication= NA,
      project=NA,
      data_type= "experiment",
      treatment_vars = "variety_code",
      carob_contributor= "Cedric Ngakou",
      carob_date="2024-06-12"
   )
   
   r <- carobiner::read.excel(ff[basename(ff)=="12136_PTYield062018_UNINAIROBI_exp1.xlsx"],sheet = "Fieldbook")
   
   d <- data.frame(      
      variety_code= r$INSTN,
      rep= as.integer(r$REP),
      yield=r$TTYNA*1000,# in kg/ha
      AUDPC= r$AUDPC,
      rAUDPC= r$rAUDPC,
      planting_date="2018-06-04" ## from data description
   )
   
	d1 <- lapply(c("2018-07-17_Fieldbook_Trial1", "2018-07-22_Fieldbook_Trial2"), 
		\(i) { 
			r1 <- carobiner::read.excel(ff[basename(ff)=="caf1cc988f3c58c6cb1af73bed6ca249.xlsx"], sheet=i)
			names(r1) <- gsub("CloneID","Clones",names(r1))
			dd <- data.frame( 
				variety_code=r1$Clones,
				rep= as.integer(r1$Rep),
				flowering_date= as.character(r1$`Flowering Date`),
				AUDPC=r1$AUDPC,
				rAUDPC=r1$rAUDPC,
				planting_date= as.character(r1$Planted),
				yield=r1$Yield*1000, # in kg/ha
				trial_id = i
			)
	   }
   )
   
   d1 <- do.call(rbind, d1)
   
   d <- carobiner::bindr(d1,d)
   
   ### Add more variables 
   d$country <- "Kenya"
   d$adm1 <- "Nairobi"
   d$location <- "Kabete"
   d$site <- "University of Nairobi"
   d$latitude <- -1.25438889
   d$longitude <- 36.729999
   d$elevation <- 1800
   d$crop <- "potato"
   d$diseases <- "potato late blight"
   d$row_spacing <- 75  ## from data description 
   d$plant_spacing <- 30 
   d$plant_density <- 44444.44 # number of plant per ha 
   d$on_farm <- TRUE
   d$irrigated <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "tubers"
  
   carobiner::write_files(path, dset, d)   
}

