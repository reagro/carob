

carob_script <- function(path) {
   
"The database contains data about on-farm trials with transplanted rice were conducted during monsoon ('Aman') season in 2016 and 2017 and winter ('Boro') season in 2016 to 2017 in agroecological zones (AEZs) 11 and 12 of south-west Bangladesh with ten treatments - seven herbicide-based IWM options, one mechanical weed control-based option, and two checks – farmers' current weed control practice and weed-free, to assess effects on weed control, grain yield, labor use, and profitability. (2021-07-09)"
   
   uri <-  "hdl.handle.net/11529/10548600"
   dataset_id <- carobiner::simple_uri(uri)
   group <- "rice_trials" 
   ff <- carobiner::get_data(uri, path, group)
   js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
   ## dataset level data 
   dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
      publication= NA,
      data_institutions = "CIMMYT",
      carob_contributor="Cedric Ngakou",
      carob_date="2023-09-27",
      data_type="experiment",
      project=NA 
   )
   
   
   bn <- basename(ff)
   
   # read the dataset
   r <- read.csv(ff[bn=="Aman_weed_data.csv"]) 
   r1 <- read.csv(ff[bn=="Aman_Yield_and_economics.csv"]) 
   
   r2 <- read.csv(ff[bn=="Boro_weed_data.csv"]) 
   r3 <- read.csv(ff[bn=="Boro_yield_and_economics-1.csv"])
   
   ### process Aman_weed_data file()
   
   dd <- r[,c("Season","Replication","Treatments","SITE","TWbS")]
   colnames(dd) <- c("season","rep","Treatment","site","weed_biomass")
   
   treat <- data.frame( treatment=c("Farmers practices + pretilochlor fb two HW","pendimethalim fb HW","Mefenacet + bensulfuron fb HW","Mefenacet + bensulfuron fb MW","Bispyribac-sodium fb HW","penoxsulan fb HW","Mefenacet+bensulfuron fb bispyribac-sodium fb HW",
             "Mefenacet + ben sulfuron fb penoxulam fb HW","MW fb HW","Weed-free by frequent HW"), 
             Treatment=c(1,2,3,4,5,6,7,8,9,10))
   dd <- merge(dd,treat,by="Treatment")
   dd$Treatment <- NULL
   ### process Aman_Yield_and_economics file()
   dd1 <- r1[,c("Season","Replication","Treatment","SITE","Grain.yield..t.ha.")]
   colnames(dd1) <- c("season","rep","Treatment","site","yield")
   dd1 <- merge(dd1,treat,by="Treatment")
   dd1$Treatment <- NULL
   ####
   ## merge dd and dd1 
   d1 <- merge(dd,dd1, by=c("season","treatment","site","rep"))
   d1$variety <- "BRRI dhan49"
   d1$planting_date <- "2016-07-15"
   ### process Boro_weed_data file()
   dd2 <- r2[,c("Season","Replication","Treatment","Site","TWbS")]
   colnames(dd2) <- c("season","rep","Treatment","site","weed_biomass")
   
   
   dd2 <- merge(dd2,treat,by="Treatment")
   dd2$Treatment <- NULL
   
   ### process Boro_yield_and_economics-1 file()
   dd3 <- r3[,c("Season","Replication","Treatment","SITE","Grain.yield..t.ha.")]
   colnames(dd3) <- c("season","rep","Treatment","site","yield")
   dd3 <- merge(dd3,treat,by="Treatment")
   dd3$Treatment <- NULL
   ## merge dd2 and dd3 
   d2 <- merge(dd2,dd3, by=c("season","treatment","site","rep"))
   d2$variety <- "BRRI dhan28"
   d2$planting_date <- "2016-12-10"
   #### joint d1 and d2
   d <- rbind(d1,d2)
   
   d$yield <- d$yield*1000  ## kg/ha
   d$weed_biomass <- d$weed_biomass*10 # kg/ha 
   # add columns
   d$country <- "Bangladesh"
   d$crop <- "rice" 
   d$dataset_id <- dataset_id
   d$trial_id <- paste(d$site,d$dataset_id,sep = "-")
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$irrigated <- FALSE
   
   ### add long and lat coordinate
   
   d$longitude[d$site=="Faridpur"] <- 89.4427491
   d$latitude[d$site=="Faridpur"] <- 24.1606327
   d$longitude[d$site=="Jashore" ] <- 89.2094419
   d$latitude[d$site=="Jashore" ] <- 23.1665256
   
   d$yield_part <- "grain" 
   
   carobiner::write_files(dset, d, path=path)
   
}


