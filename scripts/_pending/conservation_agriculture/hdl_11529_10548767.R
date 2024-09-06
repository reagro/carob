# R script for "carob"


carob_script <- function(path) {

"A field experiment was conducted in maize under medium-term conservation agriculture (CA) based maizewheat system at BISA-CIMMYT, Ladhowal, Punjab during kharif 2019 to assess the effect of CA+ practices (CA with sub-surface drip irrigation) with variable N doses on maize. The CA+ treatments were residue retained (WR) permanent bed (PB) with sub-surface drip fertigation (PB-SSD): without N (N0), 120 kg N/ha,150 kg N/ha applied in 4-equal (Eq) and differential splits (Df); CA alone treatment includ PB furrow irrigation with 120 kg N/ha (PBWRFurrow- N120); conventional tillage (CT) involved furrow irrigation with 120 kg N/ha (CTWOR-Furrow-N120) and other treatments were residue removed (WOR) PB: PBWOR-without N (N0), with 120 kg N/ha, and 150 kg N/ha applied in four Eq-splits and Df-splits. The findings of the present experiment showed that the numerical value of yield attributing characters were higher under CA+ plots as compared to CA alone (PBWR-Furrow-N120) and CT (CTWOR-Furrow-N120). Biological yield of maize was significantly higher in all CA+ plots as compared to CA alone and CT plots. Highest biological yield was recorded under PBWR-SSD-N150 Df (23.45 t/ha). Highest no. of cobs (72800/ha), no. of grains/cob (605) and cob length (22.61cm) along with dry matter resulted highest biological yield in PBWR-SSD-N150 plots. The grain N content remained statistically similar across all the N management plots, but in case of total N uptake, PBWR-SSD-N150 Df (CA+) plots dominated due to higher biomass. Besides, CA+ based PBWR-SSD-N120 (average of Df and Eq) registered 23-24% higher total N uptake than CA alone (PBWRFurrow- N120) and conventional (CTWOR-Furrow-N120) plots. Improved agronomic N use-efficiency was also recorded under CA+ plots as compared to CA alone (36.4 kg/kg N) and CT (36.7 kg/kg N) plots. (2021-02-12)"

  uri <- "hdl:11529/10548767"
  group <- "agronomy"
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- data.frame(
    carobiner::read_metadata(uri, path, group, major=1, minor=3),
    data_institute="CIMMYT",
    publication='doi:10.56093/ijas.v91i3.112544',
    project=NA,
    data_type = "on-farm experiment",
    response_vars = "yield",
    treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
    carob_contributor="Hope Mazungunye",
    carob_date="2023-09-15",
    modified_by="Siyabusa Mkuhlani",
    last_modified = "2024-09-06"
  )
  
  #Process sheet: Yield and Attributing Character
  bn <- basename(ff)
  r1<- carobiner::read.excel(ff[bn=="IJAS_Table_Replicated_Data.xlsx"], sheet= "Yield and Attributing Character")
  d1<-r1[-c(2:19)] #Selecting columns of interest, as some parameters cannot be processed.
  
  #Added new column names, as they get mixed up when data set is read. Some of the cells in the raw data set are merged.
  colnames(d1)<-c("treatment", "R1_dmy_total", "R2_dmy_total", "R3_dmy_total", "R1_harvest_index", "R2_harvest_index", "R3_harvest_index")
  
  d1<-d1[-1,] #Row is now redundant. 
  
  d1<-reshape(d1,
              varying=c("R1_dmy_total", "R1_harvest_index",
                        "R2_dmy_total", "R2_harvest_index",
                        "R3_dmy_total", "R3_harvest_index"),
              v.names=c("R", "H"),
              timevar="Rep",
              times=c("1", "2","3"),
              direction='long')
  
  colnames(d1)<-c("treatment","rep","harvest_index","dmy_total", "trial_id")
  d1$trial_id<-as.character(paste0(d1$treatment,'_',d1$rep,'_',d1$trial_id))
  
  #process yield values
  d1[, c("dmy_total", "harvest_index")] <-lapply(d1[, c("dmy_total", "harvest_index")], as.numeric)
  d1$yield_part <- "grain"
  d1$dmy_total<-d1$dmy_total*1000
  d1$yield<-d1$dmy_total*(d1$harvest_index/100) #No grain yield value but can be extrapolated using Harvest index.
  d1$dmy_residue<-d1$dmy_total-d1$yield
  
  #Management data 
  d1$land_prep_method<-NA
  d1$land_prep_method<-ifelse(grepl("PB",d1$treatment),"raised beds","conventional")
  d1$residue_prevcrop_used<-ifelse(grepl("WR",d1$treatment),TRUE,FALSE)
  d1$irrigated<-TRUE
  d1$irrigation_method<-ifelse(grepl("SSD",d1$treatment),"sub-surface drip","furrow")
  
  d1$fertilizer_used<-ifelse(grepl("N0",d1$treatment),FALSE,TRUE) 	#fertilizer information (from the data and the paper)
  d1$fertilization_method<-ifelse(grepl("N0",d1$treatment),NA,"fertigation")
  
  d1$P_fertilizer<- 60*0.346  #Basal applied. 60kg/ha p205 & 30kg/ha k2O
  d1$K_fertilizer<- 30*0.831  #Basal applied. 60kg/ha p205 & 30kg/ha k2O
  d1$N_fertilizer<-NA
  d1$N_fertilizer<-ifelse(grepl("N0",d1$treatment),0,d1$N_fertilizer)
  d1$N_fertilizer<-ifelse(grepl("N120",d1$treatment),96.5,d1$N_fertilizer)
  d1$N_fertilizer<-ifelse(grepl("N150",d1$treatment),126,d1$N_fertilizer)
  d1$N_splits<-NA
  d1$N_splits<-ifelse(grepl("PBWOR-SSD",d1$treatment),4,d1$N_splits)
  d1$N_splits<-ifelse(grepl("PBWR-SSD",d1$treatment),4,d1$N_splits)
  d1$N_splits<-ifelse(grepl("CTWOR-Furrow",d1$treatment),2,d1$N_splits)
  d1$N_splits<-ifelse(grepl("PBWR-Furrow",d1$treatment),2,d1$N_splits)
  d1$N_splits<-as.integer(d1$N_splits)
  
  #Additional details
  d1$rep<-as.integer(d1$rep)
  d1$geo_from_source <- TRUE
  d1$crop<- "maize"
  d1$on_farm <- FALSE
  d1$is_survey <- FALSE
  d1$irrigated <- TRUE
  d1$country <- "India" 
  d1$adm1<-"Punjab"
  d1$adm2<-"Ludhiana"
  d1$adm3<-"Ludhiana West"
  d1$location<-"Ladhowal"
  d1$site<-'bisa-cimmyt'
  d1$longitude <- 75.44
  d1$latitude <- 30.99
  d1$elevation <- 229
  d1$planting_date<- '2019/06/20'
  d1$harvest_date<- '2019/10/13'
  d1$variety<- "P3396"
  
  
  #Process sheet: N Data (NUE, Uptake)
  r2<- carobiner::read.excel(ff[bn=="IJAS_Table_Replicated_Data.xlsx"], sheet= "N Data (NUE, Uptake)")
  d2<-r2[c(1:7)] #Selecting columns of interest, as some parameters cannot be processed.
  
  #Added new column names, as they get mixed up when data set is read. Some of the cells in the raw data set are merged.
  colnames(d2)<-c("treatment", "R1_grain_N", "R2_grain_N", "R3_grain_N", "R1_leaf_N", "R2_leaf_N", "R3_leaf_N")
  
  d2<-d2[-1,] #Row is now redundant. 
  
  d2<-reshape(d2,
              varying=c("R1_grain_N", "R1_leaf_N",
                        "R2_grain_N", "R2_leaf_N",
                        "R3_grain_N", "R3_leaf_N"),
              v.names=c("grain_N", "leaf_N"),
              timevar="Rep",
              times=c("1", "2","3"),
              direction='long')
  
  colnames(d2)<-c("treatment","rep","grain_N","leaf_N", "trial_id")
  d2$trial_id<-as.character(paste0(d2$treatment,'_',d2$rep,'_',d2$trial_id))
  d2[, c("grain_N", "leaf_N")] <-lapply(d2[, c("grain_N", "leaf_N")], as.numeric)
  d<-merge(d1,d2,by='trial_id')
  d <- d[, !names(d) %in% c("treatment.y", "rep.y")]
  names(d)[names(d) == "treatment.x"] <- "treatment"
  names(d)[names(d) == "rep.x"] <- "rep"
  
  carobiner::write_files(meta, d, path=path)
}
