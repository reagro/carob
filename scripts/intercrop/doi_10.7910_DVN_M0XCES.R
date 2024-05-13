# R script for "carob"


carob_script <- function(path) {
   "
   The dataset contains six datasheets with different types of data i.e., cropping system physiological characteristics, gross margins, soil nutrient characterization, 
   seasonal physical conditions of the soil, soil water infiltration and seasonal weather conditions. The data will be helpful in assessing how the different Climate 
   Smart Agricultural practices (CSA) affect crop physiological characteristics, yield, and soil physical attributes.
   
   "
   uri <- "doi:10.7910/DVN/M0XCES"
   dataset_id <- carobiner::simple_uri(uri)
   group <- "intercrop"
   ff <- carobiner::get_data(uri, path, group)
   
   dset <- data.frame(
      carobiner::read_metadata(uri, path, group, major=1, minor=2),
      project=NA, 
      publication= NA, 
      data_institutions = "IFPRI", 
      carob_contributor="Cedric Ngakou", 
      carob_date="2024-05-14", 
      data_type="Experiment"
   )
   
   
   ## process file(s) 
   
   ### yield data 
   r <- carobiner::read.excel.hdr(ff[basename(ff)=="03_Gross Margins_Trial_2_Data_2019.xlsx"],skip = 0)
   d00<- r[,c("Ecozone","Trial.type","Rep","Trt","Trt_Descr","Mz.Grain_Yld.t.ha","Mz.Stover_Yld.t.ha","Bn.Grain.Yld.t.ha","Bn.Haulm.Yld.t.ha","PP.Grain_Yld.t.ha","PP.Stalks_Yld.t.ha")]
   d0<- carobiner::change_names(d00,names(d00),c("location","trial","rep","Trt","treatment","yield_M","residue_yield_M","yield_B","residue_yield_B","yield_P","residue_yield_P"))
   d0$trial_id<- paste(d0$trial,d0$Trt,sep ="_")
   
  ## create a long data set 
   vec=c("_M","_B","_P")
   d<- data.frame()
   for (i in vec){
     new_name<- gsub(i,"",names(d0)) 
     dC<- carobiner::change_names(d0,names(d0),new_name)
     dm<- dC[,c("location","trial_id","rep","treatment","yield","residue_yield")]
     dm$crop<- i
     dM<-rbind(d,dm)
     d<- dM
   }
   
   d$yield<- d$yield*1000 # in kg/ha
   d$residue_yield<- d$residue_yield*1000 # in kg/ha
   ## remove NA in yield data
   d<- d[!is.na(d$yield),]
    ## Soil data 
   r1 <- carobiner::read.excel.hdr(ff[basename(ff)=="05_Soil_Characterization_Trial_2_Data_2019.xlsx"],skip = 0)
   d11 <-r3[,c("Ecozone","Reps","pH","X.EC.Salts.uS.cm","X.Phosphorus.Olsen.ppm","Potassium.ppm","Calcium.ppm","Magnesium.ppm","Sulphur.ppm","Copper.ppm","Boron.ppm","Zinc.ppm","Iron.ppm","X.Sodium.ppm","X.C.E.C.meq.100g")] 
   d1<- carobiner::change_names(d11,names(d11),c("location","rep","soil_pH","soil_EC","soil_P_available","soil_K","soil_Ca","soil_Mg","soil_S","soil_Cu","soil_B","soil_Zn","soil_Fe","soil_Na","soil_CEC"))
   
   # merge soil data and yield data
   
   d<- merge(d,d1,by=c("location","rep"),all.x = TRUE)
   
   #Fix crop column
   P<- carobiner::fix_name(d$crop)
   
   P<- gsub("_M","maize",P)
   P<- gsub("_B","common bean",P)
   P<- gsub("_P","pigeon pea",P)
   d$crop<- P
   ## Add columns
   d$intercrops[d$crop=="maize"] <-"common bean; pigeon pea" 
   d$intercrops[d$crop=="common bean"] <-"maize; pigeon pea"
   d$intercrops[d$crop=="pigeon pea"] <-"common bean; maize"
   
   d$country <- "Tanzania"
   d$dataset_id<- dataset_id
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$irrigated <- FALSE
   d$inoculated <- FALSE
   ## get from data description 
   d$variety<- ifelse(d$crop=="maize","Meru 513",
                      ifelse(d$crop=="pigeon pea","Karatu","jesica")) 
   
   d$N_fertilizer<- 50
   d$P_fertilizer <- 20 
   d$K_fertilizer <- 0
   ## Add longitude and latitude 
   geo<- data.frame(location=c("Gallapo","Sabilo" ),longitude=c(35.8524656,35.4766004),latitude=c(-4.2829804,-4.345829))
   d <- merge(d,geo,by="location",all.x = TRUE)
   d$yield_part <- "grain" 
   ## add long and lat
   
   d$planting_date <- "2019"
  ## data type
   d$rep<- as.integer(d$rep)
   
   carobiner::write_files(dset, d, path=path)
   
}


