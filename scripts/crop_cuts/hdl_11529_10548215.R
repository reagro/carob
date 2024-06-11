# R script for "carob"


carob_script <- function(path) {
  
 
"Crop cut survey in 2015 conducted by EIAR and CIMMYT. 
  Replicated crop cuts of 16m2 in farmers fields along with additional data on 
  nutrient use and variety, and soil sample. (2015)
  "
  
uri <- "hdl:11529/10548215"
dataset_id <- carobiner::simple_uri(uri)
group <- "crop_cuts"
ff  <- carobiner::get_data(uri, path, group)



#Metadata
dset <- data.frame(
  dataset_id = dataset_id,
  group=group,
  uri=uri,
  project="TAMASA",
  publication="doi:10.5897/AJAR2019.14338",
  data_institutions = "EIAR and CIMMYT",
  data_type = "survey",
  treatment_vars = "none", 
  carob_contributor="Samar Attaher",
  carob_date="2024-06-07"
  )

#Download data
  
ff  <- carobiner::get_data(uri, path, group)
js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
dset$license <- carobiner::get_license(js)

 
f <- ff[basename(ff) == "ET_Baseline_EIAR_2015.xls"] 	
r <- carobiner::read.excel(f, sheet = "Revised_data")


d <- data.frame(
  country = "Ethiopia",
  latitude=r$Latitude,
  longitude=r$Longitude,
  elevation=r$Altitude,
  crop = "maize",
  yield_part = "grain",
  farmer_gender=r$Gender,
  variety_type=r$Type.of.variety,
  variety=r$Name.of.variety,
  plot_area= 16, #16 m2
  fertilizer_used=r$Fertilizeruse,
  OM_used=r$Fertilizer.type.organic,
  fertilizer_inorganic_used=r$Fertilizer.type.inorganic,
  fertilizer_amount <- as.numeric(r$amount.of.Inorganic.fertilizer),
  OM_amount=as.numeric(r$Org_fert_qty)
    )

d$adm1<- ifelse(d$latitude>10 & d$latitude<12, "Amhara",
                ifelse(d$latitude==9.696433576, "Beneshangul Gumu",
                       ifelse(d$latitude>7.3 & d$latitude<10, "Oromia", "SNNPR")))


r[,c(22,33,44)][r[,c(22,33,44)]==""]<-NA

r[,22]<-as.numeric(r[,22])
r[,33]<-as.numeric(r[,33])
r[,44]<-as.numeric(r[,44])


d$yield<-rowMeans(r[,c(22,33,44)],na.rm = TRUE)
d$soil_N<-as.numeric(r[,66])*10000              #conversion from % to [mg/kg]

d<- carobiner::change_names(r,c(52:65), 
                           c("soil_SOC","soil_pH","soil_Al","soil_Ca","soil_EC","soil_S",
                             "soil_Mn","soil_P_total","soil_Zn","soil_K","soil_Mg","soil_Na",
                             "soil_Fe","soil_B"))

d1 <- d[,c("country","adm1","latitude","longitude","elevation","crop","yield_part",
           "farmer_gender","variety_type","variety","plot_area","fertilizer_used","OM_used",
           "fertilizer_inorganic_used","fertilizer_amount","OM_amount","yield",
           "soil_SOC","soil_pH","soil_Al","soil_Ca","soil_EC","soil_S",
           "soil_Mn","soil_P_total","soil_Zn","soil_K","soil_Mg","soil_Na",
           "soil_Fe","soil_B","soil_N")]   



  carobiner::write_files (path, dset, d1) 
}
