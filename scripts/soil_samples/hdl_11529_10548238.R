# R script for "carob"


carob_script <- function(path) {

   "  
   Nutrient Omission Trials (NOTs) from three states in Nigeria. Six treatments, yield, soil and ear leaf, stover and grain nutrient contents (2016)
   "
   uri <- "hdl:11529/10548238"
   dataset_id <- carobiner::simple_uri(uri)
   group <- "fertilizer"
   ff  <- carobiner::get_data(uri, path, group)
   
   dset <- data.frame(
      carobiner::read_metadata(uri, path, group, major=2, minor=1),
      data_institute = "CIMMYT",
      publication= "doi:10.1016/j.fcr.2019.107585",
      project=NA,
      data_type= "experiment",
      treatment_vars = "latitude;longitude;variety;variety_type;N_fertilizer;P_fertilizer;K_fertilizer",
      carob_contributor= "Cedric Ngakou",
      carob_date="2024-06-06"
   )
   ## read data
   #r0 <- carobiner::read.excel(ff[basename(ff)=="TAMASA_NOTs_Database_Nigeria_2015_2016.xlsx"], skip=1,sheet=2)
   r <- carobiner::read.excel(ff[basename(ff)=="TAMASA_NOTs_Database_Nigeria_2015_2016.xlsx"], sheet="Fertilizer rates & variety", skip=1)
   r1 <- carobiner::read.excel(ff[basename(ff)=="TAMASA_NOTs_Database_Nigeria_2015_2016.xlsx"], fix_names = TRUE, sheet="Raw_Data_2015", skip=1)
   r2 <- carobiner::read.excel(ff[basename(ff)=="TAMASA_NOTs_Database_Nigeria_2015_2016.xlsx"], fix_names = TRUE, sheet="Raw_Data_2016", skip=1)
   
   ### process file(s)

   ## process fertilizer data 
   d1 <- data.frame(country= "Nigeria",
                    adm2= r$LGAs,
                    N_fertilizer= r$`N (kg/ha)`,
                    P_fertilizer= r$`P2O5 (kg/ha)`/(2.29),
                    K_fertilizer= r$`K2O (kg/ha)`/(1.2051),
                    S_fertilizer= r$`S (kg/ha)`,
                    Ca_fertilizer= r$`Ca (kg/ha)`,
                    Zn_fertilizer= r$`Zn (kg/ha)`,
                    Mg_fertilizer= r$`Mg (kg/ha)`)
   
   ### process season1 file(s)
   ### subset yield  data  
   d2 <- data.frame(country= "Nigeria",
                    adm1= r1$State,
                    adm2= r1$LGA,
                    location= r1$Community.village,
                    latitude= r1$GPS.Coordinate.Latitude,
                    longitude= r1$GPS.Coordinate.Longitude,
                    elevation= r1$GPS.Coordinate.Altitude,
                    previous_crop= r1$Crops.grown.in.the.past.two.years,
                    fertilizer_type= r1$Fertilizer.type,
                    year= r1$Year,
                    treatment= r1$Treatment,
                    variety_type= r1$Maize.variety.type,
                    variety= r1$Maize.variety.name.MVnam,
                    #maturity_date= r1$Crop.maturation,
                    planting_date= as.character(format(as.Date(r1$Planting.date.PLNdat,"%d/%m/%Y"), "%Y-%m-%d")),
                    harvest_date= as.Date(as.numeric(r1$Harvest.date.HDATE), origin = "1899-12-30") ,
                    plot_area= r1$Size.of.the.net.plot.m.x.m,
                    yield= r1$Grain.Yield.kg.ha,
                    residue_yield= r1$Stover.Yield.kg.ha,
                    dmy_total= r1$Above.ground.biomass.t.ha*1000 ## in kg/ha
                     )
   
   ## subset Soil data 
   d3 <- data.frame(soil_pH= r1$pH.Water.1.1,
                    soil_SOC= r1$OC.pct,
                    soil_N= r1$N.pct,
                   soil_P_Mehlich= r1$MehP.ppm,
                   soil_sand= r1$Sand.pct,
                   soil_clay= r1$Clay.pct,
                   soil_silt = r1$Silt.pct,
                   soil_Ca= r1$Ca.cmol.kg,
                   soil_Mg= r1$Mg.cmol.kg,
                   soil_K= r1$K.cmol.kg,
                   soil_Na= r1$Na.cmol.kg,
                   soil_CEC= r1$Exch.Acidity.cmol.kg,
                   soil_ECEC= r1$ECEC.cmol.kg,
                   soil_Zn= r1$Zn.ppm,
                   soil_Cu= r1$Cu.ppm,
                   soil_Mn= r1$Mn.ppm,
                   soil_Fe= r1$Fe.ppm,
                   soil_B= r1$B.ppm,
                   soil_S= r1$S.pmm)
   
   ### subset nutrient content 
   d4 <- data.frame( leaf_N= r1$pct.N.73,
                    leaf_P= r1$pct.P.74,
                    leaf_K= r1$pct.K.75,
                    leaf_Ca= r1$pct.Ca.76,
                    leaf_Mg= r1$pct.Mg.77,
                    leaf_Cu= r1$ppm.Cu.79,
                    leaf_Fe= r1$ppm.Fe.80,
                    leaf_Mn= r1$ppm.Mn.81,
                    leaf_Zn= r1$ppm.Zn.82,
                    leaf_B= r1$ppm.B,
                    residue_N= r1$pct.N.84,
                    residue_P= r1$pct.P.85,
                    residue_K= r1$pct.K.86,
                    residue_Ca= r1$pct.Ca.87,
                    resitude_Mg= r1$pct.Mg.88,
                    residue_Na= r1$ppm.Na.99,
                    residue_Mn= r1$ppm.Mn.90,
                    residue_Cu= r1$ppm.Cu.91,
                    residue_Fe= r1$ppm.Fe.92,
                    residue_Zn= r1$ppm.Zn.93,
                    grain_N= r1$pct.N.94,
                    grain_P= r1$pct.P.95,
                    grain_K= r1$pct.K.96,
                    grain_Ca= r1$pct.Ca.97,
                    grain_Mg= r1$pct.Mg.98,
                    grain_Na= r1$ppm.Na.99,
                    grain_Mn= r1$ppm.Mn.100,
                    grain_Cu= r1$ppm.Cu.101,
                    grain_Fe= r1$ppm.Fe.102,
                    grain_Zn= r1$ppm.Zn.103)
  
   ### Joint all variables 
   d <- cbind(d2,d3,d4)
   ###### merge with fertilizer data ##################
   d <- merge(d,d1,by=c("country","adm2"),all.x = TRUE)
   
   ## Process second season data
   dd <- data.frame(country= "Nigeria",
                    adm1= r2$State.Province,
                    adm2= r2$LGA,
                    location= r2$Community.village,
                    latitude= r2$GPS.Coordinate.Latitude,
                    longitude= r2$GPS.Coordinate.Longitude,
                    elevation= r2$GPS.Coordinate.Altitude,
                    year= r2$Year,
                    treatment= r2$Treatment,
                    variety_type= r2$Maize.variety.type,
                    variety= r2$Maize.variety.name.MVnam,
                    #maturity_date= r2$Crop.maturation,
                    planting_date= as.character(format(as.Date(r2$Planting.date,"%d/%m/%Y"), "%Y-%m-%d")),
                    harvest_date= substr(r2$Harvest.Date,1,10),
                    yield= r2$Grain.Yield.kg.ha,
                    residue_yield= r2$Stalk.Yield.kg.ha,
                    dmy_total= r2$Above.ground.biomass.t.ha*1000,## in kg/ha
                    leaf_N= r2$pct.N.46,
                    leaf_P= r2$pct.P.47,
                    leaf_K= r2$pct.K.48,
                    leaf_Ca= r2$pct.Ca,
                    leaf_Mg= r2$pct.Mg,
                    leaf_Cu= r2$ppm.Cu,
                    leaf_Fe= r2$ppm.Fe,
                    leaf_Mn= r2$ppm.Mn,
                    leaf_Zn= r2$ppm.Zn,
                    leaf_B= r2$ppm.B,
                    residue_N= r2$pct.N.57,
                    residue_P= r2$pct.P.58,
                    residue_K= r2$pct.K.59,
                    grain_N= r2$pct.N.60,
                    grain_P= r2$pct.P.61,
                    grain_K= r2$pct.K.62
                    )
   
   d <- carobiner::bindr(d,dd)
   d$season <- "1"
   d$season[grep("2016",d$year)] <- "2"
   d$year <- NULL
   ## remove row where geo data is NA
   d$adm2 <- gsub("5", NA, d$adm2)
   d$adm1 <- gsub("5", NA, d$adm1)
   d$location <- gsub("5",NA, d$location)
   d <- d[!(is.na(d$adm1) & is.na(d$adm2) & is.na(d$location)),]
   d$crop <- "maize"
   d$trial_id <- "1"
   d$plant_spacing <- 25 #from data description 
   d$row_spacing <- 75 # from data description
   d$plot_width <- 5  # from data description
   d$plot_lenght <- 6 # from data description
   d1$on_farm <- TRUE
   d1$is_survey <- FALSE
   d1$irrigated <- FALSE
   d1$inoculated <- FALSE
   d$yield_part <- "grain"
   ## Fexing previous crop content
   p <- carobiner::fix_name(d$previous_crop,"lower")
   p <- gsub("and","",p)
   p <- gsub(",",";",p)
   p <- gsub(";","",p)
   p <- gsub("  "," ",p)
   p <- gsub(" ","; ",p)
   p <- gsub("gnut","groundnut",p)
   p <- gsub("maize; g/nut; &; sorghum","maize; groundnut; sorghum" ,p)
   p <- gsub("mazie" ,"maize" ,p)
   p <- gsub("53",NA,p)
   d$previous_crop <- p
   ## Fixing  content
   d$fertilizer_type <- gsub("77",NA,d$fertilizer_type)
   d$fertilizer_type <- gsub(",",";",d$fertilizer_type)
   d$fertilizer_type <- gsub("Urea","urea",d$fertilizer_type)
   d$plot_area <- as.numeric(gsub("3 x 3",9,d$plot_area))
   ### Fixing date 
   d$harvest_date <- gsub("1900-02-09",NA,d$harvest_date)
   
   carobiner::write_files(path, dset, d)
   
}

