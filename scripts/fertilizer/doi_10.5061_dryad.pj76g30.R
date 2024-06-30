# R script for "carob"


carob_script <- function(path) {
   
   "
   Fertilizer application can affect nutrient concentrations of edible plant products. Data from 70 crop-nutrient response trials conducted in Mali, Niger, Nigeria, and Tanzania were used to evaluate nutrient application effects on nutrient concentrations for grain of five pulse and five cereal crops and for storage roots of cassava (Manihot esculenta L.).
   Treatments per trial were ≥12 but this study was limited to: no fertilizer applied; macronutrients applied (NPK or PK); and the macronutrient treatment plus Mg, S, Zn, and B applied (MgSZnB). Dried grain or cassava flour samples were analyzed for concentrations of all essential soil nutrients except for Ni and Cl. Concentrations of N and K were positively correlated with concentrations of most other nutrients.
   The concentrations were relatively low overall for cowpea (Vigna unguiculata L.) and pigeonpea (Cajanus cajan L.) compared with other pulse crops and for maize (Zea mays L.) compared with other cereal crops. Application of NPK or PK had little effect on nutrient concentrations except for increased mean cereal grain concentrations for N, Ca, Mg, S, Zn, Cu, and B.
   Bean (Phaseolus vulgaris L.), maize and rice (Oryza sativa L.) grain concentrations were reduced by MgSZnB for N, K, S, Cu, Mn, and B.There were no or inconsistent effects of MgSZnB on other crop-nutrient concentrations. Nutrient concentrations are not reduced by NPK for non-legumes or PK for pulses but MgSZnB often reduced bean and cereal nutrient concentrations with greater reductions for immobile compared with mobile nutrients.
   "
   uri <- "doi:10.5061/dryad.pj76g30"
   group <- "fertilizer" 
   
   ff  <- carobiner::get_data(uri, path, group)
   
   dset <- data.frame(
      carobiner::read_metadata(uri, path, group, major=1, minor=4), 
      data_institute = "UNL", 
      publication ="doi:10.2134/agronj2018.04.0274", 
      project = NA, 
      data_type = "experiment", 
      treatment_vars = "N_fertilizer;P_fertilizer;P_fertilizer;K_fertilizer;S_fertilizer;Mg_fertilizer;Zn_fertilizer;B_fertilizer", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-06-29"
   )
   
   ##### PROCESS data records
   f <- ff[basename(ff)=="Grain Nutrient Conc Study for Tropical Africa 2018.xlsx"]
   
   r <- carobiner::read.excel(f,sheet = "Data")
   
#### soil information    
   d <- data.frame(
      country= r$Country,
      trial_id= r$Plot_code,
      date= r$Year,
      crop= r$Crop,
      soil_pH= r$pH,
      soil_B= r$B...21,
      soil_Cu= r$CU,
      soil_Fe= r$FE,
      soil_Mn= r$MN,
      soil_S= r$S...26,
      soil_Zn= r$Zn...27,
      soil_ex_Ca= r$ExCa,
      soil_ex_Mg= r$ExMg,
      soil_ex_K= r$ExK,
      soil_N= r$TotN,
      soil_P_Mehlich= r$m3P,
      soil_C= r$TotCarbon,
      soil_clay= r$Clay,
      soil_silt= r$Silt,
      soil_sand= r$Sand,
      treatment= r$Contr1Macro2MgZnSB3,
      on_farm= TRUE,
      irrigated= NA,
      inoculated= FALSE,
      is_survey= FALSE
   )
   d$planting_date <- as.character(d$date)
   d$date <- NULL
   d$treatment <- c("control","NPK","MgSZnB")[d$treatment]
   ### adding yield data ( from doi:10.2134/agronj2018.04.0274)
   ### It is a mean value of yield per country and per crop 
   ### Tanzania #############################
   yd1 <- data.frame(
         country= "Tanzania",
         crop=c("Bean", "Cassava", "Cowpea", "Maize", "Pigeonpea", "Soybean", "Sorghum", "Wheat"),
         yield1=c(1.83, 16.40, 2.35, 3.76, 2.08, 1.09, 4.33, 1.85), ### control treatment
         yield2=c(2.06, 25.9, 2.61, 4.41, 2.29, 1.36, 4.935, 2.32), ### NPK trial treatment
         yield3=c(1.83,19.21, 2.35, 376, 2.08, 1.09, 4.33, 2.18))   ### MgSZnB treatment
   
   ### Niger #############################
   
   yd2 <- data.frame(
      country= "Niger",
      crop=c("Cowpea", "Groundnut", "Maize", "Pearl millet", "Rice", "Sorghum"),
      yield1=c(0.56, 0.64, 1.47, 0.66, 2.30, 1), ### control treatment
      yield2=c(0.74, 0.87, 1.93, 0.87, 2.84, 1.43), ### NPK trial treatment
      yield3= c(0.71, 0.79, 1.47, 0.66, 2.30, 1)) ### MgSZnB treatment
   
   ### Nigeria #############################
   
   yd3 <- data.frame(
      country="Nigeria",
      crop=c("Groundnut", "Maize", "Soybean", "Sorghum"),
      yield1=c(1.07, 4.87, 1.83, 1.21), ### control treatment
      yield2=c(1.74, 5.75, 2.42, 1.85), ### for NPK trial treatment
      yield3=c(1.07, 4.87, 2.06, 1.47)) ### for MgSZnB treatment
   
   ### Mali #############################
   yd4 <- data.frame(
      country= "Mali",
      crop=c("Maize", "Pearl millet", "Rice"),
      yield1=c(2.82, 1.05, 2.21), # control treatment
      yield2=c(4.03,1.24, 2.66),  # NPK trial treatment
      yield3=c(2.82, 1.05, 2.21)) ## MgSZnB treatment
   
   #### joint yield data ##################################
   yd <- carobiner::bindr(yd1,yd2,yd3,yd4)
   
   lst <- list()
   var=c("1","2","3")
   for (i in var) {
      names(yd) <- gsub(i,"",names(yd))
      yd <- yd[,c("country","crop","yield")]
      yd$treatment  <- as.numeric(i)
      lst[[i]] <- yd
   }	
   
   d1 <- do.call(rbind, lst)
   d1$treatment <- c("control","NPK","MgSZnB")[d1$treatment]
  
   d <- merge(d,d1,by=c("country","crop","treatment"),all.x = TRUE)
   
   p <- carobiner::fix_name(d$crop,"lower")
   p <- gsub("bean","common bean",p)
   p <- gsub("soycommon bean","soybean",p)
   p <- gsub("pigeonpea","pigeon pea",p)
   d$crop <- p
   ### Nutrient contain in grain 
   
   d2 <- data.frame(
      grain_B= r$B...7,
      grain_Ca= r$Ca,
      grain_Cu= r$Cu,
      grain_K= r$K,
      grain_Mg= r$Mg,
      grain_Mn= r$Mn,
      #grain_Na= r$Na,
      grain_P= r$P,
      grain_S= r$S...16,
      grain_N= r$N
   )
   
   d <- cbind(d,d2)
   
   ################ Adding fertilizer ###################
   d$N_fertilizer <- d$P_fertilizer <- d$P_fertilizer  <- d$K_fertilizer <- d$S_fertilizer <- d$Mg_fertilizer <-  d$Zn_fertilizer <- d$B_fertilizer <- 0
   
   d$N_fertilizer[d$treatment=="NPK"] <- 90
   d$P_fertilizer[d$treatment=="NPK"] <- 15/2.29
   d$K_fertilizer[d$treatment=="NPK"] <- 20/1.2051
   d$S_fertilizer[d$treatment=="MgSZnB"] <- 15
   d$Mg_fertilizer[d$treatment=="MgSZnB"] <- 10
   d$Zn_fertilizer[d$treatment=="MgSZnB"] <- 3.5
   d$B_fertilizer[d$treatment=="MgSZnB"] <- 0.5
   
   ###### adding longitude and latitude #############
   ## The data and the document (published article) do not specify the site where the experiment was carried out.
   ## should we set long and lat into NA instead of using country coordinate?
   geo <- data.frame(country=c("Mali", "Niger", "Nigeria", "Tanzania"),
                     latitude=c(16.3700359, 17.7356214, 9.6000359, -6),
                     longitude=c(-2.2900239, 9.3238432, 7.9999721, 35))
   
   d <- merge(d,geo,by="country",all.x = TRUE)
   d$yield <- d$yield*1000 ## in kg/ha

   d$yield_part= "grain"
   d$yield_part[d$crop=="cassava"] <- "roots"
   
   carobiner::write_files(path, dset, d)
}


