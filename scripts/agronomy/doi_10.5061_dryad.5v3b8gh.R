# R script for "carob"


#ISSUES 
# the groundnut intercrop and yield needs to be processed
# it is clear from the description that the presence or absence of an intercrop this is a treatment variable but it is not treated as such


carob_script <- function(path) {
     
"Sorghum (Sorghum bicolor L. Moench)–groundnut (Arachis hypogaea L.) intercropping is important in the Sahel and Sudan Savanna. Nineteen trials were conducted during 2014 and 2015 in Mali and Niger for determination of: response functions for sorghum sole crop (SSC) and intercrop to N, P, and K; effects of the P×N interaction and of a nutrient package for diagnosis of other deficiencies; and a procedure for estimation of intercrop functions from SSC response functions. Six Niger on-station trials included treatments for determination of intercrop effects on sorghum yield and response. Mean grain and fodder yield increases for SSC were, respectively, 18.8 and 17.6% with 10 kg ha–1 P, 30.7 and 18.8% with 30 kg ha–1 N, and 0 and 6.8% with 10 kg ha–1 K applied. The diagnostic treatment increased grain yield for only 1 of 19 site-years (SYs). Sorghum grain yield was 30% less with intercropping compared with SSC for 2 of 6 SY but 11% less overall. Overall, sorghum fodder yield was 23% more with intercropping. Sorghum response to P was 86% greater for SSC compared with intercrop. With intercropping, groundnut pod yield exceeded sorghum grain yield. The value of intercrop production was higher than for SSC. Fodder added 23 and 30% to the value of SSC and intercrop harvests, respectively. Equations were developed for determining optimal nutrient rates and intercrop response functions from SSC functions. Fertilizer applied to the intercrop compared with SSC has much more profit potential for this part of West Africa."
   
   uri <- "doi:10.5061/dryad.5v3b8gh"
   group <- "agronomy"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- data.frame(
      carobiner::read_metadata(uri, path, group, major=1, minor=2), 
      data_institute = "UNL", 
      publication="doi:10.2134/agronj2017.02.0120", 
      project=NA, 
      data_type= "experiment", 
      response_vars= "yield", 
      treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;intercrops", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-06-21"
   )
   
   d <- lapply(c("NigerOFT", "NigerOST", "Mali", "BurkinaFasoDori"), \(i){
      r <- carobiner::read.excel(ff[basename(ff)=="W Africa Sorghum data set.xlsx"], sheet = i)
      names(r) <- gsub("Year", "Y", names(r))
      names(r) <- gsub("Soil", "Site", names(r))
      names(r) <- gsub("SorGrYld", "SorGrYd", names(r))
      names(r) <- gsub("SorStYd|SorStYld", "SorFoYd", names(r))
      if(is.null(r$`Manure rate, Mg ha-1`)){r$`Manure rate, Mg ha-1` <- NA}
      d <- data.frame(
         location= r$Site, 
         planting_date= as.character(r$Y), 
         rep= as.integer(r$Rep), 
         treatment= r$Trt, 
         N_fertilizer= r$Nrate, 
         P_fertilizer= r$Prate, 
         K_fertilizer= r$Krate, 
         OM_amount= r$`Manure rate, Mg ha-1`*1000, # kg/ha
         crop= "sorghum", 
         yield= r$SorGrYd*1000, # in kg/ha
         fwy_total= r$SorFoYd, 
         intercrops="groundnut", 
         trial_id= i
         
      )
   })
     
   d <- do.call(rbind, d) 
    i <- which(is.na(d$N_fertilizer) & (d$yield == 0))
	d <- d[-i,]
   
   d$country <- NA
   d$country[grep("Niger", d$trial_id)] <- "Niger"
   d$country[grep("Mali", d$trial_id)] <- "Mali"
   d$country[grep("Burkina", d$trial_id)] <- "Burkina Faso"
   
   d$irrigated <- as.logical(NA)
   d$on_farm <- TRUE
   d$on_farm[grep("NigerOST", d$trial_id)] <- FALSE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "grain"
   d$plot_area <- 36 # m2


   d$treatment <- paste0("N", d$N_fertilizer, "P", d$P_fertilizer, "K", d$K_fertilizer)

   d$location[grepl("Kebila", d$location)] <- "Kebila"
   d$location[grepl("Kol", d$location)] <- "Kolombada" 
   
	geo <- data.frame(
		location = c("Cam", "Bengou", "Kagara", "Maradi", "Konni", "Maradi_Angoual", "Kolombada", "Bema", "Kou", "Kebila", "Dori", "Boni", "Hounde"), 
		latitude = c(NA, 11.9906, 13.3748, 13.5012, 13.7916, 13.533, 12.6935, 15.043, 13.3785, 11.2734, 14.0328, 11.5125, 11.4922), 
		longitude = c(NA, 3.5932, 7.0099, 7.1025, 5.2478, 7.9488, -7.0095, -9.3532, -4.5978, -7.042, -0.0348, -3.3864, -3.5206)
	)
      
   d <- merge(d, geo, by="location", all.x = TRUE)
 
   
   ## remove Na in yield
   ## do not use unique, it hides errors!
##   d <- d[!is.na(d$yield), ]
   
    carobiner::write_files(path, meta, d)
   
}


