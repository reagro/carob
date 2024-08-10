# R script for "carob"

### Weather variables need to be clarified , there is no description.  




carob_script <- function(path) {
   
 " The data set were generated through on-farm field trials that were established in Maluti-a-Phofung municipality, situated in the eastern Free State province of South Africa as part of a project entitled Innovations in Technology, Institutional and Extension Approaches towards Sustainable Agriculture and enhanced Food and Nutrition Security in Africa (Acronym-InnovAfrica). This project was implemented by a consortium of 16 institutions from Africa and Europe continents across six African countries (viz. Ethiopia, Kenya, Malawi, Rwanda, South Africa and Tanzania) from June 2017 to November 2021. Farmer-led trials were established for three planting seasons (2017/2018 to 2019/2020) to demonstrate, test and upscale the best-bet sustainable agricultural intensification practices for improved food and nutrition security in smallholder settings."
   
   uri <- "doi:10.5061/dryad.8w9ghx3q6"
   group <- "agronomy"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- data.frame(
      carobiner::read_metadata(uri, path, group, major=1, minor=2), 
      data_institute = "CIMMYT", 
      publication=NA, 
      project=NA, 
      data_type= "experiment", 
      response_vars= "yield", 
      treatment_vars = "variety;intercrops", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-08-10"
   )
   
   ff <- ff[grepl("xlsx", basename(ff))]
   
   process <- function(f){ 
      
      r1 <- carobiner::read.excel.hdr(f, sheet = "Agronomy Data", skip=0)
   ## Fixing longitude and latitude  
      for (i in 2:nrow(r1)) if (is.na(r1$Latitute.degree[i])) r1$Latitute.degree[i] <- r1$Latitute.degree[i-1]
      for (i in 2:nrow(r1)) if (is.na(r1$Longitude.degree[i])) r1$Longitude.degree[i] <- r1$Longitude.degree[i-1]
      for (i in 2:nrow(r1)) if (is.na(r1$altitude.m[i])) r1$altitude.m[i] <- r1$altitude.m[i-1]
      for (i in 2:nrow(r1)) if (is.na(r1$Farm.No[i])) r1$Farm.No[i] <- r1$Farm.No[i-1]
      
   ### Agronmy data
      d1 <- data.frame(
         latitude= r1$Latitute.degree,
         longitude= r1$Longitude.degree,
         elevation= r1$altitude.m,
         crop= r1$Crop.species,
         variety= r1$Cultivar.variety,
         intercrops = ifelse(grepl("Maize/beans Intercrop",r1$Cropping.System)& r1$Crop.species=="maize", "common bean",
                             ifelse(grepl("Maize/beans Intercrop",r1$Cropping.System)& r1$Crop.species=="Beans","maize","none")),
      
         planting_date= as.character(as.Date(r1$Plant.date, "%d/%m/%Y")),
         weeding_dates= paste0(as.Date(r1$First.weeding.date, "%d/%m/%Y"), ";", as.Date(r1$Second.weeding.date, "%d/%m/%Y")),
         harvest_date=  as.character(as.Date(r1$Harvest.date, "%d/%m/%Y")),
         yield= r1$Yield.production.Ton.ha*1000, # kg/ha
         season= substr(gsub("_", "-", basename(f)),26,34),
         country= "South Africa",
         trial_id= r1$Farm.No
         )
  
   ## soil data 
       r2 <- carobiner::read.excel.hdr(f,sheet = "Soil data",skip=0)
       r2 <- r2[-1,] ## removing the first row
### Fixing longitude and latitude
       for (i in 2:nrow(r2)) if (is.na(r2$Latitute.degree[i])) r2$Latitute.degree[i] <- r2$Latitute.degree[i-1]
       for (i in 2:nrow(r2)) if (is.na(r2$Longitude.degree[i])) r2$Longitude.degree[i] <- r2$Longitude.degree[i-1]
       for (i in 2:nrow(r2)) if (is.na(r2$altitude.m[i])) r2$altitude.m[i] <- r2$altitude.m[i-1] 
        names(r2) <- gsub("pH.H2O", "pH",names(r2))
   
       if(is.null(r2$S)) r2$S <- NA
       if(is.null(r2$EC)) r2$EC <- NA
       if(is.null(r2$CEC)) r2$CEC <- NA
       if(is.null(r2$Particle.size.distribution)) r2$Particle.size.distribution <- NA
       if(is.null(r2$X.14))  r2$X.14 <- NA
       if(is.null(r2$X.15))  r2$X.15 <- NA
       if(is.null(r2$Depth))  r2$Depth <- NA
   
      d2 <- data.frame(
         latitude= r2$Latitute.degree,
         longitude= r2$Longitude.degree,
         elevation=  r2$altitude.m,
         soil_sample_bottom= substr(r2$Depth,3,5),
         soil_sample_top = substr(r2$Depth,1,2),
         soil_pH= as.numeric(r2$pH),
         soil_P_available= as.numeric(r2$P),
         soil_Ca= as.numeric(r2$Ca),
         soil_Mg= as.numeric(r2$Mg),
         soil_K= as.numeric(r2$K),
         soil_Na= as.numeric(r2$N),
         soil_EC= as.numeric(r2$EC),
         soil_sand= as.numeric(r2$Particle.size.distribution),
         soil_silt= as.numeric(r2$X.14),
         soil_clay= as.numeric(r2$X.15),
         soil_S= as.numeric(r2$S),
         soil_CEC= as.numeric(r2$CEC)
         )
      
      merge(d1,d2,by=c("longitude", "latitude", "elevation"), all.x = TRUE)
  
   }
   
   d <- lapply(ff, process)
   d <- do.call(rbind,d)
   d$weeding_dates[grepl("2029-01-15",d$weeding_dates)] <- "2019-01-15"
   d$crop <- gsub("Beans", "common bean", d$crop)
   d$soil_sample_bottom <- as.numeric(gsub("-60",60,d$soil_sample_bottom))
   d$soil_sample_top <- as.numeric(gsub("0-",0,d$soil_sample_top))
   
   d$irrigated <- as.logical(NA)
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "grain"
   d$adm3 <- "Maluti a Phofung"
   d$adm2 <- "Thabo Mofutsanyana District Municipality"
   d$adm1 <- "Free State"
   d$geo_from_source <- TRUE
   
   ### adding information from data description
   d$plant_spacing <- 90
   d$row_spacing <- 25
   d$row_spacing[d$crop=="common bean"] <- 5
   d$fertilizer_amount <- 200 # kg/ha
   d$N_fertilizer <- 84
   d$P_fertilizer <- 56/2.29
   d$K_fertilizer <- 28/1.2051
   d$herbicide_product  <- "metolachlor"
   d$herbicide_used  <- TRUE
 
     
### record weather data 
   
  weather <- function(f){
     Sheet <- "Climate data"
     if (grepl("2019_2020",basename(f)) ) Sheet <- "Sheet1"
     w <- carobiner::read.excel(f, sheet = Sheet, na=c("--",NA))
     ## treat header 
     names(w) <- w[grep("Year" ,w[,1])[1],] ## 
     w <- w[-1,1:11] ### keep rows with useful information 
     ## removing separate line between trials to get continuous data 
     ind=grep("Average|Total|Highest|Lowest|Day" ,w[,3]) 
     w <- w[-ind,] 
     w <- w[!is.na(w$Year),]
     ## process
     data.frame(
        country = "South Africa",
        date = as.character(as.Date(paste(w$Day,w$Month,w$Year, sep="-"),"%d-%m-%Y")),
        location = "Maluti a Phofung",
        station_name= "QWAQWA; UNIQWA",
        longitude= 28.82518,
        latitude= -28.48291,
        temp = w$Tx, 
        prec = w$Rain,
        rhum = w$RHx, 
        wspd = w$U2, 
        srad = w$Rs
     )
     
   }
   
    dw <- lapply(ff, weather)
    dw <- do.call(rbind,dw)  
 
   ## remove empty rows in date 
   dw <- dw[!is.na(dw$date),] 


carobiner::write_files(path, meta, d,wth = dw)
   
}

#carob_script(path)
