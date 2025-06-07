# R script for "carob"

## moving to pending because there is no treatment level yield data, so we cannot use this for the "fertilizer" group unless we aggregated the treatments and use the published average yields.

## Alternatively, this could be added to the "soil" group, but note that most soil data are also averages from the publication. That would be OK for the "fertilizer" group, but not for the "soils" group.

## would need to contact authors to rescue this.


carob_script <- function(path) {
   
"Fertilizer applications are poised to increase across sub-Saharan Africa (SSA), but the fate of added nitrogen (N) is largely unknown. We measured vertical distributions and temporal variations of soil inorganic N following fertilizer application in two maize (Zea mays L.)-growing regions of contrasting soil type.
   
Fertilizer trials were established on a clayey soil in Yala, Kenya, and on a sandy soil in Tumbi, Tanzania, with application rates of 0–200 kg N/ha/yr. Soil profiles were collected (0–400 cm) annually (for three years in Yala and two years in Tumbi) to examine changes in inorganic N pools. 
   
Topsoils (0–15 cm) were collected every 3–6 weeks to determine how precipitation and fertilizer management influenced plant-available soil N. Fertilizer management altered soil inorganic N, and there were large differences between sites that were consistent with differences in soil texture.
   
Initial soil N pools were larger in Yala than Tumbi (240 vs. 79 kg/ha). Inorganic N pools did not change in Yala (277 kg/ha), but increased fourfold after cultivation and fertilization in Tumbi (371 kg/ha). Intra-annual variability in NO−3-N concentrations (3–33 μg/g) in Tumbi topsoils strongly suggested that the sandier soils were prone to high leaching losses.Information on soil inorganic N pools and movement through soil profiles can h vulnerability of SSA croplands to N losses and determine best fertilizer management practices as N application rates increase. A better understanding of the vertical and temporal patterns of soil N pools improves our ability to predict the potential environmental effects of a dramatic increase in fertilizer application rates that will accompany the intensification of African croplands. " 
   

   uri <- "doi:10.5061/dryad.5qr62"
   group <- "fertilizer"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- data.frame(
      carobiner::get_metadata(uri, path, group, major=2, minor=1), 
      data_organization = "UM",
      publication = "doi:10.1890/15-1518.1", 
      project = NA, 
      data_type = "experiment", 
      response_vars = "yield",
      treatment_vars = "N_fertilizer", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-07-05"
   )
   
#   ff <- ff[grep(".csv$", ff)]

   process <- function(f){
      r <- read.csv(f)
      names(r) <- gsub("Year","Date",names(r))
      names(r) <- gsub("Year","Date",names(r))
      names(r) <- gsub("BulkD","BD",names(r))
      if (is.null(r$pH)) r$pH <- as.numeric(NA)
	  if (is.null(r$Depth)) r$Depth <- as.numeric(NA)
      if (is.null(r$Location)) r$Location <- as.numeric(NA)
	  if (is.null(r$BD)) r$BD <- as.numeric(NA)
      if (is.null(r$DepthMid)) r$DepthMid <- as.numeric(NA)
	  if (is.null(r$kgNperha)) r$kgNperha <- as.numeric(NA)
      data.frame(
         country= r$Location,
         soil_pH= r$pH,
         treatment=r$Treatment,
         date= r$Date,
         soil_depth = r$DepthMid,
         soil_C = r$ConcNH4,
         #soil_N03= r$ConcNO3,
         soil_bd= r$BD,
         soil_N= r$kgNperha,
         depth=r$Depth,
         trial_id = basename(f)
      )
    }
   
   d <- lapply(ff, process)
   d <- do.call(rbind,d)
   
   d$crop <- "maize"
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$irrigated <- as.logical(NA)
   d$inoculated <- FALSE
   d$yield_part <- "grain" 
   
   ## Fixing country name
   d$country[is.na(d$country)] <- d$trial_id[is.na(d$country)]
   d$country[grep("TZ",d$country)] <- "Tanzania"
   d$country[grep("KE",d$country) ] <- "Kenya"
   d$country[grep("2014_CIAT_DP.csv",d$country) ] <- "Kenya" ## from data description
   d$treatment[d$treatment=="0 N" | d$treatment=="0"] <- "control"
   d$treatment[d$treatment=="200" | d$treatment=="200 N"] <- "200N"
   
   ### adding location 
   d$location[d$country=="Kenya"] <- "Yala"
   d$location[d$country=="Tanzania"] <- "Tumbi"
   d$longitude[d$location=="Yala"] <- 34.5338
   d$latitude[d$location=="Yala"] <- 0.0954
   d$longitude[d$location=="Tumbi"] <- 34.6292  
   d$latitude[d$location=="Tumbi"] <- -11.0335
   
   ###
   d$depth <- gsub("to", "-", d$depth)
	d$depth <- gsub(" - | -","-",d$depth)
   k <- t((data.frame(strsplit(d$depth, "-")))) ## transpose 
   rownames(k) <- 1:nrow(k)  ## Renaming index 
   d$soil_sample_top <- as.numeric(k[,1])
   d$soil_sample_bottom <- as.numeric(k[,2])
   i <- grep("WeeklyKCl", d$trial_id)
   d$soil_depth[i] <- 15
   d$soil_sample_top[i] <- 0
   d$soil_sample_bottom[i] <- 15
   d$depth[i] <- "0-15"
   
   ## Fixing date
   d$date <- carobiner::eng_months_to_nr(d$date)
   j <- grep("2014_CIAT_DP.csv", d$trial_id)
   d$date[j] <- as.character(as.Date(d$date[j], "%d/%m/%y"))
   i <- grep("WeeklyKCl", d$trial_id)
   d$date[i] <- as.character(as.Date(d$date[i], "%d-%m-%y"))

   
   ## adding yield data ( annual yield ) ## from doi:10.1890/15-1518.1
## RH: you cannot add the average yields for treatments, we need the actual data .
##   d$year  <- substr(d$date, 1, 4)
##   yd <- data.frame(
##	          treatment= c("control", "control", "200N", "control", "200N", "control", "control", "75", "200N"),
##            year=c("2012", "2013","2013", "2014", "2014","2013",rep("2014",3)),
##            country=c(rep("Kenya",5),rep("Tanzania",4)),
##            yield=c(6.2, 3.8, 4.9, 3.8, 7.2, 1.2, 0.2, 1.3, 0.8)
##         )
## d <- merge(d, yd, by=c("treatment","year","country"), all.x = TRUE)
##   d$year <- NULL
##   d$yield <- d$yield*1000  ## kg/ha
   
### adding more soils information from doi:10.1890/15-1518.1
	soil <- data.frame(
		depth=c(rep(c("0-15", "15-30", "30-50", "50-100", "100-150", "150-200", "200- 250", "250-300", "300-350", "350-400"),2)),
		soil_sand=c(52.2, 52.9, 44.6, 37.8, 39.4, 47.4, 45.8, 43.8, 45.4, 43.4, 87.4, 88.5, 88.9, 86.3, 90.3, 88.3, 92, 92, 90.4, 90.3),
        soil_silt=c(12.4, 10.3, 11.7, 9.6, 9.6, 9.6, 9.6, 10.3, 15.6, 17.6, 3.6, 2.4, 1.48, 2.2, 1.8, 3.8, 0.2, 1.8, 3, 1.8),
        soil_clay= c(35.2, 36.65, 43.6, 52.5, 50.8, 42.8, 44.5, 45.7, 38.8, 38.8, 8.9, 9, 9.5, 11.4, 7.8, 7.8, 7.8, 6.2, 6.6, 7.8),
        country=c(rep("Kenya",10), rep("Tanzania",10)))
   
   
	d <- merge(d, soil, by=c("depth", "country"), all.x = TRUE)
   
   d$k <- ifelse(d$country=="Kenya" & d$depth=="0-15", 1,
                 ifelse(d$country=="Tanzania" & d$depth=="0-15", 2, NA))
   
   ### information from doi:10.1890/15-1518.1
   d$soil_P_available <- c(0.06,0.03)[d$k] ## in mg/kg
   d$soil_K <- c(50.5*1000,0.71*1000)[d$k]
   d$soil_Ca <-c( 19.4*1000,1.13*1000)[d$k] ## in mg/kg
   d$soil_EC <- c(248.75, 18.25)[d$k]
   d$soil_CEC <- c(15.4, 1.78)[d$k]
   d$soil_N <- c(0.011*10000, 0.18*10000)[d$k]
   d$soil_C <- c( 1.9, 2.05)[d$k]
   d$soil_Mg <- c(2.26*1000,0.3*1000)[d$k] ## in mg/kg
   d$depth <- d$k <- NULL
  
   d$N_fertilizer <- 0
   d$N_fertilizer[d$treatment=="200N"] <- 200
   d$N_fertilizer[d$treatment=="75"] <- 75
   d$P_fertilizer <- d$K_fertilizer <- 0
   
   ##setting planting date into NA.  
   d$planting_date <- as.character(NA)
   ## remove row with NA in yield 
#   d <- unique(d[!is.na(d$yield),])
   
   carobiner::write_files (path, meta, d) 
}

