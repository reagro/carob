# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {
   
"This database compiles soybean phenology data to address significant variations in soybean adaptation and development caused by genetic improvements and regional climatic differences. The dataset includes growth staging information collected from field experiments conducted across 11 location-years in Arkansas, Minnesota, Ohio, Virginia, and Wisconsin (USA) during 2017 and 2018. It incorporates data from commercial soybean varieties spanning maturity groups 0 to 7.5. Growth stages were determined using Fehr and Caviness (1977) approach. This dataset is intended as a resource for the scientific community, students, and stakeholders, providing soybean phenological data to improve predictions and decision-making in areas such as input timing, yield estimation, irrigation management, cultivar selection, and phenotyping"
   
   uri <- "doi:10.5061/dryad.2bvq83c20"
   group <- "agronomy"
   ff <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=3, minor=NA,
      data_organization = "UWM", #University of Wisconsin–Madison
      publication=NA, 
      project=NA, 
      data_type= "experiment", 
      treatment_vars= "land_prep_method; variety", 
      response_vars = "yield", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2025-06-16",
      completion=75,
      notes="Soybean growth stages data have not been processed"
   )
   
   
   f <- ff[basename(ff) == "SeveroSilvaEtal_Phenology_Dataset_Dryad.xlsx"]
   
   ### process General information 
   r1 <- carobiner::read.excel(f, sheet = "GeneralInformation", na= ("*"))
   
   d1 <- data.frame(
      year= as.character(r1$Year...1),
      location= ifelse(grepl("Charleson", gsub("_", "-", r1$Location)), "South Charleston", gsub("_", "-", r1$Location)),
      adm1= r1$State...2,
      land_prep_method= ifelse(grepl("chisel", r1$`Tillage type`), "reduced tillage",
                               ifelse(grepl("no-till", r1$`Tillage type`), "minimum tillage", r1$`Tillage type`)),
      lonlat= r1$`GPS coordinates`,
      planting_date= as.character(r1$`Planting Date`),
      harvest_date= gsub(" ", "", r1$`Harvest Date`),
      seed_density= as.numeric(r1$`Seeding rate (seeds/ha)`)
   )
   
   ### process Grain Harvest
   r2 <- carobiner::read.excel(f, sheet = "GrainHarvest",  na= ("*"))
   names(r2) <- r2[1,]
   r2 <- r2[-1, ]
   d2 <- data.frame(
      adm1= r2$State,
      location= r2$location,
      year= r2$year,
      plot= r2$plot,
      rep= as.integer(r2$rep),
      variety= r2$variety,
      #rm= r2$RM,
      yield_moisture= as.numeric(r2$`combine moisture`),
      yield= as.numeric(r2$`Total yield kg/a`)*2.471, # in kg/ha
      grain_protein= as.numeric(r2$`Protein 13%`),
      #grain_oil= r2$`Oil 13%`, ## oil content in %
      country= "United States" ,
      crop= "soybean" ,
      on_farm= TRUE ,
      is_survey= FALSE ,
      irrigated= NA ,
      yield_part= "grain",
      trial_id=paste0(r2$location, "-", r2$plot) ,
      geo_from_source= TRUE
   )
   
   ### merge d1 and d2
   
   d <- merge(d2, d1, by= c("adm1", "location", "year"), all.x = TRUE)
   
   lonlat <- do.call(rbind, strsplit(d$lonlat, ","))
   d$longitude  <- as.numeric(gsub("  | ", "", lonlat[,2]))
   d$latitude <- as.numeric(lonlat[,1])
   d$lonlat <- NULL
   d$adm1 <- ifelse(grepl("AR", d$adm1), "Arkansas" , 
       ifelse(grepl("MN", d$adm1),"Minnesota" , 
       ifelse(grepl("OH", d$adm1),"Ohio" ,
       ifelse(grepl("VA", d$adm1),"Virginia", "Wisconsin"))))
  
  ### Fixing harvest data
   HD <- strsplit(as.character(gsub("and", ",", d$harvest_date)), ",")
   HD <- lapply(HD, function(x) {
      length(x) <- 2  # 
      return(x)
   })
   
   split_HD <- do.call(rbind,  HD)
   d$harvest_date <- split_HD[, 1] 
   d$harvest_date <- ifelse(grepl("/", d$harvest_date),gsub("00", "20", format(as.Date(d$harvest_date, format = "%m/%d/%Y"), "%Y-%m-%d")), d$harvest_date)
   i <- grepl("^\\d{5}$", d$harvest_date)
   d$harvest_date[i] <- as.character(as.Date(as.numeric(d$harvest_date[i]), origin = "1899-12-31"))
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   
   ###  Phenology data 
   r3 <- carobiner::read.excel(f, sheet = "PhenologyData",  na= ("*"))
   names(r3) <- r3[2,]
   r3 <- r3[-c(1:2), ]
   grow_st <- names(r3)[grepl("Fehr|Be|End", names(r3))]
   d3 <- data.frame(
      adm1= r3$State,
      location= r3$location,
      plot=r3$plot,
      year= r3$year,
      rep= r3$rep,
      variety= r3$variety,
      transplanting_days= as.numeric(r3$`Plant date`)## in day of the year
   )
   d3 <- cbind(d3, r3[, grow_st])
   
   #### Process Node data 
   r4 <- carobiner::read.excel(f, sheet = "NodeData",  na= ("*"))
   names(r4) <- r4[1,]
   r4 <- r4[-c(1:2), ]
   Node <- names(r4)[grepl("Date", names(r4))]
   d4 <- data.frame(
      adm1= r4$State,
      location= gsub("South Charleston, OH", "South Charleston", r4$location),
      year= r4$year,
      plot= r4$plot,
      rep= r4$rep,
      variety= r4$variety
   )
   d4 <- cbind(d4, r4[, Node])
   d4 <- d4[!is.na(d4$adm1),]
   dd <- merge(d3, d4, bx= c("adm1", "location", "plot","rep","variety", "year"), by= c("adm1", "location", "plot","rep","variety", "year"))
   dd$adm1 <- ifelse(grepl("AR", dd$adm1), "Arkansas" , 
               ifelse(grepl("MN", dd$adm1),"Minnesota" , 
               ifelse(grepl("OH", dd$adm1),"Ohio" ,
                ifelse(grepl("VA", dd$adm1),"Virginia", "Wisconsin"))))
   ### merge Node and phenology data with yield
   df <- merge(d, dd, by=c("adm1", "location", "plot","rep","variety", "year"), all.x = TRUE)
   df$record_id <- as.integer(1:nrow(df))
   names(df) <- gsub(" ", "", names(df))
   gowth_Node <- names(df)[grepl("Fehr|Be|End|Date", names(df))]
   d <- df[, !names(df) %in% gowth_Node]
   
   d_growth_Node <- df[, gowth_Node]
   d_growth_Node$record_id <- as.integer(1:nrow(d_growth_Node))
   d_growth_Node$year <- df$year
   #d_growth_Node$transplanting_days <- df$transplanting_days
   
   dGN <- reshape(d_growth_Node, direction="long", varying = gowth_Node , v.names="value", timevar="step")
   
   dGN$variable <- c(names(d_growth_Node)[-c(21:62)], rep("Node_Nbr", 40))[dGN$step]
  
   dGN$date <- c(rep(NA, 20), seq(from = 163, by = 7, length.out = 40))[dGN$step]
  ### convert day of year in date format (observation date)
    i <- !is.na(dGN$date)
   dGN$date[i] <- as.character(as.Date(paste0(dGN$year[i], "-01-01"))+ dGN$date[i]-1) 
   
   ### grain composition 
   #db <- r2[, grepl("db", names(r2))]
   d$transplanting_date <- as.character(as.Date(paste0(d$year, "-01-01"))+ d$transplanting_days-1) 
  
    d$plot <- d$year <- dGN$year <- d$transplanting_days <- dGN$step <- dGN$id <-  NULL
   
   carobiner::write_files(path, meta, d, long = dGN)
   
}
