# R script for "carob"



carob_script <- function(path) {
   
"Traits related with drought resistance in common beans. A set of 16 bean genotypes belonging to the Middle American gene pool were evaluated under field conditions with two levels of water supply (irrigated and drought) over two seasons. Lines SEN 56, BFS 29, NCB 226 and SER 16 showed high grain yield in both irrigated and terminal drought conditions, while RCB 593 and G 40001 showed high grain yield in drought. Drought resistant lines produced at least 15% and 50% more grain yield than EAP 9510-77, commercial check for Central America, in irrigated and drought condition, respectively. In addition, they were also superior to other commercial checks such as DOR 390 and Bribri."
   
   uri <- "doi:10.7910/DVN/AFNDFX"
   group <- "agronomy" 
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- data.frame(
      carobiner::read_metadata(uri, path, group, major=3, minor=0), 
      data_institute = "CIAT", 
      publication = "doi:10.15517/ma.v29i1.27618",
      project =NA, 
      data_type = "experiment",
      response_vars = "yield;fwy_total",
      treatment_vars = "variety;irrigated", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-10-13"
   )
   
   f <- ff[basename(ff)=="02. Datos _Chaves.csv"]
  
   ## processing data
   r <- read.csv(f, fileEncoding='latin1',check.names=F, na=c("."))
   names(r) <- gsub("100SW", "Sw100", names(r))
   d <- data.frame(
     country= "Colombia",
     location= "Palmira",
     crop= "common bean",
     planting_date= ifelse(grepl("Riego", r$Ambiente) & grepl("2012", r$Año), "2012-08-10",
                    ifelse(grepl("Riego", r$Ambiente) & grepl("2013", r$Año), "2013-07-18",
                    ifelse(grepl("Sequia", r$Ambiente) & grepl("2012", r$Año), "2012-08-03", "2013-07-15"))) ,
     irrigated= ifelse(grepl("Riego", r$Ambiente), TRUE, FALSE),
     treatment=  ifelse(grepl("Riego", r$Ambiente), "irrigation", "Drought"),
     rep= r$Rep,
     variety= r$Genotipo,
     yield= r$YDHA,
     seed_weight= r$Sw100*10,
     flowering_days= r$DF,
     harvest_index= r$HI,
     fwy_total= as.numeric(r$CB),
     LAI= r$LAI,
     seed_density= r$SNA*10000,
     trial_id= paste0(r$Año, "_", r$Ambiente)
     
   )
   
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "seed"
   d$latitude <- 3.53782
   d$longitude <- -76.2968
   d$geo_from_source <- FALSE
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   d$harvest_index[d$harvest_index > 100] <- NA
   
   carobiner::write_files (path, meta, d)
}


