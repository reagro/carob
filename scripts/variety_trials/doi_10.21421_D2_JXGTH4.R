# R script for "carob"


carob_script <- function(path) {
   
"High-Oil lines trial comprising of 36 entries [32 elite breeding lines from ICRISAT and 4 improved varieties (SAMNUT-22, SAMNUT 23, SAMNUT 24 and SAMNUT 26) as checks] was conducted at BUK locations."

   uri <- "doi:10.21421/D2/JXGTH4"
   dataset_id <- carobiner::simple_uri(uri)
   group <- "variety_trials" 
   
   ff  <- carobiner::get_data(uri, path, group)
   meta <- data.frame(
      carobiner::read_metadata(uri, path, group, major=1, minor=0), 
      data_institute = "ICRISAT", 
      publication =NA, 
      project = NA, 
      data_type = "experiment", 
      treatment_vars = "variety", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-07-10"
   )
   f <- ff[basename(ff) == "Data file of High oil genotype for yield and agronomic traits in Buk.xlsx"] 	
   r <- carobiner::read.excel(f, sheet = "Sheet1")
   
   d1  <- data.frame(
      rep= as.integer(r$`Replication number`),
      variety= r$Genotypes,
      emergence_days= r$`Date of emergence`,
      flowering_days= r$DF50,
      yield= r$DPWkgha* 1.18, ## Fresh weight
      dmy_residue= r$DFWkgha,
      seed_weight = r$HSWg,
      harvest_days= NA ## 
   )
   
   d2 <- data.frame(
      rep= as.integer(r$`Replication number`),
      variety= r$Genotypes,
      emergence_days= r$`Date of emergence`,
      flowering_days= r$DF50,
      yield= r$DPWkgha80* 1.18, ## Fresh weight
      dmy_residue= r$DFWkgha80,
      harvest_days= 80 
      
   )
   
   d <- carobiner::bindr(d1,d2)
   d$country= "Nigeria"
   d$location= "Bayero University, Kano"
   d$crop= "groundnut"
   
   ## Adding disease 
   r1 <- carobiner::read.excel(f, sheet = "Sheet2")
   
   dd <- data.frame(
      rep= r1$`Replication number`,
      variety= r1$Genotypes,
      diseases = "rust",
      disease_severity= as.character(r1$RUST90)
   )
   
   d <- merge(d,dd,by=c("rep","variety"),all.x = TRUE)
   
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$irrigated <- NA 
   d$inoculated <- FALSE
   d$yield_part <- "pod"
   d$trial_id <- "1"
   d$planting_date <- "2016"
   d$longitude  <- 8.4269711
   d$latitude  <-  11.9703215
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   carobiner::write_files (path, meta, d) 
   
}