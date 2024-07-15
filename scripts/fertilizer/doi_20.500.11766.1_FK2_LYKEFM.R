# R script for "carob"


carob_script <- function(path) {
  
  "
	Description:

    Final dataset from agronomic experiment in Gumara Maksegnit (2016), as elaborated by GARC researcher in charge for this trial (Baye Ayalew). Please contact author and contact person at ICARDA to obtain more detailed metadata or to propose collaboration.

"
    
  uri <- "doi:20.500.11766.1/FK2/LYKEFM"
  group <- "fertilizer"
  ff <- carobiner::get_data(uri, path, group)
 
  meta <- data.frame(
  	carobiner::read_metadata(uri, path, group, major=2, minor=0),
    publication="hdl:20.500.11766/5084",
    data_institute = "ICARDA",
    carob_contributor="Eduardo Garcia Bendito",
    carob_date="2022-01-20",
    data_type="experiment",
		project=NA  
	)
  
  
  
  # Process both trial/farm sites in a loop, since both have the same structure. Then append them together
  
  dd <- data.frame()
  for (f in (ff[basename(ff) %in% c("Worku_s_Farm.csv", "Mandie_s_Farm.csv")])) {
    d <- read.csv(f, sep=";")
    farm <- gsub("_s_.*", "", gsub(".csv", "", basename(f))) # Get the name of the farm only
    # process file(s)
    
    d$country <- "Ethiopia"
    d$site <- farm
    d$trial_id <- as.character(farm)
    # "The GPS coordinate reference system is unknown" --> (read.csv(ff[basename(ff) == "DataDictionary_Introduction.csv"], sep = ";")[,11])
    # It's assumed to be EPSG:32637 since this is a common system in Ethiopia and the coordinates match a farming region.
    d$latitude <- ifelse(farm == "Mandie", 12.4215, 12.3971)
    d$longitude <- ifelse(farm == "Mandie", 37.5833, 37.5621)
    # There is no year 2 in the original dataset
    d$planting_date <- ifelse(d$Year == 1, "2013-06-03", "2014-06-08")
    d$harvest_date <- ifelse(d$Year == 1, "2013-10-25", "2014-10-27")
    d$on_farm <- TRUE
    d$is_survey <- FALSE
    d$treatment <- "Multi-level application of N fertilizers at different times."
    d$rep <- d$Replicate
    d$crop <- "wheat"
	d$yield_part <- "grain"
	
    d$yield <- d$Yield
# This refers to the treatment levels indicated in "Treatment.csv"	
    d$N_fertilizer <- ifelse(d$Treatment %in% c(1:5), 23, 
                       ifelse(d$Treatment %in% (6:10), 46,
                         ifelse(d$Treatment %in% c(11:15), 69, 0)))
 # This refers to the splits indicated in "Treatment.csv"
    d$N_splits <- ifelse(d$Treatment %in% c(1:3), 2L,
                   ifelse(d$Treatment %in% c(4,9,14), 1L,
                    ifelse(d$Treatment %in% c(5,10,15), 3L, 0L)))
    d$P_fertilizer <- 46/2.29 # as per reference hdl:20.500.11766/5084
    d$K_fertilizer <- 0
	# see "Experimental_Layout.png"
    d$plant_spacing <- 20 

    # Subset to columns of interest
    d <- d[,c("country", "site", "trial_id", "latitude", "longitude", "planting_date", "harvest_date", "on_farm", "is_survey", "treatment", "rep", "crop", "yield", "N_fertilizer", "N_splits", "P_fertilizer", "K_fertilizer", "plant_spacing")]
    dd <- rbind(dd,d) 
  }
	dd$yield_part <- "grain"  
  carobiner::write_files(meta, dd, path=path)
}

