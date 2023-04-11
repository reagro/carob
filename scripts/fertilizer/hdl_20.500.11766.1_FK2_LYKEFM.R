# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
	Description:

    Final dataset from agronomic experiment in Gumara Maksegnit (2016), as elaborated by GARC researcher in charge for this trial (Baye Ayalew). Please contact author and contact person at ICARDA to obtain more detailed metadata or to propose collaboration.

"
  
  uri <- "doi:20.500.11766.1/FK2/LYKEFM"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication="hdl:20.500.11766/5084",
    data_citation = "Ayalew, Baye, 2020, Determination of rate and timing of N application on bread wheat, https://hdl.handle.net/20.500.11766.1/FK2/LYKEFM, MELDATA, V1",
    data_institutions = "International Center for Agricultural Research in the Dry Areas (ICARDA)",
    carob_contributor="Eduardo Garcia Bendito",
    experiment_type="fertilizer",
    has_weather=FALSE,
    has_management=FALSE
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=0)
  dset$license <- carobiner::get_license(js)
  
  # Process both trial/farm sites in a loop, since both have the same structure. Then append them together
  
  dd <- data.frame()
  for (f in (ff[basename(ff) %in% c("Worku_s_Farm.csv", "Mandie_s_Farm.csv")])) {
    d <- read.csv(f, sep=";")
    farm <- gsub("_s_.*", "", gsub(".csv", "", basename(f))) # Get the name of the farm only
    # process file(s)
    d$dataset_id <- dataset_id
    d$country <- "Ethiopia"
    d$site <- farm
    d$trial_id <- paste0(dataset_id, "_", farm)
    # "The GPS coordinate reference system is unknown" --> (read.csv(ff[basename(ff) == "DataDictionary_Introduction.csv"], sep = ";")[,11])
    # It's assumed to be EPSG:32637 since this is a common system in Ethiopia and the coordinates match a farming region.
    d$latitude <- ifelse(farm == "Mandie", 12.4215, 12.3971)
    d$longitude <- ifelse(farm == "Mandie", 37.5833, 37.5621)
    # There is no year 2 in the original dataset
    d$start_date <- ifelse(d$Year == 1, "2013-06-03", "2014-06-08")
    d$end_date <- ifelse(d$Year == 1, "2013-10-25", "2014-10-27")
    d$on_farm <- TRUE
    d$is_survey <- FALSE
    d$treatment <- "Multi-level application of N fertilizers at different times."
    d$rep <- d$Replicate
    d$crop <- "wheat"
    d$yield <- d$Yield
    d$N_fertilizer <- ifelse(d$Treatment %in% c(1:5), 23, # This refers to the treatment levels indicated in "Treatment.csv"
                             ifelse(d$Treatment %in% (6:10), 46,
                                    ifelse(d$Treatment %in% c(11:15), 69, 0)))
    d$N_splits <- ifelse(d$Treatment %in% c(1:3), 2, # This refers to the splits indicated in "Treatment.csv"
                         ifelse(d$Treatment %in% c(4,9,14), 1,
                                ifelse(d$Treatment %in% c(5,10,15), 3, 0)))
    d$P_fertilizer <- 46/2.29 # as per reference https://hdl.handle.net/20.500.11766/5084
    d$K_fertilizer <- 0
	# see "Experimental_Layout.png"
    d$plant_spacing <- 20 

    # Subset to columns of interest
    d <- d[,c("dataset_id", "country", "site", "trial_id", "latitude", "longitude", "start_date", "end_date", "on_farm", "is_survey", "treatment", "rep", "crop", "yield", "N_fertilizer", "N_splits", "P_fertilizer", "K_fertilizer", "plant_spacing")]
    dd <- rbind(dd,d) 
  }
  
  # all scripts must end like this
  carobiner::write_files(dset, dd, path, dataset_id, group)
}

