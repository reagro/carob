# R script for "carob"

# ## ISSUES
"Grain yield not specified if it is kg/ha,
 CP2 is not explicitly defined under column treatment in carob

"

carob_script <- function(path) {
  
  "Description:
	This trial is designed with 1 conventional farmers practice and 4 conservation agriculture (CA) treatments in 5 replications; Plots are subdivided into a continues maize area and a maize/legume (sunnhemp) rotation to investigate the effect of CA practices on soil quality and system productivity. The trial was set in the growing season of 2005 and is still running through to 2017 and beyond. The treatments are as follows: T1. Conventional mouldboard ploughing (CPM): maize with residue removal, manual seeding and fertilization in the tilled seedbed after ploughing. Plots are subdivided into split plots with continues maize and a maize/sunnhemp rotation T2. Sub-soiling with a Magoye ripper (RIM): maize with residue retention, manual seeding and fertilization in the ripping line. Plots are subdivided into split plots with continues maize and a maize/sunnhemp rotation T3. Direct seeding (DSM) with a Fitarelli Jabplanter: maize with residue retention, seeding and fertilization is carried out with the Jabplanter. Plots are subdivided into split plots with continues maize and a maize/sunnhemp rotation T4. Basin Planting (BAM): maize with residue retention, a manual system were basins (at 15cm x 15cm x 15cm spacing) are dug with hoes during the winter period and manually seeded and fertilized at the onset of rains. Plots are subdivided into split plots with continues maize and a maize/sunnhemp rotation T5. Magoye ripping (RI-ML): maize with residue retention, intercropped with cowpea (Vigna unguiculata) at seeding of maize. Plots are subdivided into split plots with continues maize/cowpea pea and a maize/cowpea//sunnh emp rotation.

  "
  
  uri <- "hdl:11529/10843"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "conservation_agriculture"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    data_citation= ______,
    publication= NA,
    data_institutions = "CIMMYT",
    data_type="experiment",
    carob_contributor="Fredy chimire",
    carob_date="2023-08-26"
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=2)
  dset$license <- carobiner::get_license(js)
  
  
  f <- ff[basename(ff) == "Henderson 2005.2016.xlsx"]
  
  # Select sheeet with revised data from the excel file 
  d <- carobiner::read.excel(f, sheet = "All Maize yields Henderson")
  
  ## process file(s)
  
  # selecting columns of interest which match the carob standard format
  d <- d[,c("Harvest year","Country","Location","Crop","Rep","Grainy ield","Trial name","Label")]
  
  d$dataset_id <- dataset_id
  d$on_farm <- FALSE
  d$is_survey <- FALSE
  d$is_experiment <- TRUE
  d$irrigated <- FALSE
  ## the treatment code	
  
  #d$rep
  
  # Assign treatment codes with actual names
  
  treatments_mapping <- c("CPM"="Conventional mouldboard ploughing","RIM"="Sub-soiling with a Magoye ripper",
                       "DSM"="Direct seeding (DSM) with a Fitarelli Jabplanter","BAM"="Basin Planting","RIML"=" Magoye ripping")
                       
  
  d$country <- d$Country
  d$treatment <- treatments_mapping[d$Label]
  d$harvest_date <- d$`Harvest year`
  d$trial_name <- d$`Trial name`
  d$yield_part <- "grain"
  d$yield <- d$`Grainy ield`
  d$rep <- d$Rep
  d$crop <- "maize"
  d$location <- d$Location
  d$latitude <- -17.5585947  #https://vymaps.com/ZW/Henderson-Research-Station-394000277403139/#google_vignette
  d$longitude <- 30.9814704

  
  # all scripts must end like this
 carobiner::write_files(dset, d, path=path)
}


