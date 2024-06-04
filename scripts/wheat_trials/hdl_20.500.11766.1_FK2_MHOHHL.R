# remotes::install_github("reagro/carobiner")

# R script for "carob"

## ISSUES
# ....

path <-setwd (".")
carob_script <- function(path) {
  
  "
	Description:
	The dataset contains the description and results of a field experiment performed under the project
  “Designing InnoVative plant teams for Ecosystem Resilience and agricultural Sustainability (DIVERSify)”
  in Kfardan, Lebanon in 2018. It contains sheets about plot information, plot level data, species level data,
  field metadata and an image of the field plan."
  
  uri <- "hdl:20.500.11766.1/FK2/MHOHHL"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "wheat_trials"
  ## dataset level metadata 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    project="Designing InnoVative plant teams for Ecosystem Resilience and agricultural Sustainability (DIVERSify)” in Kfardan, Lebanon in 2018",	   
    publication=NA,
    data_citation = 'Maalouf, Fouad; Abou-Khater, Lynn, 2020, "DIVERSify field experiment results in Kafardan 2018", https://hdl.handle.net/20.500.11766.1/FK2/MHOHHL, MELDATA, V3',
    data_institutions = "ICARDA",
    carob_contributor="Layal Atassi",
    carob_date="2024-03-24",
    data_type="experiment"
  )
  
  ## download data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=3, minor=0)
  ## what is major and minor
  dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)
  dset$authors <- carobiner::get_authors(js)
  dset$description <- carobiner::get_description(js)
  
  
  
  #read the data file
  
  f1 <- ff[basename(ff) == "03_Species_Level_Data.csv"]
  f2 <- ff[basename(ff) == "01_Plot_Information.csv"]
  
  
  r1 <- read.csv(f1, sep = ";")
  r2 <- read.csv(f2, sep = ";")
  
  
  r1 <- r1[r1$PlantPartner == "Cereal", ]
  r2 <- r2[r2$PlantPartner == "Cereal", ]
  
  # Merge data frames together
  r3 <- merge(x = r1, y = r2, by = "PlotCode", all.x = TRUE)
  # yield with no values =-9999
  r3["GrainYield"][is.na(r3["GrainYield"])]<--9999
  
  # Add columns and change the columns names
  
  d <- carobiner::change_names(r3, c("Site","GrainYield", "Year.x","CropSpeciesCommonName", "CropVariety"), 
                               c("site","yield", "date","crop","variety"))
  
  
  
  #### about the data #####
  
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  
  ##### Location #####
  d$dataset_id <- dataset_id
  d$country <- "Lebanon"
  d$adm1 <- "Baalbek-Hermel"
  d$adm2 <- "Baalbek"
  d$site<- "Kafardan"
  d$rain <- 194.4
  d$elevation <- 1080
  d$latitude<- 	34.00714
  d$longitude<- 36.04647
  
  d$emergence_date <- "2018-03-02"
  

  
  d1 <- d[,c("date","dataset_id","country","adm1","adm2","site","latitude","longitude","on_farm","is_survey","irrigated","elevation","crop","variety",
             "emergence_date","rain", "yield")]   
  
  
  
  # all scripts must end like this
  carobiner::write_files(dset, d1, path=path)
}