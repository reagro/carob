# R script for "carob"

# ## ISSUES 
# ....


carob_script <- function(path) {
  
  "Description:
  The objective of the study is to test different plant arrangements between maize and Gliricidia sepium and evaluate its effects on soil quality and productivity. Below is the list of treatments applied during the experiment.
Traditional Maize- Groundnuts rotation [with half recommended fertilizer on maize, no fertilizer on groundnuts]
Maize-Groundnut rotation with Gliricidia [ Maize/Gliricidia (COMACO’s Gliricidia spacing: 5m x 1m) – Groundnuts/Gliricidia]
Doubled up Maize-Groundnut rotation with Gliricidia [Maize/Gliricidia (Dispersed shading spacing; 10m x 5m)/pigeonpea – Groundnuts/Gliricidia/Pigeonpea] "
  
  uri <- "doi:10.7910/DVN/O69RDX"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "conservation_agriculture"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    data_citation= "Project Leader : Thierfelder, Christian (International Maize and Wheat Improvement Center (CIMMYT)),Researcher : Mwila, Mulundu (Zambian Agriculture Research Institute (ZARI)),Other : International Maize and Wheat Improvement Center (CIMMYT),Other : Zambian Agriculture Research Institute (ZARI)",
    publication= NA,
    data_institutions = "CIMMYT",
    data_type="experiment",
    carob_contributor="Fredy Chimire",
    carob_date="2024-1-16"
  )
  
  
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=2)
  # dset$license <- "not specified" #carobiner::get_license(js)
  dset$license <- carobiner::get_license(js)
  
  f <- ff[basename(ff) == "AR_ZAM_CIMMYT_Gliricidia_onstation_2020.csv"]
  
  # Select sheet with revised data from the excel file 
  r <- read.csv(f)
  
  d <- data.frame(country= r$Country,harvest_date=r$Year,rep= r$Rep,crop= r$Crop,intercrops=r$Intercrop,adm2=r$District,location=r$Location,dmy_total = r$biomass, yield = r$grainyield)
  
  # for first dataset
  d$dataset_id <- dataset_id
  
  d$is_experiment <- TRUE
  d$on_farm <- TRUE
  # description of treaments
  treatments <- c("Traditional Maize- Groundnuts rotation [with half recommended fertilizer on maize, no fertilizer on groundnuts]",
                  "Maize-Groundnut rotation with Gliricidia [ Maize/Gliricidia (COMACO’s Gliricidia spacing: 5m x 1m) – Groundnuts/Gliricidia]",
                  "Doubled up Maize-Groundnut rotation with Gliricidia [Maize/Gliricidia (Dispersed shading spacing; 10m x 5m)/pigeonpea – Groundnuts/Gliricidia/Pigeonpea]")
  d$treatement <-factor(r$Treat, levels = 1:3,labels = treatments)
  
  d$yield_part <- "grain"
  
  
 
  # https://www.findlatitudeandlongitude.com/l/Msekera+Chipata+Zambia/5548305/
  d$lattitude <- -13.64451
  d$longitude <- 32.6447
  
  carobiner::write_files(dset, d, path=path)
}


