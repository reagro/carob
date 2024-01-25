# R script for "carob"

## ISSUES
# The yield seem too high, maybe data entry error?


carob_script <- function(path) {
  
  "Description:

    [Fertilizer response trials to compare the performance of 5 ‘best-bet’ fertilizer recommendations with the current blanket
    recommendation in multiple locations and farms.
    The best-bets differ in N:P:K ratios and rates and
    are designed based on assumptions on how fertilizer 
    responses may vary across locations and fields. 
    The five treatments are compared in each site with only 
    the reference treatment replicated.
    Tuber yield as well as secondary agronomic data were assessed. 
    Data were collected using the ODK-based digital data collection
    tool 'Smart Agronomy Data Management System (SAnDMan).]

"
  
  uri <- "doi:10.21223/YACJGV"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    data_citation="Vandamme, Elke ",
    publication= NA,
    data_institutions = "	International Potato Center (CIP)",
    data_type="experiment", 
    carob_contributor="Njogu Mary & Stephen Gichuhi",
    carob_date="2023-01-22",
    revised_by="NA"
  )
  
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  dset$license <- carobiner::get_license(js)
  
  
  f <- ff[basename(ff) == "Scaling_AKILIMO_yield_data.csv"]
  
  r <- read.csv(f)
  
  
  
  ## process file(s)
  
  ## use a subset
  d <- r[,c(7,54,3,14,16,17)]
  
 
 colnames(d) <- c("date","yield","trial_id","country","latitude","longitude")


  d$country <- "Rwanda"
  d$yield <- d$yield*1000 #change from t/ha to kg/ha,value seem too large
  d$dataset_id <- dataset_id
  d$crop <- "potato"
  d$yield_part <-"tubers"
  d$N_fertilizer <- 0
  d$N_fertilizer <- ifelse(r$treat == "NPK6", 51,
                           ifelse(r$treat == "NPK4_MOP2", 34,
                                  ifelse(r$treat == "NPK4_UREA2", 80,
                                         ifelse(r$treat == "NPK4_DAP2", 52, 94))))
  d$K_fertilizer <- ifelse(r$treat == "NPK6", 42,
                           ifelse(r$treat == "NPK4_MOP2", 78,
                                  ifelse(r$treat == "NPK4_UREA2", 28,
                                         ifelse(r$treat == "NPK4_DAP2", 28, 78))))
  d$P_fertilizer <- ifelse(r$treat == "NPK6", 22,
                           ifelse(r$treat == "NPK4_MOP2", 15,
                                  ifelse(r$treat == "NPK4_UREA2", 15,
                                         ifelse(r$treat == "NPK4_DAP2", 32, 41))))
  d$date <- as.character(as.Date(d$date,format= "%d-%b-%y"))
 
  
  # all scripts must end like this
  carobiner::write_files(dset, d, path=path)
}


