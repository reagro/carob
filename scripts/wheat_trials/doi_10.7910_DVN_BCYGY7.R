# R script for "carob"

# ## ISSUES 

# ....


carob_script <- function(path) {
  
  "Description:
  Wheat is the most widely grown cereal in Ethiopia next to tef and maize. The residue from wheat is mainly used as livestock feed. In addition to the grain yield, the quantity and quality of wheat straw produced is very important for smallholders and affect variety selection and adoption by farmers. Variations in food-feed traits of different bread wheat cultivars released for the highlands of Ethiopia were evaluated across different districts. This dataset provides grain yield, straw yield, and fodder traits of the straw for different wheat varieties in different districts over two growing seasons."
  uri <- "doi:10.7910/DVN/BCYGY7"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "wheat_trials"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    title ="Variations in Food Feed Traits of Bread Wheat Varieties for Ethiopia",
    data_citation="International Livestock Research Institute (ILRI)",
    publication= NA,
    data_institutions = "International Livestock Research Institute (ILRI)",
    data_type="experiment",
    carob_contributor="Fredy Chimire",
    carob_date="2024-3-10"
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=2)
  # dset$license <- "not specified" #carobiner::get_license(js)
  dset$license <- carobiner::get_license(js)

  # Select sheet with revised data from the text file file 
  f <- ff[basename(ff) == "001_data-wheat-yield-Ethiopia_Debrezeit_year1.csv"]
  f1 <- ff[basename(ff) == "002_data-wheat-yield-Ethiopia_Kulumsa_year1.csv"]
  f2 <- ff[basename(ff) == "003_data-wheat-yield-Ethiopia_multipleLocations_year2.csv"]
  # read datasets
  
  r <- read.csv(f)
  r1 <- read.csv(f1)
  r2 <- read.csv(f2)
  
  # Slice columns which match carob columns
  d0 <- data.frame(location= r$Location,variety=r$Wheat.Variety,dmy_residue=r$Straw.Yield..ton.ha.*1000, yield = r$Grain.yield..ton.ha.*1000)
  d1 <- data.frame(location= r1$Location,variety=r1$Wheat.Variety,dmy_residue=r1$Straw.Yield..ton.ha.*1000, yield = r1$Grain.yield..ton.ha.*1000)
  d2 <- data.frame(location= r2$Site,variety=r2$Wheat.Variety,dmy_residue=r2$Straw.yield..ton.ha.*1000, yield = r2$Grain.yield..ton.ha.*1000)
  # for first dataset
  d <- rbind(d0,d1,d2)
  
  d$dataset_id <- dataset_id
  
  
  d$crop <- "wheat"
  d$date <- as.character(2016)
  d$planting_date <- as.character(2016)
  d$country <- "Ethiopia"
  #title <- "Variations in Food Feed Traits of Bread Wheat Varieties for Ethiopia"
  d$location <- trimws(d$location, "right")
  
  #d$is_experiment <- TRUE
  d$yield_part <- "seed"
  
  coord <- list("Debre Ziet" = c(8.7657, 38.9978),
                "Kulumsa" = c(8.0199, 39.1603),
                "Asasa"  = c(7.1076, 39.2012),
                "Dawa Busa"= c(8.7771, 38.0116),
                "Bekoji"  =   c(7.5267, 39.2539))
  d <- d[!is.na(d$dmy_residue), ]
  d$latitude <- unlist(lapply(d$location, function(loc) coord[[loc]][1]))
  d$longitude <- unlist(lapply(d$location, function(loc) coord[[loc]][2]))
  d$trial_id <- as.character(as.integer(as.factor(paste(d$country, d$location))))
  
  carobiner::write_files(dset, d, path=path)
}

