# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "Description:
CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Temperature Wheat Yield Trial (HTWYT) is a replicated yield trial that contains spring bread wheat (Triticum aestivum) germplasm adapted to Mega-environment 1 (ME1) which represents high temperature areas. (2022)"

  
  uri <- "hdl:11529/10548944"
  group <- "wheat_trials"
  dataset_id <- carobiner::simple_uri(uri)
  
  #### Download data 
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  
  ##### dataset level metadata 
  dset <- data.frame(
    carobiner::extract_metadata(js, uri, group),
    dta_institutions = "CIMMYT",
    publication= "NA",
    project=NA,
    data_citation="Global Wheat Program (CIMMYT) IWIN CollaboratorsSingh, Ravi (CIMMYT) - ORCID: 0000-0002-4676-5071 Saint Pierre, Carolina - ORCID: 0000-0003-1291-7468",
     daata_type= "experiment",
    carob_contributor= "Njogu Mary",
    carob_date="2024-03-01"
  )
  # read data 
  fenv <- ff[grep("EnvData.xls", basename(ff))]
  floc <- ff[grep("Loc_data.xls", basename(ff))]
  fraw <- ff[grep("RawData.xls", basename(ff))]
  
  if (carobiner::is_excel(floc)) {
    loc <- carobiner::read.excel(floc)
    if (basename(fraw) %in% c("21 HTWYT_RawData.xls")) {
      suppressWarnings(raw <- carobiner::read.excel(fraw, na=c("", "-")))
    } else {
      raw <- carobiner::read.excel(fraw, na=c("", "-", "."))
    }
    env <- carobiner::read.excel(fenv)
    colnames(raw) <- gsub(" ", ".", colnames(raw))                                                       
    colnames(env) <- gsub(" ", ".", colnames(env))
  } else {
    loc <- read.csv(floc, sep = "\t", fileEncoding = "latin1")
    raw <- read.csv(fraw, sep = "\t", fileEncoding = "latin1")	
    env <- read.csv(fenv, sep = "\t", fileEncoding = "latin1")
  }
  

d <- c( 'Trial.name','Loc_no','Country','Loc_desc', 'Trait.name','Cycle', 'Gen_name', 'Rep', 'Plot','Value')
raw <- raw [ , d]
 
loc$latitude <- loc$Lat_degress + loc$Lat_minutes / 60 
loc$longitude <- loc$Long_degress + loc$Long_minutes / 60 


# Merge raw and loc tables to get latlon variables
raw <- merge(raw, loc[, c("Loc_no", "longitude", "latitude")], by ="Loc_no", all.x = TRUE)

envd <- c('Trait.name', 'Value', 'Trial.name', 'Loc_no', 'Country', 'Cycle')
env <- unique(env[,envd])

# take the first in case of duplicates
env <- aggregate(Value ~ ., data=env, \(i) i[1])
env <- reshape(env, idvar=envd[-c(1:2)], timevar = "Trait.name", direction = "wide")
colnames(env) <- gsub("Value.","", colnames(env))

r <- merge(raw, env, by = c("Country", "Loc_no", "Trial.name", "Cycle"), all.x = TRUE) 

r$planting_date <- as.Date(r$SOWING_DATE, "%b %d %Y")


if (!is.null(r$HARVEST_STARTING_DATE)) {
  r$harvest_date <- as.Date(r$HARVEST_STARTING_DATE, "%b %d %Y")
} else {
  r$harvest_date <- as.Date(r$HARVEST_FINISHING_DATE, "%b %d %Y")
}

r$IRRIGATED <- r$IRRIGATED != "YES"
  r$dataset_id <- dataset_id
  r$Country <- "India"

  fertfun <- function(x, v) {
    i <- grep(v, colnames(x))
    #if (length(i) == 0) return(cbind(NA, NA))
    rn <- x[,i,drop=FALSE]
    rn[rn==0] <- NA
    fert <- apply(rn, 1, \(i) sum(as.numeric(i), na.rm=T))
    test <- grepl("P2O5", colnames(rn))
    if (all(test)) {
      fert <- fert / 2.29
    } else if (any(test)) { stop("?") }
    test <- grepl("K2O", colnames(rn))
    if (all(test)) {
      fert <- fert / 1.21
    } else if (any(test)) { stop("?") }
    splits <- apply(rn, 1, \(i) sum(as.numeric(i) > 0, na.rm=T))
    cbind(fert, splits)
  } 
  
  x <- fertfun(r, "FERTILIZER_%N")
  r$N_fertilizer <- x[,1]
  r$N_splits <- as.integer(x[,2])
  
  r$P_fertilizer <- fertfun(r, "FERTILIZER_%P")[,1]
  r$K_fertilizer <- fertfun(r, "FERTILIZE<-R_%K")[,1]
  
  r$crop <- "wheat"
  r$yield_part <- "grain"
  r$location <- r$Loc_desc
  r$trial_id <- r$trial.name
  r$location <- "Nidhona"
 
   # set all colnames to lowercase and subset	
  colnames(r) <- tolower(colnames(r))
  
 
 # Subset for relevant columns
  
z<- c("country","location","trial_id","latitude","longitude", 
                             "planting_date", "harvest_date", "rep", "crop", "n_fertilizer", 
                             "n_splits", "p_fertilizer", "k_fertilizer", "irrigated", 
                             "yield_part", "dataset_id")

    # all scripts must end like this
    carobiner::write_files(dset, d, path=path)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

