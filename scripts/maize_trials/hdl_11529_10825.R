# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "Description:
  

    [copy the abstract from the repo]

"
  
  uri <- "hdl:11529/10825"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "maize_trials"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    data_citation="Thierfelder, Christian; (CIMMYT, Zimbabwe); ORCID; 0000-0002-6306-7670",
    publication=NA,
    data_institutions = "CIMMYT",
    data_type="experiment",
    carob_contributor="Fredy Chimire" 
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  
  js <- carobiner::get_metadata(dataset_id, path, group, major=3, minor=1)
  dset$license <- carobiner::get_license(js)
  
  
  f <- ff[basename(ff) == "Summary Zambia On-farm Demonstration 2006-2015.xls"]
  
  #r <- read.csv(f)
  r <- readxl::read_excel(f,sheet = 1) |> as.data.frame()
  r1 <- readxl::read_excel(f,sheet = 2) |> as.data.frame()
  
  
  ## process file(s)
  
  
  ## use a subset
  #d <- carobiner::change_names(r, from, to)
  
  d<-r
  
  #### about the data #####
  ## (TRUE/FALSE)
  
  d$dataset_id <- dataset_id
  d$harvest_date<- as.character(d$`1...2`)
  d$on_farm <-TRUE
  d$adm1<-d$`1...3`
  d$is_survey <- FALSE
  #d$is_experiment <- TRUE
  d$irrigated <- FALSE
  ## the treatment code	
  d$treatment <- d$Tmnt.
  d$yield<- d$`Grain yield (kg/ha)`
  d$residue_yield<-d$`Stalk yield (kg/ha)`
  d$crop<-"maize"
  d$rep<- as.integer(d$`Plot No.`)
  d$location<-d$`1...4`
  d$country <-"Zambia"
  d$yield_part <- "grain"
  d$striga_trial <- FALSE
  d$borer_trial <- FALSE
  d$striga_infected <- FALSE
  #d$trial_id <- 
  d <- d %>%
    mutate(trial_id = as.character("treatment"))
  
  d <- d[,c("dataset_id","trial_id","harvest_date","on_farm","adm1","is_survey","irrigated","treatment","yield","residue_yield",
            "crop","rep","location","country","yield_part","striga_trial","borer_trial","striga_infected")]
  
  
  
  
  #d<-d[,(ncol(d)-13):ncol(d)]
  
  # Pick unique coordinates of the location
  locs <- unique(d[,c("country","location","adm1")])
  
  # Get the geo coordinates of the location
  #geocodes <- carobiner::geocode(country=locs$country,location=locs$location,adm1=locs$adm1)
  geocodes <- carobiner::geocode(country=locs$country,location=locs$adm1)
  geocodes1 <- geocodes$df
  
  geocodes2 <- geocodes1 %>%
    rename(longitude=lon,latitude = lat,adm1=location)
  
  mergeddf <- merge(d,geocodes2,by=c("country","adm1"))
  
    
    # all scripts must end like this
    #carobiner::write_files(dset, d, path=path)
    carobiner::write_files(dset, mergeddf, path=path)
}

## now test your function in a clean R environment 
# path <- _____
#carob_script(path)

