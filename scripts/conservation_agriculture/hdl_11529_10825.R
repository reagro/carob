# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "Description:
  
On-farm demonstration plots were set in Zambia to demonstrate the effects of conservation agriculture (CA) technologies as compared to the traditional farmers practice (ploughing with a mouldboard plough). The CA treatments included basins (BA), ripping (RI) and direct seeding with a direct seeder (DS) and direct seeding with a jab planter (JP). Also superimposed to the treatments are rotations and intercropping of maize with a grain legume (either soyabean or cowpea) and these are compared with continuous maize planting. The study is carried out in various communities of Zambia. Thus, the data set presents yields for maize and the legumes from these sites over 9 seasons (2006-2015). (2016-12-08)

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
    data_citation="Thierfelder, Christian, 2016. Facilitating the widespread adoption of conservation agriculture in maize-based systems in Zambia. https://hdl.handle.net/11529/10825, CIMMYT Research Data & Software Repository Network, V3",
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
  r <- carobiner::read.excel(f, sheet = "Zambia all sites all maize")
  r1 <- carobiner::read.excel(f, sheet = "Zambia all legume all years")
  
    
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
  d$trial_id = as.character(d$treatment)
  
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

