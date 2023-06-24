# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
Genotype ´ environment (G x E) interaction
can be studied through multienvironment trials used 
to select wheat (Triticum aestivum L.) lines. We used spring wheat yield data from
136 international environments to evaluate the predictive ability (PA) of different models
in diverse environments by modeling G X E using the pedigree-derived additive relationship matrix (A matrix).
  
"
  
  uri <- "hdl.handle.net/11529/10831"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "wheat_trials"
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication=NA,#doi: 10.2135/cropsci2016.06.0558
    data_citation = "Sukumaran, Sivakumar; Crossa, Jose; Jarquín, Diego; Reynolds, Matthew, 2016, 
    Yield data for pedigree-based prediction models with 
    genotype × environment interaction in multi-environment 
    trials of CIMMYT wheat, https://hdl.handle.net/11529/10831, CIMMYT Research Data & 
    Software Repository Network, V1",
    data_institutions = "CIMMYT",
    carob_contributor="Cedric Ngakou",
    experiment_type=NA,
    has_weather=FALSE,
    has_management=TRUE
  )
  
  ## download and read data 
  ff <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=2)
  dset$license <- carobiner::get_license(js)
  
  f <- ff[basename(ff) == "1SATYN.csv"] 
  f1 <- ff[basename(ff) == "1WYCYT.csv"]
  f2 <- ff[basename(ff) == "2SATYN.csv"]
  f3 <- ff[basename(ff) == "2WYCYT.csv"]
  f4 <- ff[basename(ff) == "3SATYN.csv"]
  f5 <- ff[basename(ff) == "3WYCYT.csv"]
  f6 <- ff[basename(ff) == "4SATYN.csv"]
  
  # read the dataset
  r <- read.csv(f)
  r1 <- read.csv(f1)
  r2 <- read.csv(f2)
  r3 <- read.csv(f3)
  r4 <- read.csv(f4)
  r5 <- read.csv(f5)
  
  # process file(s)
  d <- rbind(r, r1, r4) # r1,r,r4 have the same variable 
  r5 <- r5[c(2,3,5,1,6)]  
  r2 <- r2[c(1,2,4,5,6)] 
  d <- rbind(d, r5, r2)
 
 ## create a data frame with location , longitude, latitude and country variable
 ## The information come from # doi: 10.2135/cropsci2016.06.0558 # 
 code<- c("BGLD D","BGLD J","BGLD R","China L","Croatia O","Egypt A",
        "Egypt G","Egypt N","Egypt S","Egypt Si","Egypt SK","India D","India H",
        "India I","India K","India L","India U","India V",
        "Iran D","Iran DZ","Iran K","Iran Z","Mex BC","Mex CM","Mex JAL","Mex SIN",
        "Mex SON","Mex-Baj","Nepal B","Pak B","Pak F","Pak I","Pak P","Romania I","SA B")
 
 location<- c("Dinajpur","Joydebpur","Rajshahi","Laomancheng","Osijek","Assiut","Gemmeiza","Nubaria","Sohag",
        "Sids","Sakha","Delhi","Dharwad","Indore","Karna","Ludhiana","Ugar Khurd","Varanasi","Darab-hassan-abad","Dezfoul","Kara",
        "Zargan","Mexicali Baja California","Cd Obregon, Sonora","Tepatitlan Jalisco","Valle del Fuerte, Sinaloa",
        "Valle del Yaqui","Bajio","Bhairahawa","Bahawalpur","Faisalabad","Islamabad","Pirsabak","Fundulea","Bethlehem")
 
 longitude<- c("88.63","90.42","89.037",NA,"18.69","31.20",NA,"30.07","32.09","30.93","30.94","77.22",NA,"75.86","77.47","75.85","74.82","83.007","54.54","48.40","51.38",
               "52.72","-115.47","-99.19","-102.75","-115.44","-98.75","-99.21","83.45","71.66","73.09","73.06",NA,"26.50","28.30")
 
 latitude<- c("25.62","23.99","24.62",NA,"45.55","27.17",NA,"30.66","26.76","28.90","31.09","28.65",NA,"22.72","18.70","30.90","16.67","25.33","28.75","32.37","35.80",
              "29.77","32.64","19.38","20.81","32.63","20.10","19.38","27.51","29.39","31.42","33.69",NA,"44.46","-28.23")
 
 country<- c("Bangladesh","Bangladesh","Bangladesh","China","Croatia","Egypt","Egypt","Egypt","Egypt","Egypt","Egypt","India",
        "India","India","India","India","India","India","Iran","Iran","Iran","Iran","Mexico","Mexico","Mexico","Mexico","Mexico","Mexico",
        "Nepal","Pakistan","Pakistan","Pakistan","Pakistan","Romania","South Africa")
 
  
 
  location <- data.frame(code,location,country,longitude,latitude)
 
 
  # Add location and country in the dataset 
  d <- merge(d, location, by="code")
  #fix long and lat
  d$longitude[d$location=="Dharwad"]<-75.0066516
  d$latitude[d$location=="Dharwad"]<-15.4540505
  d$longitude[d$location=="Pirsabak"]<-72.0393338
  d$latitude[d$location=="Pirsabak"]<- 34.0258704
  d$longitude[d$location=="Gemmeiza"]<-24.2037306
  d$latitude[d$location=="Gemmeiza"]<- 12.6666536 
  
  d$country[d$location=="Gemmeiza"]<-"Sudan" #
  d$location[d$location=="Pirsabak"]<-"Pir Sabak" #
  #Add column
  d$dataset_id <- dataset_id
  d$trial_id<- paste0(d$dataset_id,"-",d$code)
  
  # Extract relevant columns 
  d <- d[,c("dataset_id","trial_id","country","location","longitude","latitude","Rep","YLD")]
  colnames(d)<-c("dataset_id","trial_id","country","location","longitude","latitude","rep","yield") # standard names
  # Add columns
  d$crop<-"wheat"
  d$season<- "sprind"  
  d$on_farm <- FALSE
  d$is_survey <- TRUE
  d$irrigated <- FALSE
  
  

  
  #data type
  d$yield <- (as.numeric(d$yield))*1000 
  d$longitude <- as.numeric(d$longitude)
  d$latitude <- as.numeric(d$latitude)
  
  carobiner::write_files(dset, d, path=path)
  
}

