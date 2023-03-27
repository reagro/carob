
"
Title: Grain Yield and Other Agronomic Traits of International Maize Trials – Gabon, 1990

Description: This is an international study that contains data on yield and other Agronomic traits of 
maize including borer and striga attacks on maize in Africa.The study was carried out by the International
Institute of Tropical Agriculture between 1989 and 2015 in over thirty African countries including Gabon.
"
carob_script <- function(path) {
  
  uri <- "https://doi.org/10.25502/20180727/1820/MA"
  dataset_id <- agro::get_simple_URI(uri)
  group <- "variety_performance"
  
  ## data set level data
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication="",
    data_citation = "Menkir, A. (2018) ‘Grain Yield and Other Agronomic Traits of International Maize 
    Trials – Gabon, 1990’. International Institute of Tropical Agriculture (IITA). Available at: 
    https://doi.org/10.25502/20180727/1820/MA",
    data_institutions = "IITA",
    carob_contributor="Rachel Mukami",
    experiment_type="variety_performance",
    has_weather=FALSE,
    has_management=FALSE
  )
  
  ## download and read data
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  dset$license <- carobiner::get_license(js)

  # reading the dataset
  f <- ff[basename(ff) == "International_Maize_Trial_Gabon_Regular.csv"]
  d <- data.frame(read.csv(f))

  d$dataset_id <- dataset_id
  d$trial_id <- d$TRIAL_ID
  d$country <- "Gabon"
  d$location <- tolower(d$LOCATION)
  d$longitude <- d$LONGITUDE 
  d$latitude <- d$LATITUDE
  d$crop <- "maize"
  d$yield <- d$YIELD
  d$on_farm <- "yes"
  d$is_survey <- "no"

  # Extracting coordinates
  
  url <- "https://nominatim.openstreetmap.org/search?q="
  format <- "&format=json"
  locs <- c("country","location")
  places <- d[,c("country","location","latitude","longitude")]
  places <- places[!duplicated(places), ]
  
  get_json <- function(column){
    a <- httr::GET(paste0(url,gsub(" ", "%20", column),format))
    if(a$status_code == 200){
      b <- jsonlite::fromJSON(paste0(url,gsub(" ", "%20", column),format))
      return(b)
    }
  }
  
  for (place in 1:nrow(places)) {
    pcs <- places[place,locs]
    for (col in rev(locs)){
      p <- pcs[,col]
      if (!is.na(p)){
        json <- get_json(p)
        if (length(json) < 1){
          NULL
        }
        else {
          lat <- json[1,"lat"]
          lon <- json[1,"lon"]
          print(lat)
          print(lon)
          places$latitude[place] <- lat
          places$longitude[place] <- lon
          break
        }
      }
    }
  }
 
  d$latitude <- places$latitude
  d$longitude <- places$longitude
  d <- d[,c("dataset_id","trial_id","country","location","longitude","latitude","crop","yield","on_farm","is_survey")]  
  
  # all scripts must end like this
  
  carobiner::write_files(dset, d, path, dataset_id, group)
  TRUE
}
