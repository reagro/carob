# R script for EiA version of"carob"

## ISSUES
# 1. DOI and much of the metadata is missing
# 2. Data reads are still unstable and user needs to have access
# 3. License is missing (CC-BY)?
# 4. Many valuable variables that need to be integrated still...
# 5. ...

carob_script <- function(path) {
  
  "
	SOME DESCRIPTION GOES HERE...

"
  
  uri <- "doi:10.7910/DVN/F3VTAL"
  group <- "eia"
  ff <- carobiner::get_data(uri, path, group)
  
  dset <- data.frame(
    # Need to fill-in metadata...
    carobiner::read_metadata(uri, path, group, major=1, minor=0),
    # uri = carobiner::simple_uri(uri),
    # dataset_id = uri,
    # authors = "Christine Kreye",
    # data_institutions = "International Institute of Tropical Agriculture (IITA)",
    # title = NA,
    # description = "Validations of the SAA Nigeria Use Case MVP",
    # group = group,
    # license = 'Some license here...',
    carob_contributor = 'Eduardo Garcia Bendito',
    # data_citation = '...',
    project = 'Excellence in Agronomy - GAIP Ghana validations',
    data_type = "survey", # or, e.g. "on-farm experiment", "survey", "compilation"
    carob_date="2024-05-22"
  )
  
  # Survey
  f <- ff[basename(ff) == "EiA Beneficiaries Profile _responses_download.xlsx"]

  # Read relevant file
  ds <- carobiner::read.excel(f, sheet = "Survey Responses")
  
  locs <- data.frame(sapply(as.data.frame(do.call(rbind, strsplit(ds$`Auto Recorded GIS`, ","))), function(x) as.numeric(as.character(x))))
  colnames(locs) <- c("latitude", "longitude", "acc")
  # crop
  crops <- data.frame(sapply(as.data.frame(do.call(rbind, strsplit(ds$`Which crops did you grow last farming season?`, ","))), function(x) tolower(gsub("\\s*\\([^\\)]+\\)", "", x))))
  colnames(crops) <- c("crop", "crop2", "crop3")
  crop <- crops$crop # Assuming first to be the main crop
  # intercrops
  icrop <- data.frame(sapply(as.data.frame(do.call(rbind, strsplit(ds$`If OTHERS in Question, kindly specify`, paste0(c(", ", " and "), collapse = "|")))), function(x) tolower(gsub("\\s*\\([^\\)]+\\)", "", x))))
  icrops <- gsub("NA", "", paste(icrop$V1, ifelse(icrop$V1 != icrop$V2, icrop$V2, NA), sep = "; "))
  icrops <- ifelse(startsWith(icrops , "; "), NA, icrops)
  icrops <- ifelse(endsWith(icrops , "; "), gsub("; ", "", icrops), icrops)
  
  d <- data.frame(
    country = "Ghana",
    location = data.frame(sapply(ds$`Name of community`, function(x) gsub("\\s*\\([^\\)]+\\)", "", x)))[[1]],
    longitude = locs$longitude,
    latitude = locs$latitude,
    crop = crop,
    intercrops = icrops, # could also be crop_rotation
    land_tenure = ifelse(ds$`Do you own land(s) in the community?` == "(A)Yes", "own", "lease"),
    land_ownedby = data.frame(sapply(ds$`If YES, what type of ownership?`, function(x) gsub("\\s*\\([^\\)]+\\)", "", x)))[[1]],
    cropland_used = data.frame(sapply(ds$`How many acres of the land do you allocate for farming?`, function(x) gsub("\\s*\\([^\\)]+\\)", "", x)))[[1]],
    fertilizer_type = gsub(",", "; ", data.frame(sapply(ds$`What type of fertilizers do you use?`, function(x) gsub("\\s*\\([^\\)]+\\)", "", x))))[[1]]
  )
  
  # Fix land area
  d$cropland_used[grep("1-4", d$cropland_used)] <- 2
  d$cropland_used[grep("5-10", d$cropland_used)] <- 7.5
  d$cropland_used[grep("16 and above", d$cropland_used)] <- 16
  
  carobiner::write_files(dset, d, path=path)
}
