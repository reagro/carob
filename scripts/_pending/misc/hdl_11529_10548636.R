# R script for "carob"

## ISSUES
# script does not capture the main treatments 
# probably should have its own group? "tillage"?


carob_script <- function(path) {
  
  "Description:

    [copy the abstract from the repo]

"
  
  uri <- "hdl:11529/10548636"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "wheat_trials"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    data_citation="Verhulst, Nele; Honsdorf, Nora; Mulvaney, Michael J.; Singh, Ravi; Ammar, Karim; Govaerts, Bram, 2021, Nine years of data on genotype by tillage interaction and performance progress for 14 bread and 13 durum wheat genotypes on irrigated raised beds in Mexico, https://hdl.handle.net/11529/10548636, CIMMYT Research Data & Software Repository Network, V2",
    publication="doi.org/10.1016/j.fcr.2017.11.011" ,
    data_institutions = "CIMMYT",
    data_type="experiment", 
    carob_contributor="Mitchelle Njukuya",
    carob_date="2023-02-06",
    revised_by=NA
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)
  dset$authors <- carobiner::get_authors(js)
  dset$description <- carobiner::get_description(js)
  
  
  f <- ff[basename(ff) == "PUB-201-DIB_2021-Data_2021-12-12_corrected.xlsx"]
  
  r <- readxl::read_excel(f) |> as.data.frame()
  r <- readxl::read_excel(f,sheet = "Data", skip = 2)
  r1 <- readxl::read_excel(f,sheet = "Genotype list", skip = 3)
  
  
  ## use a subset
  d <- data.frame(trial_id=r$GID,crop=r$type,planting_date=r$SOW,emergence=r$EMER,flowering=r$FLO,
                  maturity=r$MAT,planting_height=r$HEI,yield=r$YLD)
  d$planting_date <- as.character.Date(d$planting_date)
  d$trial_id <- as.character(d$trial_id)
  d$dataset_id <- dataset_id
  d$on_farm <- TRUE
  d$is_experiment <- TRUE
  d$irrigated <- TRUE
  
  #excel sheet for abbreviations indicated the following system names:
  #PB-FI - Permanents beds, Full irrigation
  #PB-RI - Permanents beds, Reduced irrigation
  #CB-RI - Conventional tilled beds, Reduced irrigation
  #CB-FI - Conventional tilled beds, Full irrigation
  
  d$treatment <- r$syst
  #site information was obtained from publication
  d$country <- "Mexico"
  d$site <- "Sonora"
  d$adm1 <- "Ciudad Obregon"
  d$longitude <- -109.9338
  d$latitude <- 27.4847
  
  #fixing crop names
  #BW -> Bread Wheat, DW -> Durum Wheat
  d$crop <- carobiner::replace_values(d$crop,c("BW","DW"),c("wheat","wheat")) 
  
  #Information on fertilizer application rates was found in the publication 
  
  d$P_fertilizer <- 23
  d$N_fertilizer <- 103
  d$N_fertilizer[d$treatment =="PB-RI"] <- 203 
  d$N_fertilizer[d$treatment =="CB-RI"] <- 203
  d$N_fertilizer[d$treatment =="CB-FI"] <- 178
  d$N_fertilizer[d$treatment =="PB-FI"] <- 178
  d$fertlizer_type <- "urea" 
  d$yield_part <- "grain"
  
  d0<-data.frame(trial_id=r1$`Germplasm ID`,variety=r1$Name)
  
  d <- merge(d,d0,by= 'trial_id')
  
  
  carobiner::write_files(dset, d, path=path)
}



