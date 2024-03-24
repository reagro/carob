# R script for "carob"

carob_script <- function(path) {
  
  "Description: CIMMYT’s durum wheat breeding program performed parallel selection in conventional tilled (CT) and zero tillage (ZT) soils with the aim to compare the effect of selection under either CT or ZT on the performance of selected progenies. From 16 initial crosses, 234 lines were selected under CT and 250 under ZT. All 484 lines were subsequently tested for yield and growth traits during three seasons (winter 2012-2013, 2013-2014 and 2014-2015) near Ciudad Obregon, Sonora, Mexico in three different testing environments. Those included ZT and CT with full irrigation and CT with reduced irrigation. The experiment was set-up as an alpha lattice design with three replications for each testing environment. Within each replication, genotypes were arranged randomly in three blocks of 160 and 170 genotypes. The dataset includes the following data: days to heading (DH), plant height (PHT), grain yield (GY) and two NDVI values (NDVI1 and NDVI2). Throughout the experiment, NDVI readings were recorded at regular intervals and growth curves were created based on the obtained data. For analysis, two values were selected, one measurement during early vegetative growth (NDVI1), around four weeks after planting, and the second at maximum growth (NDVI2)."  
  uri <- "hdl:11529/10548473"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "wheat_trials"
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    data_citation="Verhulst, Nele (CIMMYT), Ammar, Karim (CIMMYT), Honsdorf, Nora (CIMMYT), Govaerts, Bram, Crossa, Jose (CIMMYT), Vargas, Mateo (CIMMYT)",
    publication=NA,
    data_institutions = "CIMMYT",
    title= "Parallel selection of durum wheat in conventional and zero tillage",
    data_type="experiment", 
    carob_contributor="Fredy Chimire",
    carob_date="2024-03-24"
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  dset$license <- carobiner::get_license(js)
  
  
  f <- ff[basename(ff) == "Database_902-Selection-Honsdorf_et_al.xlsx"]
  r <- readxl::read_excel(f,sheet = "Data")
  
  d <- data.frame(crop="wheat", yield_part="grain",yield= r$YLD*1000, 
                  treatment = r$TEnv,trial_id= as.character(as.integer(as.factor(paste(r$Genotype, r$Plot)))),
                  planting_date= as.character((r$Year)), rep= as.integer(r$Rep)
                  ,height= as.numeric(r$PHT))
  
  #### about the data #####
  
  d$dataset_id <- dataset_id
  d$irrigated <- TRUE
  d$country= "Mexico"
  d$adm1 <- "Ciudad Obregon"

  d$adm2 <- "Sonora"
  d$latitude <- 27.4822
  d$longitude <- -109.9313
  #d$heading <- d
  d$trial_id <- as.character(as.integer(as.factor(paste(r$Genotype, r$Plot))))
  
 carobiner::write_files(dset, d, path=path)
}

