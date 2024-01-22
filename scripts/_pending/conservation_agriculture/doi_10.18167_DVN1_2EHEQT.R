


carob_script <- function(path) {
   
   "
   Description:
   Data on rainfed rice production and management were collected for cropping seasons 2006–2007, 2007–2008, 2008–2009 and 2009–2010 around five villages of the BV-Lac programme on lowland and hillside farmer's fields under conservation agriculture in the Lake Alaotra region of Madagascar, 
   resulting in 3803 site x management x soil x season combinations. (2020-10-07)
"
   
   uri <-  "doi:10.18167/DVN1/2EHEQT"
   dataset_id <- carobiner::simple_uri(uri)
   group <- "conservation_agriculture" 
   ## dataset level data 
   dset <- data.frame(
      dataset_id = dataset_id,
      group=group,
      uri=uri,
      publication= NA,# DOI:10.1017/S0014479714000155
      data_citation ="Bruelle, Guillaume; Domas, Raphael; Andriamalala, Herizo; Hyac, Paulin; Ravonomanana, Jean Eddy, 2020, Rainfed rice yield and management data from 2006 to 2010 on farmer's fields under conservation agriculture in the Lake Alaotra region of Madagascar,
      https://doi.org/10.18167/DVN1/2EHEQT, CIRAD Dataverse, V1, UNF:6:KODF5Pm0fcTqCAFwrvBLRA== [fileUNF]",
      data_institutions = "CIRAD",
      carob_contributor="Cedric Ngakou",
      carob_date="2023-10-18",
      data_type="experiment",
      project=NA 
   )
   
   ## download and read data 
   ff <- carobiner::get_data(uri, path, group)
   js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=2)
   dset$license <- "Open License" # carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)
   
   bn <- basename(ff)
   
   # read and process files
   
   r <- readxl::read_excel(ff[bn=="2006-2010_database_bvlac_bruelle_v01.20201009.xlsx"],sheet=1) |> as.data.frame()
   
   d <- r[,c("id_field","village","crop_season","soil","tillage_system","sow_date","manure","nitrogen","yield","rain_year")]
        colnames(d) <- c("trial_id","location","season","soil_type","tillage","planting_date","OM_applied","N_fertilizer","yield","rain")
   
   ## add columns
   d$country <- "Madagascar"
   d$crop <- "rice"
   d$dataset_id <- dataset_id
   d$yield_part <- "grain" 
   d$on_farm <- TRUE
   d$irrigated <- TRUE
   d$inoculated <- FALSE
   d$is_survey <- FALSE
   d$fertilizer_type <- "urea"
   d$OM_type <- "horse manure"
   d$OM_used <- TRUE

   # fix long and lat
   Geo <- data.frame(location=c("Antsahamamy","Ambohimiarina","Ambohitsilaozana","Ambongabe","Ampitatsimo"),
                    lat=c(-18.9185449,-21.3561474,-17.7013574,-17.706648,-18.6728924),
                    lon=c(47.5591672,47.5679899,48.4656547,48.1885083,47.4563563))
  
    d <- merge(d,Geo,by="location")
   
   d$longitude <- d$lon
   d$latitude <- d$lat
   d$lon <- d$lat <- NULL
   i <- grepl("Y06_07",d$season)
   i1 <- grepl("Y08_09",d$season)
   i2 <- grepl("Y07_08",d$season)
   i3 <- grepl("Y09_10",d$season)
   d$season[i] <- "2006-2007"
   d$season[i1] <- "2008-2009"
   d$season[i2] <- "2007-2008"
   d$season[i3] <- "2009-2010"
   
   # fix fertilize
   d$P_fertilizer <- 0 
   d$K_fertilizer <- 0  
   
   ### NPK amount is given but the rate of N, P and K is missing
	##RH if it missing, we cannot use it.
	
   #data type
   d$planting_date <- as.character(d$planting_date)
   
   # all scripts must end like this
   carobiner::write_files(dset, d, path=path)
   
}

