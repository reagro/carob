
# setwd("~/CIMMYT_Geonutrition/carob")
carob_script <- function(path) {
  
  "Physical topsoil properties in Murugusi, Western Kenya"
  
  uri <- "doi:10.7910/DVN/GVNJAB"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "soil"
  dset <- data.frame(
    dataset_id =dataset_id,
    group=group,
    project= NA,
    uri=uri,
    data_citation="Piikki, Kristin; Söderström, Mats; Sommer, Rolf; Da Silva, Mayesse, 2019, 'Physical topsoil properties in Murugusi, Western Kenya', https://doi.org/10.7910/DVN/GVNJAB, Harvard Dataverse, V1, UNF:6:Lxx21ICZO4yG59/xN+m8WQ== [fileUNF]",
    ## if there is a paper, include the paper's doi here
    ## also add a RIS file in references folder (with matching doi)
    publication= NA,
    data_institutions = "CIAT",
    data_type="on-farm experiment", 
    carob_contributor="Andrew M. Sila"  
  )
  
  ## download data from the uri provided
  ff<- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=4)
  
  # No need to read the table with MIR data
  f0 <- ff[basename(ff) == "02 soil_samples-data.xlsx"]
  d0 <- data.frame(carobiner::read.excel(f0))
  

  # Define the UTM zone (Zone 36N in this case)
  utm_zone <- "+proj=utm +zone=36 +datum=WGS84"
  
  # Convert UTM to geographic (latitude and longitude) coordinates
  utm_data_sp <- sp::SpatialPoints(data.frame(d0$POINT_X_utm36N,d0$POINT_Y_utm36N), proj4string = sp::CRS(utm_zone))
  latlong_data <- sp::spTransform(utm_data_sp, sp::CRS("+proj=longlat +datum=WGS84"))
  
  # Extract latitude and longitude values
  d0$latitude <- sp::coordinates(latlong_data)[, 2]
  d0$longitude <- sp::coordinates(latlong_data)[, 1]
  

	# drop columns nitrogen_acid, exna, exbas, ESR, ESP, CaMg
  rd <- which(colnames(d0) %in% c("Lab_id", "POINT_X_utm36N", 'POINT_Y_utm36N'))
  
  d0 <- d0[,-rd]
  
	hd <- c('soil_bd','soil_total_carbon', 'soil_clay', 'soil_sand','latitude','longitude')
	
	colnames(d0) <- hd
	
	d0$country <-  'Kenya'
	
	d0$soil_top <- 0
	
	d0$soil_bottom <- 20
	
	d0 <- d0[,c('longitude', 'latitude', 'country', 'soil_top', 'soil_bottom', 'soil_bd', 'soil_total_carbon', 'soil_clay', 'soil_sand')]

	carobiner::write_files(dset, d0, path=path)
}



