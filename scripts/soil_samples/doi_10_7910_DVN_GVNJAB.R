
carob_script <- function(path) {
  
  "Physical topsoil properties in Murugusi, Western Kenya"
  
  uri <- "doi:10.7910/DVN/GVNJAB"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "soil_samples"
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
  ff <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=4)
  
  # No need to read the table with MIR data
  f <- ff[basename(ff) == "02 soil_samples-data.xlsx"]
  r <- data.frame(carobiner::read.excel(f))

  d <- carobiner::change_names(r[, -c(2:3)], c("Lab_id", "bd_020","TC_020", "clay_020", "sand_020"), 
			c('trial_id', 'soil_bd','soil_total_carbon', 'soil_clay', 'soil_sand'))
					
    v <- terra::vect(as.matrix(r[, c("POINT_X_utm36N", "POINT_Y_utm36N")]), 
							crs="+proj=utm +zone=36")
	v <- terra::project(v, "+proj=longlat") |> terra::crds()
  # Extract latitude and longitude values
	d$latitude <- v[,2]
	d$longitude <- v[,1]
  
	d$country <-  'Kenya'
	
	d$soil_sample_depth_top <- 0
	d$soil_sample_depth_bottom <- 20
	
	carobiner::write_files(dset, d, path=path)
}



