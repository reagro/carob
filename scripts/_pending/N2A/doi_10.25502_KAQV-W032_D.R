"
N2Africa farm monitoring - Kenya, 2011
N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries. 
"
carob_script <- function(path) {
  
	uri <- "doi:10.25502/KAQV-W032/D"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	ff	 <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  
	## data set level data 
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
		project="N2Africa",
		publication=NA,
		data_institutions = "IITA",
		carob_contributor="Robert Hijmans",
		carob_date="2023-07-09",
		data_type = "on-farm experiment"
    )
  
  
	
	n2afun <- carobiner::get_function("N2A_monitoring_2", path, group)
	d <- n2afun(ff, path)
	d$dataset_id <- dataset_id
	
	#for now some are crazy high, perhaps different unit?
	d$yield[d$yield > 10000] <- NA
	d <- d[!is.na(d$yield),]
	
	carobiner::write_files(dset, d, path=path)
	
}

