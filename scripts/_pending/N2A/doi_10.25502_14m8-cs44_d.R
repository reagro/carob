"
N2Africa farm monitoring - Kenya, 2011
N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries. 
"
carob_script <- function(path) {
  
	uri <- "doi:10.25502/14M8-CS44/D"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
  
	## data set level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project="N2Africa",
		uri=uri,
		publication=NA,
		data_citation = "Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, P., Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira, L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller, K., Baars, E., & Heerwaarden, J. van. (2020). N2Africa farm monitoring - Malawi, 2011 - 2012 [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/14M8-CS44/D",
		data_institutions = "IITA",
		carob_contributor="Robert Hijmans",
		carob_date="2023-07-09",
		data_type = "on-farm experiment"
    )
  
  ## download and read data 
  
	ff	<- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	dset$license <- carobiner::get_license(js)
	
	n2afun <- carobiner::get_function("N2A_monitoring_2", path, group)
	d <- n2afun(ff, path)
	d$dataset_id <- dataset_id
	
	# longitude might be OK
	d$longitude <- d$latitude <- NULL
	# one record. no yield, to avoid warning, for now
	d <- d[d$crop != "sweetpotato", ]

	carobiner::write_files(dset, d, path=path)

}

