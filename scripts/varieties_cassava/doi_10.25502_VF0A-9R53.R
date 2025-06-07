# R script for "carob"
# license: GPLv3

carob_script <- function(path) {

"Advanced Yield Trial using 21 accessions  in Ubiaja selected from 15.GS.C1.AYT.20 AND AYT.22(ungenotyped) in IB.IK.and UB"
  
	uri <- "doi:10.25502/VF0A-9R53"
	group <- "varieties_cassava"
	ff  <- carobiner::get_data(uri, path, group)
		
	meta <- data.frame(
		carobiner::get_metadata(uri, path, group, major=1, minor=3),
		data_organization = "IITA",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		carob_contributor = "Robert Hijmans",
		carob_date = "2024-09-18",
		notes = NA
	)

	process_cassava <- carobiner::get_function("process_cassava", path, group)
	d <- process_cassava(ff)
	carobiner::write_files(path = path, metadata = meta, records = d$records, timerecs=d$timerecs)
}

