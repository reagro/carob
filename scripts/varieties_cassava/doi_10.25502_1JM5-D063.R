# R script for "carob"
# license: GPLv3

carob_script <- function(path) {

"Genomic selection cycle 3 set B Advanced Yield Trial for poundability using 21 accessions in Ubiaja for 2019-20 breeding season from 2017gensel.c3b.pyt45.ib"
  
	uri <- "doi:10.25502/1JM5-D063"
	group <- "varieties_cassava"
	ff  <- carobiner::get_data(uri, path, group)
		
	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=3),
		data_institute = "IITA",
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

