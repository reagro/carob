# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:
CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Temperature Wheat Yield Trial (HTWYT) is a replicated yield trial that contains spring bread wheat (Triticum aestivum) germplasm adapted to Mega-environment 1 (ME1) which represents high temperature areas. (2002)"

	uri <- "hdl:11529/11089"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "wheat_trials"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   project="CIMMYT Semi-Arid Wheat Yield Trial",	   
	   publication=NA,
	   data_citation = "Global Wheat Program; IWIN Collaborators; Singh, Ravi; Payne, Thomas, 2019, '1st to 10th High Temperature Wheat Yield Trial', https://hdl.handle.net/11529/11089, CIMMYT Research Data & Software Repository Network, V4, UNF:6:LSthho4eES7Tm960eaiEwA== [fileUNF]",
	   data_institutions = "CIMMYT",
	   carob_contributor="Andrew Sila",
	   carob_date="2023-05-03",
	   data_type="on-station experiment"
 	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=4, minor=1)
	dset$license <- carobiner::get_license(js)

## process file(s)
	sets <- gsub("_RawData.xlsx", "", grep("RawData", basename(ff), value=TRUE))
	sets <- gsub("_RawData.xls", "", sets)

	proc_wheat <- carobiner::get_function("proc_wheat", path, group)

	d <- vector("list", length(sets))
	for (i in seq_along(sets)) {
		fs <- grep(sets[i], ff, value=TRUE)
		d[[i]] <- proc_wheat(fs)
	}

	dd <- do.call(carobiner::bindr, d)
	dd$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, dd, path=path)
}

