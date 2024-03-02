# R script for "carob"

# putting this here

# this overlaps with 
# doi_10.25502_20180814_1446_HJ.R"
# doi_10.25502_20180814_1446_HJ.R
# and others. 
# But this dataset has the soil chemistry data. 
# So we should probably use this file as a basis and enrich with the other data sources


carob_script <- function(path) {
  
  "Description:
  Omission trials conducted in 5 countries under AfSIS Phase 1 under CIAT
  "
			
	uri <- "doi:10.7910/DVN/C6DIIC"
	group <- "fertilizer" 

	dataset_id <- carobiner::simple_uri(uri)
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=5)

	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group=group),
		project= "AfSIS", 
		data_citation="Kihara, Job; Huising, Jeroen; Nziguheba, Generose; Zingore, Shamie, 2018. Omission trials conducted in 5 countries under AfSIS Phase 1 under CIAT. https://doi.org/10.7910/DVN/C6DIIC, Harvard Dataverse, V2, UNF:6:6Z4vk7GAnLOMtwkK6uparQ== [fileUNF]",
		publication = "doi:10.1016/j.agee.2016.05.012",
		data_institutions = "CIAT",
		data_type="Multi-location trials",
		carob_contributor = "Robert Hijmans",
		carob_date="2024-03-02"
	)

#	carobiner::write_files(dset, d, path=path)
}


