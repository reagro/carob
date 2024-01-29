## ISSUES

## NO LON/LAT


carob_script <- function(path) { 
  
  "
This dataset is a result of a household survey of farmers who engaged with the Africa RISING Ethiopia program, for a period of about 4 years. The purpose was to assess the degree of uptake of Africa RISING promoted interventions (farm-integration, sustainable intensification); and to evaluate possible impacts of uptake on various indicators of “sustainability”. The outcome indicators related to agricultural production, human welfare, gender roles, innovation, food security, and poverty.
"

	uri <- "doi:10.7910/DVN/TZKTCK"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "survey"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project="AfricaRising",
		uri=uri,
		data_citation="Africa RISING Ethiopia RHoMIS 2018International Livestock Research Institute (ILRI), 2020. Africa RISING Ethiopia RHoMIS 2018. https://doi.org/10.7910/DVN/TZKTCK, Harvard Dataverse, V2, UNF:6:v7u19Y327culqdGpp+CNMw== [fileUNF]",
		publication= NA,
		data_institutions = "ILRI",
		data_type="survey", 
		carob_contributor="Robert Hijmans",
		carob_date="2024-01-29"
	)
  
  ## download and read data 
  
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=3)
	dset$license <- carobiner::get_license(js)
	dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)

	return(TRUE)	
}

