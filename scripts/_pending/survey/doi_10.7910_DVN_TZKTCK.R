# R script for "carob"

## ISSUES

## NO LON/LAT


carob_script <- function(path) { 
  
  "
This dataset is a result of a household survey of farmers who engaged with the Africa RISING Ethiopia program, for a period of about 4 years. The purpose was to assess the degree of uptake of Africa RISING promoted interventions (farm-integration, sustainable intensification); and to evaluate possible impacts of uptake on various indicators of “sustainability”. The outcome indicators related to agricultural production, human welfare, gender roles, innovation, food security, and poverty.
"

	uri <- "doi:10.7910/DVN/TZKTCK"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=2, minor=3),
		project="AfricaRising",
		publication= NA,
		data_institute = "ILRI",
		data_type="survey", 
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor="Robert Hijmans",
		carob_date="2024-01-29"
	)
  
  

	return(TRUE)	
}

