## ISSUES


carob_script <- function(path) { 
  
"
Landscape Diagnostic Survey (LDS) for rice contains farmer's data on current production practices they applied for cultivating rice during 2017 monsoon season. The dataset contains 6857 farmers’ information captured from Bihar, Uttar Pradesh and Odisha states from eastern part of India. The objective of collecting this data is to bridge the existing data-gap and to generate data-based evidence that can help in evidence-based planning. The LDS is designed in a way that data is collected from randomly selected farmers spread uniformly within a KVK (government extension system) domain/district. Survey questionnaire captures all production practices applied by farmers from land preparation to harvesting, including detailed sections on rice establishment, fertilizer use, weed control and irrigation application. Data is captured through electronically enabled Open Data Kit (ODK) tool on mobile phone or tablet. (2021-08-05)"

  uri <- "hdl:11529/10548605"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "survey"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project="CSISA",
    uri=uri,
    data_citation="Ajay, Anurag; Craufurd, Peter; Sharma, Sachin; Ranjan, Harshit; Samaddar, Arindam; Poudel, Gokul; Malik, RK; Singh, Balwinder; Panneerselvam, P; Singh, AK; Rai, Ashok; Keil, Alwin; McDonald, Andrew, 2021. Data on farmers’ rice production practices during 2017 monsoon season from eastern states of India. https://hdl.handle.net/11529/10548605, CIMMYT Research Data & Software Repository Network, V1, UNF:6:kSBKzH9G+QMBbV7uXfhfOA== [fileUNF]",
    publication= NA,
    data_institutions = "CIMMYT",
    data_type="survey", 
    carob_contributor="Robert Hijmans",
    carob_date="2024-01-27"
  )
  
  ## download and read data 
  
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	dset$license <- carobiner::get_license(js)
	dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)
	
}

