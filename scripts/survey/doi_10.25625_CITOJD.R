## ISSUES

carob_script <- function(path) { 
  
  "
This study collected evidence on the use of early sown wheat varieties and complementary zero tillage technologies in Northwest India. Detailed information on farmers’ knowledge, adoption decisions, personal experience, and perceptions of early sown wheat and zero tillage technologies were collected at the household level using different survey tools. Additional information on agricultural practices during the Rabi Season 2021/22 were collected at the plot level and geocoded. Overall, the dataset comprises responses from 1206 wheat farmers in 70 villages across 7 districts in Punjab and Haryana that were collected between September and October 2022. The villages were selected using stratified random sampling based on a sampling frame of 1722 communities that had been identified as predominantly wheat growing areas based on remote-sensing data from satellite images. The dataset provides rich information that may be used for assessing the diffusion and impact of recently developed wheat varieties designed for early sowing, identifying barriers to the wider adoption of these technologies, and informing policy making aimed at improving adoption and usage decisions of agricultural innovations.
"

  uri <- "doi:10.25625/CITOJD"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "survey"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project="",
    uri=uri,
    data_citation="Naeher, Dominik; Albanna, Basma; Kumar, Abhijeet; Vollmer, Sebastian, 2023. Household and plot-level survey data on adoption, outcomes, and perceptions of early sown wheat and zero tillage in Northwest India. https://doi.org/10.25625/CITOJD, GRO.data, V1, UNF:6:5l5PoTiAWRas7QFCI+GcBA== [fileUNF]",
    publication= NA,
    data_institutions = " Göttingen",
    data_type="survey", 
    carob_contributor="Robert Hijmans",
    carob_date="2024-01-29"
  )
  
  ## download and read data 
  
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=2)
	dset$license <- carobiner::get_license(js)
	dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)

	return(TRUE)	
}

