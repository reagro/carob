# R script for "carob"

## ISSUES


carob_script <- function(path) { 
  
"
The objective of this survey was to identify the major wheat productivity and profitability drivers in Nepal Terai. Nepal Terai is considered as the part of Indo-Gangetic Basin and its the major cereal production domain in Nepal. In year 2016, immediately following the wheat harvest, this survey was conducted. A total of 10 districts were purposively selected for the survey based on the highest wheat acreage. In each district, a total of five sub-districts were further selected purposively based on the highest wheat acreage. In each sub-districts (or village development committees: VDCs) a total of 10 wheat producing farms were selected randomly from the farmers name list provided by the village level administrative authorities. The overall sampling frame consists of 10 samples from each VDC × 5 sub-districts (VDCs) in each district × 10 districts in Nepal Terai = 500 samples. Moreover, in order to check the farmers self reported yield, 50% farmers largest plots were selected for the crop cuts and the crop cuts data are also available with this dataset. These crop cuts were conducted prior to the survey – during the time when farmers harvest their wheat crop. Farmers may have multiple plots and asking the data from each plot may reduce the data quality. Therefore, to increase the precision and collect the quality data, farmers inputs (e.g., seed, fertilizer, weed management practices, varieties, sowing time, harvesting time, and other crop management practices), and outputs were asked only for largest plot. Data were collected through a direct farm visit and paper-based survey was deployed. The details of the questions asked, codebook, their description, meta-data, and data can be found in this dataset. (2021-09-24)"

	uri <- "hdl:11529/10548615"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		project="CSISA",
		#data_citation="Gokul P. Paudel; Andrew J. McDonald, 2021. Data on identifying sustainable wheat productivity drivers in Nepal’s Terai. https://hdl.handle.net/11529/10548615, CIMMYT Research Data & Software Repository Network, V1, UNF:6:w5/k07NPArxJgX7NKnRMTA== [fileUNF]",
		publication= NA,
		data_institute = "CIMMYT",
		data_type="survey", 
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor="Robert Hijmans",
		carob_date="2024-01-27"
	)
  
	return(TRUE)	
	
}

