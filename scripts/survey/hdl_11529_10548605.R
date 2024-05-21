## ISSUES


carob_script <- function(path) { 
  
"
Landscape Diagnostic Survey (LDS) for rice contains farmer's data on current production practices they applied for cultivating rice during 2017 monsoon season. The dataset contains 6857 farmers’ information captured from Bihar, Uttar Pradesh and Odisha states from eastern part of India. The objective of collecting this data is to bridge the existing data-gap and to generate data-based evidence that can help in evidence-based planning. The LDS is designed in a way that data is collected from randomly selected farmers spread uniformly within a KVK (government extension system) domain/district. Survey questionnaire captures all production practices applied by farmers from land preparation to harvesting, including detailed sections on rice establishment, fertilizer use, weed control and irrigation application. Data is captured through electronically enabled Open Data Kit (ODK) tool on mobile phone or tablet. (2021-08-05)"

	uri <- "hdl:11529/10548605"
	group <- "survey"

	ff <- carobiner::get_data(uri, path, group)
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		project="CSISA",
		#data_citation="Ajay, Anurag; Craufurd, Peter; Sharma, Sachin; Ranjan, Harshit; Samaddar, Arindam; Poudel, Gokul; Malik, RK; Singh, Balwinder; Panneerselvam, P; Singh, AK; Rai, Ashok; Keil, Alwin; McDonald, Andrew, 2021. Data on farmers’ rice production practices during 2017 monsoon season from eastern states of India. https://hdl.handle.net/11529/10548605, CIMMYT Research Data & Software Repository Network, V1, UNF:6:kSBKzH9G+QMBbV7uXfhfOA== [fileUNF]",
		publication= NA,
		data_institute = "CIMMYT",
		data_type="survey", 
		treatment_vars = "none",
		carob_contributor="Robert Hijmans",
		carob_date="2024-01-27"
	)
  
	f <- ff[basename(ff)=="CSISA_IND_LDS_Rice_2017_Data.csv"]
	r <- read.csv(f)

	do_LCAS <- carobiner::get_function("do_LCAS", path, group)
	d <- do_LCAS(r)

#	d$longitude <- d$latitude <- NULL
    carobiner::write_files(path, dset, d)
}

