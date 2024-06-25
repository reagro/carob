# R script for "carob"

## ISSUES
# isssue fixing glycine max in previous_crop


carob_script <- function(path) {

"
Within the framework of SATYN, two types of nurseries are produced: SATYN series with odd numbers are lines for drought-stressed areas, and SATYN series with even numbers are lines for heat stress conditions. These nurseries have been phenotyped in the major wheat-growing mega environments through the International Wheat Improvement Network (IWIN) and the Cereal System Initiative for South Asia (CSISA) network, which included a total of 136 environments (site-year combinations) in major spring wheat-growing countries such as Bangladesh, China, Egypt, India, Iran, Mexico, Nepal, and Pakistan. (2020)
"

#### Identifiers
	uri <- "hdl:11529/10548593"
	group <- "wheat_trials"

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)

##### dataset level metadata 
	dset <- data.frame(
		# change the major and minor versions if you see a warning
		carobiner::read_metadata(uri, path, group, major=2, minor=0),
		data_institute = "CIMMYT",
		publication = NA,
		project = "Stress Adapted Trait Yield Nurseries",
		data_type = "experiment",
		treatment_vars = "variety_code;longitude;latitude", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-06-25"
	)
	
##### PROCESS data records
	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)	
	
	#fixing bad data
	d$previous_crop <- gsub("chickpean","chickpea",d$previous_crop)
	d$previous_crop <- gsub(" (glycine max)","soybean",d$previous_crop)
	d$previous_crop <- gsub("spring wheat","wheat",d$previous_crop)
	
# all scripts must end like this
	carobiner::write_files(path, dset, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

