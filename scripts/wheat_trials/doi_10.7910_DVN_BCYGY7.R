# R script for "carob"

carob_script <- function(path) {
  
"Wheat is the most widely grown cereal in Ethiopia next to tef and maize. The residue from wheat is mainly used as livestock feed. In addition to the grain yield, the quantity and quality of wheat straw produced is very important for smallholders and affect variety selection and adoption by farmers. Variations in food-feed traits of different bread wheat cultivars released for the highlands of Ethiopia were evaluated across different districts. This dataset provides grain yield, straw yield, and fodder traits of the straw for different wheat varieties in different districts over two growing seasons."

	uri <- "doi:10.7910/DVN/BCYGY7"
	group <- "wheat_trials"

	ff <- carobiner::get_data(uri, path, group)

	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=2),
		project=NA,
		publication= NA,
		data_institute = "ILRI",
		data_type="experiment",
		treatment_vars = "variety",
		carob_contributor="Fredy Chimire",
		carob_date="2024-3-10"
	)
  
	f1 <- ff[basename(ff) == "001_data-wheat-yield-Ethiopia_Debrezeit_year1.csv"]
	f2 <- ff[basename(ff) == "002_data-wheat-yield-Ethiopia_Kulumsa_year1.csv"]
	f3 <- ff[basename(ff) == "003_data-wheat-yield-Ethiopia_multipleLocations_year2.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)

	r12 <- rbind(r1, r2)	
	d12 <- data.frame(location=r12$Location, variety=r12$Wheat.Variety, dmy_residue=r12$Straw.Yield..ton.ha.*1000, yield = r12$Grain.yield..ton.ha. * 1000)
	d3 <- data.frame(location=r3$Site, variety=r3$Wheat.Variety, dmy_residue=r3$Straw.yield..ton.ha. * 1000, yield=r3$Grain.yield..ton.ha. * 1000)

	d <- rbind(d12, d3) 
	
	d$crop <- "wheat"
	d$planting_date <- "2016"
	d$country <- "Ethiopia"
	d$location <- trimws(d$location)
	
	d$is_survey <- FALSE
	d$yield_part <- "grain"
	d$trial_id <- as.character(as.integer(as.factor(d$location)))
  
	coord <- data.frame(
		location = c("Debre Ziet", "Kulumsa", "Asasa", "Dawa Busa", "Bekoji"), 
		longitude = c(38.9978, 39.1603, 39.2012, 38.0116, 39.2539), 
		latitude = c(8.7657, 8.0199, 7.1076, 8.7771, 7.5267)
	)
	d <- merge(d, coord, by="location")
	
	carobiner::write_files(path, dset, d)
}

