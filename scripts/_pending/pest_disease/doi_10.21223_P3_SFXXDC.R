# R script for "carob"


carob_script <- function(path) {

"Description:B3C0 is the initial cycle of recombination of group B3- Population B. This population has been derived from population A. R genes have been eliminated from this group and horizontal resistance has been retained. Currently, these clones are being used on the variety selection in developing countries. This group of clones were planted in a randomized complete block design (RCBD) with 2 replicates at Comas, located at 2400 to 2788 masl in Junin situated in the Central mountain ranges in Peru. The trials were established at Comas due to the high disease pressure of late blight present in the area and used to screen selected potato genotypes for resistance to this disease." 


	uri <- "doi:10.21223/P3/SFXXDC"
	group <- "disease"
	ff <- carobiner::get_data(uri, path, group)


	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=3),
	   project=NA,
	   publication= NA,
	   data_institute = "CIP",
	   carob_contributor="Henry Juarez",
	   carob_date="2023-06-21",
		data_type="experiment" 
 	)


	f <- ff[basename(ff) == "PTLB200409_OXAPMP_B3C0OXA05-01.xls"]
	dates <- c("2002-02-03", "2002-02-13", "2002-02-21", "2002-02-28", "2002-03-07")

	proc_lb <- carobiner::get_function("proc_breeding_trial", path, group)
	p <- proc_lb(f, dates, dataset_id)
	d <- p$d

	d$country <- "Peru"
	d$adm1 <- "?"
	d$adm2 <- "?"
	d$adm3 <- "?"
	d$location <- "?"
	d$site <- "?"
	d$elevation <- NA
## each site must have corresponding longitude and latitude
	d$longitude <- NA
	d$latitude <- NA
	
	d$planting_date <- as.character(as.Date("2001-12-10"))
	d$harvest_date  <- as.character(as.Date("2002-04-02"))

	d$pathogen <- "Phytophthora infestans"
	d$diseases <- "potato late blight"
   	d$is_survey = FALSE

	carobiner::write_files(path=path, meta, d, timerecs=p$tim)
}
