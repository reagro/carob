0# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
Description:

  On-farm demonstration plots were set in Zambia to demonstrate the effects of conservation agriculture (CA) technologies as compared to the traditional farmers practice (ploughing with a mouldboard plough). 
  The CA treatments included basins (BA), ripping (RI) and direct seeding with a direct seeder (DS) and direct seeding with a jab planter (JP). Also superimposed to the treatments are rotations and intercropping of maize with a grain legume (either soyabean or cowpea) and these are compared with continuous maize planting. The study is carried out in various communities of Zambia. Thus, the data set presents yields for maize and the legumes from these sites over 9 seasons (2006-2015). (2016-12-08)

"
  
	uri <- "hdl:11529/10825"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "conservation_agriculture"
	ff	<- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=3, minor=1)
	## dataset level data 
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
		project=NA,
		publication=NA,
		data_institutions = "CIMMYT",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-08-02",
		data_type="on-station experiment"
    )
  
  
	
	f <- ff[basename(ff) == "Summary Zambia On-farm Demonstration 2006-2015.xls"]
	
	#read the data
	r1 <- carobiner::read.excel(f, sheet = 1, fix=TRUE)
	colnames(r1) <- tolower(colnames(r1))
	colnames(r1)[1:4] <- c("id", "year", "adm1", "village")
	r1[, 5:7] <- NULL
	colnames(r1) <- gsub(".11", "", colnames(r1))
	r1$crop.grown <- "maize"

	r2 <- carobiner::read.excel(f, sheet = 2, fix=TRUE)
	colnames(r2) <- tolower(colnames(r2))
	colnames(r2)[1:3] <- c("id", "year", "adm1")

	r <- carobiner::bindr(r1, r2)
	
	d <- data.frame(
		adm1 = r$adm1, site=r$village, treatment=r$tmnt, 
		crop= tolower(r$crop.grown),
		residue_yield = r$stalk.yield.kg.ha, 
		yield = r$grain.yield.kg.ha,
		rep = as.integer(r$site.rep),
		plant_density = r$final.stand.pl.ha,
		planting_date = as.character(r$year)
	)
	d$crop[d$crop=="cowpeas"] <- "cowpea"

	d$country <- "Zambia"
	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$yield_part <- "grain"

	##really??? 
	###d$planting_date <- "2006"
	###d$harvest_date	<- "2015"

	d$trial_id <- as.character(as.integer(as.factor(paste(d$crop, d$rep))))
	
	p <- carobiner::fix_name(d$treatment)
	p <- gsub("DS", "direct seeder", p)
	p <- gsub("Control plot", "control", p)
	p <- tolower(gsub("^Direct$", "direct seeder",p))
	d$treatment <- p

	d$intercrops <- ifelse(grepl("maize/cowpea int", p), "cowpea", "no crop")
	
	d$crop_rotation <- ifelse(grepl("maize-cowpea rotation", p), "maize; cowpea",
			ifelse(grepl("maize-soybean rotation", p), "maize; soybean",
			ifelse(grepl("soybean-maize rotation", p), "maize; soybean", "no crop")))
													 
	# add longitude and	latitude
	geo <- data.frame(adm1=c("Monze","Kabwe","Chipata","Chibombo","Lundazi","Katete"),
			longitude=c(27.4763925, 28.3992336, 32.6324569, 27.6530228, 33.3487457, 32.04272),
			 latitude=c(-16.2759563, -14.4571147, -13.7478246, -14.8569383, -12.4137188, -14.060241)) 
	
	d <- merge(d, geo, by="adm1", all.x=TRUE)

	# fixes residue_yield and plant_density out of the bounds
	d$residue_yield[d$residue_yield > 100000] <- NA
	d$plant_density[d$plant_density == 0] <- NA
	d <- d[!is.na(d$yield), ]

	carobiner::write_files(dset, d, path=path)
}



