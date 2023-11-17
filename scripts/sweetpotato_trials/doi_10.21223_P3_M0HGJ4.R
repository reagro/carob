

carob_script  <- function(path) {
	
	"
	Description:
	The objectives of this research were to estimate the impact of genotype × environment interactions (G×E) in sweetpotato and select genotypes based on drought indices such as geometric mean, percent yield reduction, drought sensitivity index and harvest index. 
	Experiments were set up at Umbeluzi Research Station, 32 km South of Maputo. A total of 58 clones were evaluated during the dry season of 2006 and 2008. Two treatments were applied for this multi-year trial: full irrigation and without irrigation at the middle of root initiation growth stage.
	The field layout was a randomized complete block design with three replications. ‘Jonathan’, ‘Resisto’ and ‘Tanzania’ were the check cultivars in each treatment. Data were collected on storage root and vine yields in the field and DM in the quality laboratory using root samples from the fields. 
	Computation of Harvest index, geometric means, drought sensitity index, drought intensity index as well as drought tolerance expression were made from the field data. The analysis of variance, regression and the additive main effects multiplicative interaction (AMMI) analyses, plus phenotypic coefficient of variation and ecovalence were used for dissecting the G×E and assessing the stability of each clone.
	
"
	
	uri <- "doi:10.21223/P3/M0HGJ4"
	dataset_id  <- carobiner::simple_uri(uri)
	group <- "sweetpotato_trials"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		uri=uri,
		publication= NA,# 
		data_citation ="Andrade, Maria; Naico, Abdul; Ricardo, Jose; Eyzaguirre, Raul; Makunde, Godwill; Gruneberg, Wolfgang; Ortiz, Rodomiro, 2016, Replication Data for: Genotype x environment interaction and selection for drought adaptation in sweetpotato (Ipomoea batatas [L.] Lam.) in Mozambique., 
		https://doi.org/10.21223/P3/M0HGJ4, International Potato Center, V1",
		data_institutions = "CIP",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-11-06",
		data_type="experiment",
		project=NA 
	)
	
	## download and read data 
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=11)
	dset$license <- carobiner::get_license(js)
	
	bn <- basename(ff)
	
	# read file
	r <- carobiner::read.excel(ff[bn=="Drought_06_08.xls"])  
	
	d <- r[,c("YEAR","TREATMENT","REP","CULTIVAR","RYTHA","FYTHA","BIOM")]
	colnames(d) <- c("planting_date","irrigation","rep","treatment","yield","biomass_leaves","dmy_total")
	

	## add columns
	d$crop <- "sweetpotato" 
	d$dataset_id <- dataset_id
	d$country <- "Mozambique"
	d$adm1 <- "Maputo"
	d$location <-  "Umbeluzi"
	d$trial_id <- paste(d$treatment,d$location,sep = "_")
	d$yield_part <- "roots"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	i <- grepl("Non_Irrigation",d$irrigation)
	d$irrigated[i] <- FALSE
	d$irrigation <- NULL
	### add long and lat coordinate
	d$longitude <- 32.3777344
	d$latitude <- -26.0299562
	
	## fix yield value
	d$yield <- gsub("\\*",NA,d$yield)
	d$dmy_leaves <- gsub("\\*", NA, d$dmy_leaves)
	d$dmy_total <- gsub("\\*", NA, d$dmy_total)
	d <- d[!is.na(d$yield),] ## remove NA in yield	
	#data type
	d$rep <- as.integer(d$rep)
	d$yield <- (as.numeric(d$yield))*1000 # in kg/ha
	d$dmy_leaves <- (as.numeric(d$dmy_leaves))*1000 # in kg/ha
	d$dmy_total <- (as.numeric(d$dmy_total))*1000 # in kg/ha
	d$planting_date <- as.character(d$planting_date)
	
	### fix the bounds error 
	d$dmy_leaves[d$dmy_leaves>20000] <- NA 
	##CN
	# Most biomass_leaves values are out of range. Could we perhaps review the maximum biomass_leave limit in record_crops.csv?
	
	# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
	
}



