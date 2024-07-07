# R script for "carob"


## It is not clear what the treatments are. "TREATMENT.No." has codes


carob_script <- function(path) {

"This dataset is generated from maize response trials (legume/maize rotation system) that were conducted in Linthipe EPA, Dedza district, and Ntubwi EPA in Machinga district in Malawi, 2019/2020 cropping season. In the previous season, certifies and recycled seed of different varieties of groundnut were grown. In the current season, maize was planted as a test crop."

	uri <- "doi:10.7910/DVN/1T4Q3F"
	group <- "conservation_agriculture"
	ff <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=1),
		data_institute = "IFPRI",
		publication= NA,
		project=NA,
		data_type= "experiment",
		carob_contributor= "Shumirai Manzvera",
		carob_date="2024-02-22"
	)
	f0 <- ff[basename(ff) == "Linthipe_Dedza_MaizeResopnsetoGroundnut_2019_2020.csv"]
	f1 <- ff[basename(ff) == "Mtubwi_Machinga_MaizeResopnsetoGroundnut_2019_2020.csv"]
	r0 <- read.csv(f0)
	r1 <- read.csv(f1)

## use a subset
	d0 <- data.frame(
		adm2=r0$District, 
		adm3=r0$EPA, 
		rep=r0$REPLICATION,
		variety_code=r0$maize.variety.planted.in.2019.2020.season,
	    previous_crop="groundnut", 
		crop_rotation="maize; groundnut",
	    dmy_total=r0$Total.biomass..kg.ha.,
		yield=r0$Grain.yld.ha, 
		treatment=r0$TREATMENT.No.,
		longitude=34.1253751, 
		latitude=-14.1832077, 
		trial_id="1"
	)
					

	d1 <- data.frame(
		adm2=r1$District, 
		adm3=r1$EPA, 
		rep=r1$REPLICATION,
		variety_code=r1$maize.variety.2019.2020.season,
	    previous_crop="groundnut", 
		crop_rotation="maize; groundnut",
	    dmy_total=r1$Total.biomass..kg.ha.,
		yield=r1$Grain.yld..kg.ha.,treatment=r1$TREATMENT.NO.,
		treatment=r1$TREATMENT.No.,
		longitude =35.5737, 
		latitude = -14.9458, 
		trial_id="2"
	)
		
	d <- carobiner::bindr(d0, d1)
	
	d$country<- "Malawi"
	
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	d$planting_date <- "2019"
	d$harvest_date  <- "2020"

	d$crop <- "maize"
	d$yield_part <- "grain"
	d$dmy_total <- gsub(",", "", d$dmy_total)
	d$dmy_total <- as.numeric(d$dmy_total)
	d$treatment <- as.character(d$treatment)
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

	
	carobiner::write_files(meta, d, path=path)
}



