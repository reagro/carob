# R script for "carob"

## ISSUES
# don't know which year this was

# Fertilizer applied at 75 kg N, 20 kg P, and 90 kg K per hectare;
# 12.20 g per plant of NPK 15-15-15 was applied at 4 and 8 WAP; 
# 5.08 g of Urea was applied at 10 WAP ; 
#4.14g per plant of MOP was applied at 12 and 16 WAP													
# MOP=Muriate of potash 
# WAP=Weeks after planting



carob_script <- function(path) {

"
The aim of this reinvestment is to achieve impact at smallholder level at scale through agronomic decision support and tailored advice by sustaining the use of the AKILIMO service for smallholder cassava growers, as well as integrating the tools, approaches and learnings into new initiatives so that these benefit the wider research-and-development community involved in agronomy-at-scale.
"

	uri <- "doi:/10.25502/YZKM-3V47/D"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   project=NA,
	   uri=uri,
	   data_citation="Hauser, S., Kreye, C., Salako, F. K., Ologunde, O., Adebayo, O. E., Busari, M. A., & Olowokere, O. E. (2023). Cassava storage root yield as affected by planting and harvest date and fertilizer and variety in SW Nigeria [Data set]. IITA. https://doi.org/10.25502/YZKM-3V47/D",
	   publication= NA,
	   data_institutions = "IITA",
	   carob_contributor="Robert Hijmans",
	   data_type="experiment"
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)
	dset$license <- carobiner::get_license(js)

	f <- ff[basename(ff) == "Staggered planting FUNNAB"]
	r <- read.csv(f)
	#f <- ff[basename(ff) == "Metadata for staggered planting data"]
	#m <- read.csv(f)

	d <- data.frame(
		dataset_id = dataset_id,
		record_id =r$ID,
		trial_id = "1",
		treatment= apply(r[, c("Planting_date", "Fertilizer", "Cassava_variety", "Harvest_date")], 1, paste, collapse="_"),
		country="Nigeria",
		location= ifelse(r$Location=="fu", "Federal University of Agriculture, Abeokuta, Ogun State (FUNNAB)", "Psaltry International Limited, Ado-Awaye, Oyo state (Ado Awaye)"),
		longitude=NA,
		latitude=NA,
		crop="cassava",
		variety=ifelse(r$Cassava_variety == "V1", "TME 419", "TMS 30572"),
		rep=r$Replication,
		planting_date = as.numeric(gsub("P", "", r$Planting_date)),
		harvest_date = as.numeric(gsub("H", "", r$Harvest_date)),
		fertilizer_type=NA,
		N_fertilizer=0,
		P_fertilizer=0,
		K_fertilizer=0,
		yield_part="roots",
		yield = r$Fresh_root_yield * 1000
	)

	i <- r$Location == "fu"
	d$latitude[i] <- 7.232
	d$longitude[i] <- 3.441
	d$latitude[-i] <- 7.777
	d$longitude[-i] <- 3.41

	i <- d$treatment == "F1"
	d$N_fertilizer[i] <- 75
	d$P_fertilizer[i] <- 20
	d$K_fertilizer[i] <- 90
	d$N_splits[i] <- 2L
	d$fertilizer_type[i] <- "MOP; urea"

## we do not know the year. For now placeholder is 2091/92

	s <- c(4, 6, 8)[d$planting_date]
	h <- c(9, 11, 13)[d$harvest_date]
	h <- formatC(s + h - 12, width=2, flag="0")
	d$harvest_date <- paste0("2092-", h)

	pdates <- paste0("2091-", c("04", "06", "08"))
	d$planting_date <- pdates[d$planting_date]

	carobiner::write_files(dset, d, path=path)
}

