# R script for "carob"

# Fertilizer applied at 75 kg N, 20 kg P, and 90 kg K per hectare;
# 12.20 g per plant of NPK 15-15-15 was applied at 4 and 8 WAP; 
# 5.08 g of Urea was applied at 10 WAP ; 
#4.14g per plant of MOP was applied at 12 and 16 WAP													
# MOP=Muriate of potash 
# WAP=Weeks after planting


#Article
# Ologunde O.H, Busari M.A, Adebayo O.E, Olowokere F.A, Kreye C, et al. (2023) Effects of planting date and crop age at harvest on cassava root and starch yield in the derived savanna of southwestern Nigeria. Advances in Agriculture, Horticulture and Entomology: AAHE-182


carob_script <- function(path) {

"
The aim of this reinvestment is to achieve impact at smallholder level at scale through agronomic decision support and tailored advice by sustaining the use of the AKILIMO service for smallholder cassava growers, as well as integrating the tools, approaches and learnings into new initiatives so that these benefit the wider research-and-development community involved in agronomy-at-scale.
"

	uri <- "doi:10.25502/YZKM-3V47/D"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   project=NA,
	   uri=uri,
	   data_citation="Hauser, S., Kreye, C., Salako, F. K., Ologunde, O., Adebayo, O. E., Busari, M. A., & Olowokere, O. E. (2023). Cassava storage root yield as affected by planting and harvest date and fertilizer and variety in SW Nigeria [Data set]. IITA. https://doi.org/10.25502/YZKM-3V47/D",
	   # this DOI is currently not active
	   publication = NA, #"doi:10.37722/AAHAE.2022403",
	   data_institutions = "IITA",
	   carob_contributor="Robert Hijmans",
	   carob_date="2023-07-07",
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
		treatment= apply(r[, c("Planting_date", "Harvest_date", "Fertilizer", "Cassava_variety")], 1, paste, collapse="_"),
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
	d$latitude[!i] <- 7.777
	d$longitude[!i] <- 3.41

	i <- d$treatment == "F1"
	d$N_fertilizer[i] <- 75
	d$P_fertilizer[i] <- 20
	d$K_fertilizer[i] <- 90
	d$N_splits[i] <- 2L
	d$fertilizer_type[i] <- "KCl; urea"

# year from communication with S Hauser. See publication 
	s <- c(4, 6, 8)[d$planting_date]
	h <- c(9, 11, 13)[d$harvest_date]
	h <- formatC(s + h - 12, width=2, flag="0")
	d$harvest_date <- paste0("2018-", h)

	pdates <- paste0("2017-", c("04", "06", "08"))
	d$planting_date <- pdates[d$planting_date]

	carobiner::write_files(dset, d, path=path)
}

