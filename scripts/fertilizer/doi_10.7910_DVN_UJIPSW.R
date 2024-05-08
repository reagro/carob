# R script for "carob"

## EGB
# Related to scripts/fertilizer/doi_10.7910_DVN_1A6WMD.R


carob_script <- function(path) {

"
Retaining crop residues in fields is a pathway to build SOM on smallholder farmers.
The quality of crop residues can be improved through integrating more legume residues.
This experiment assess the effect of legume residues from a doubled up system as compared
to maize residues on rotational maize and N dynamics in the short term.
In the long term, this can increase both quantity and quality of SOM
"

#### Identifiers
	uri <- "doi:10.7910/DVN/UJIPSW"
	group <- "fertilizer"

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)

##### dataset level metadata 
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institutions = "International Institute of Tropical Agriculture (IITA)",
		publication = "doi:10.1016/j.fcr.2021.108225",
		project = "Africa RISING",
		data_type = "experiment",
		carob_contributor= "Eduardo Garcia Bendito",
		carob_date="2024-05-07"
	)
	
##### PROCESS data records

# read data 

	f1 <- ff[basename(ff) == "Old design_maize yields.csv"]
	r1 <- read.csv(f1)
	r1[r1 == ""] <- NA # Remove empty strings
	r1 <- r1[rowSums(is.na(r1)) != ncol(r1), ] # Remove empty rows
	f2 <- ff[basename(ff) == "Old design_gnut yields.csv"]
	r2 <- read.csv(f2)
	r2[r2 == ""] <- NA # Remove empty strings
	r2 <- r2[rowSums(is.na(r2)) != ncol(r2), ] # Remove empty rows

# "streamlite"; highcharts

## process file(s)

	d1 <- data.frame(
		crop = "maize",
		trial_id = r1$Farmer.code,
		location = r1$EPA,
		t = r1$Treatment.number,
		yield = r1$Grain.weight..kg.ha.,
		residue_yield = r1$stover.dry.weight..kg.ha.,
		dmy_total = r1$Total.biomass..kg.ha.,
		row_spacing = r1$Distance.between.ridges..m.*100 # to cm
	)
	d2 <- data.frame(
	  crop = "groundnut",
	  variety = r2$Variety,
	  trial_id = r2$Farmer.code,
	  location = r2$EPA,
	  t = r2$Treatment.number,
	  yield = r2$Grain.weight..kg.ha.,
	  residue_yield = r2$stover.dry.weight..kg.ha.,
	  dmy_total = r2$Total.biomass..kg.ha.,
	  row_spacing = r2$Distance.between.ridges..m.*100 # to cm
	)

	d <- carobiner::bindr(d1, d2)
	
#### about the data #####
	d$on_farm <- TRUE
	d$is_survey <- FALSE

## the treatment code	
	d$treatment <- NA
	d$treatment <- ifelse(d$t == 1, paste0("N69P9K0"),
	                      ifelse(d$t == 2, paste0("N0P0K0"), paste0("N34P4K0")))

##### Location #####
## EGB:
# For Ntiya (EPA Mtiya of Malawi) ==> https://www.google.com/maps/place/Kwilembe,+Malawi
	d$country <- "Malawi"
	d$longitude <- ifelse(d$location == "Nsanama", 35.507,
	                      ifelse(d$location == "Nyambi", 35.573, 35.395))
	d$latitude <- ifelse(d$location == "Nsanama", -14.964,
	                      ifelse(d$location == "Nyambi", -14.689, -14.352))

##### Crop #####
	d$intercrops <- NA
	d$intercrops[d$t %in% c(3, 7, 8, 9)] <- "pigeon pea"
	d$variety[d$crop == "maize"] <- "DKC 8033"

##### Time #####
	d$planting_date <- as.character(ifelse(d$crop == "maize", "2018", "2017"))

##### Fertilizers #####
## EGB:
## Based on publication experimental design
	d$N_fertilizer <- ifelse(d$t == 1, 69,
	                      ifelse(d$t == 2, 0, 34.5))
	d$N_splits <- as.integer(2)
	d$P_fertilizer <- ifelse(d$t == 1, 9,
	                         ifelse(d$t == 2, 0, 4.5))
	d$K_fertilizer <- 0 # from pub
	d$fertilizer_type <- "NPK;urea"

##### Yield #####
	d$yield_part <- "grain"
	
	d <- d[, -which(names(d) %in% c("t"))]

# all scripts must end like this
	carobiner::write_files(path, dset, d)
}
