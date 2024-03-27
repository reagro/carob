# R script for "carob"

## ISSUES
# 1. Coordinates could not be found by the geocode, manually entered coordinates
#    for respective sites, coordinates extracted from geonames website.
# 2. 2 entries in yield had NA values. Replaced NA values with 0.


carob_script <- function(path) {

"
[Conservation agriculture involves reduced tillage, diversification of plant associations, and retention of crop residues to maintain soil cover. However, there is knowledge gap on the appropriate rate of application and interactive effect of residues and nitrogen as in some situations cases of nitrogen lock-up have been reported. This present data set addresses the effects of different nitrogen and residue levels on maize productivity, soil temperature, soil moisture and soil structure in contrasting soil types over 6 seasons. The trials were set across southern Africa i.e. Malawi, Mozambique, Zambia and Zimbabwe. The treatments were as follows: Main treatments: 1. Conventional tillage 2. No-tillage, 0 t/ha residues 3. No-tillage, 2 t/ha residues 4. No-tillage, 4 t/ha residues 5. No-tillage, 6 t/ha residues 6. No-tillage, 8 t/ha residues, Subtreatments: 1. 0 N 2. 30N (200 kg/ha Compound D – 46 kg/ha AN 3. 90N (200 kg/ha Compound D –220 kg/ha AN) The measured attributes are as follows: 1. Maize and grain yields 2. Soil profile temperature 3. Soil profile mositure 4. Normalized difference vegetation index (NDVI)]
"

#### Identifiers
	uri <- "hdl:11529/10868"
	group <- "conservation_agriculture"

# the script filename should be paste0(dataset_id, ".R")
	dataset_id <- carobiner::simple_uri(uri)

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)

##### dataset level metadata 
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
		data_citation="Thierfelder, Christian; Mhlanga, Blessing, 2017, Interaction effects of different residue and nitrogen levels on maize growth, yield, soil parameters, and N leaching, https://hdl.handle.net/11529/10868, CIMMYT Research Data & Software Repository Network, V1",
		data_institutions = "CIMMYT",
		publication=NA,
		project=NA,
		data_type= "experiment",
		carob_contributor= "Blessing Dzuda",
		carob_date="2024-03-26"
	)
	
##### PROCESS data records

# read data 

	f <- ff[basename(ff) == "Residue Level Trial.xlsx"]
	r <- carobiner::read.excel(f)

## process file(s)

## use a subset
	d <- data.frame(crop="maize", 
					        country=r$Country,
					        site=r$Site,
					        planting_date=r$Season,
					        treatment=r$Treatment,
					        rep=r$Replicate,
					        dmy_total=r$`Biomass (kg/ha)`,
					        yield_part="grain",
					        yield=r$`Grain (kg/ha)`)

	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <-FALSE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- FALSE

	d$site<-carobiner::replace_values(d$site,"DTC","Domboshawa Training Centre")
	d$site<-carobiner::replace_values(d$site,"MFTC","Monze Farmer Training Centre")
	d$site<-carobiner::replace_values(d$site,"SRS","Sussundenga Research Station")
	d$site<-carobiner::replace_values(d$site,"UZ","University of Zimbabwe")
	
	
##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
##
	d$latitude[d$site=="Domboshawa Training Centre"]<- 31.1253
	d$latitude[d$site=="Monze Farmer Training Centre"]<- 27.4733
	d$latitude[d$site=="Monze"]<- 27.4733
	d$latitude[d$site=="Chitedze"]<- 33.8525
	d$latitude[d$site=="Makoholi"]<- 30.7715
	d$latitude[d$site=="Sussundenga Research Station"]<- 33.2953
	d$latitude[d$site=="University of Zimbabwe"]<- 31.0546
	
	d$longitude[d$site=="Domboshawa Training Centre"]<- -17.6738
	d$longitude[d$site=="Monze Farmer Training Centre"]<- -16.2803
	d$longitude[d$site=="Monze"]<- -16.2803
	d$longitude[d$site=="Chitedze"]<- -13.8362
	d$longitude[d$site=="Makoholi"]<- -19.8330
	d$longitude[d$site=="Sussundenga Research Station"]<- -19.4130
	d$longitude[d$site=="University of Zimbabwe"]<- -17.7824
	
	d$trial_id<- paste0(d$dataset_id,"_",d$rep)
	
	##### Time #####
	## time can be year (four characters), year-month (7 characters) or date (10 characters).
	d$planting_date <- as.character(as.Date(d$planting_date))
	
	##### Yield #####
	d$yield <- as.numeric(d$yield)
	d$yield[is.na(d$yield)] <-0
	
	d$treatment<- as.character(d$treatment)
	d$rep<-as.integer(d$rep)
	
# all scripts must end like this
	carobiner::write_files(path, dset, d)
}

