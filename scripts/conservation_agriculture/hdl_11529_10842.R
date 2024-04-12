# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
[This data set is from a long-term (2010-2016) trial set in sandy soils. The 
study seeks to monitor and evaluate the effects over time of conservation 
agriculture (CA) practices on crop yield, soil quality, weeds, pests and diseases.
The trial was set as a randomised complete block design with the following 
treatments:T1: Check plot (CP); traditional farmers practice using the mouldboard
plough, maize as a sole crop, no residue retention, stubbles incorporated T2: 
Direct seeding with animal drawn seeder (DSM), maize as a sole crop, residue 
retention (at a rate of 2.5-3 t ha-1 in the first year, thereafter all crop 
residues retained) T3: Basin (BAM), maize as a sole crop, residue retention T4: 
Jab planter (JPM), maize as a sole crop, residue retention T5: Direct seeding 
with animal drawn seeder (DSMB), biochar incorporated, maize as a sole crop, 
residue retention T6: Direct seeding with animal drawn seeder (DSMP), maize-pigeon
pea (Cajanus cajan) intercropping, residue retention T7: Crop rotation A1 (A1M): 
direct seeding with animal drawn seeder, maize-groundnut rotation (Phase 1), 
residue retention; Maize- Groundnut T8: Crop rotation A2(A2G): direct seeding 
with animal drawn seeder, maize-groundnuts rotation (Phase 2), residue retention;
Groundnuts- Maize T9: Crop rotation B1 (B1M): direct seeding with animal drawn 
seeder, maize-sunflower rotation (Phase 1), residue retention; Maize- Sunflower 
T10: Crop rotation B2 (B2S): direct seeding with animal drawn seeder, maize-sunflower
rotation (Phase 2), residue retention; Sunflower- Maize. (2016)]
"

#### Identifiers
	uri <- "hdl:11529/10842"
	group <- "conservation_agriculture"

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)

##### dataset level metadata 
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=2),
		data_institutions = "CIMMYT",
		publication= NA,
		project=NA,
		data_type= "experiment",
		carob_contributor= "Blessing Dzuda",
		carob_date="2024-04-12"
	)
	
##### PROCESS data records

# read data 

	f <- ff[basename(ff) == "Domboshawa 2010.2016.xlsx"]
	r <- carobiner::read.excel(f)

## process file(s)

## use a subset
	d1 <- data.frame(planting_date=as.character(r$year),
				      	 country=r$Country,
				      	 location=r$Location,
				      	 site="Domboshawa Training Centre",
				      	 treatment=as.character(r$Tmnt.),
				      	 crop=tolower(r$Crop),
				      	 rep=as.integer(r$Rep),
				      	 dmy_total=r$`Biomass yield (kg/ha)`,
				      	 yield_part="grain",
				      	 yield=r$`Grain/cotton yield (kg/ha)`,
				      	 latitude="-17.60527",
				      	 longitude="31.13669",
				      	 trial_id="1")
	d1$latitude<- as.numeric(d1$latitude)
  d1$longitude<- as.numeric(d1$longitude)
  d1$crop<- gsub("maize/ppea","maize,pegion pea",d1$crop)
  d1$crop<- carobiner::replace_values(d1$crop,"maize+cowpea","maize,cowpea")
  #changing intercropping symbols, although they will alter the meaning of cropping strategy

#### about the data #####
## (TRUE/FALSE)
	d1$on_farm <- FALSE
	d1$is_survey <- FALSE
	d1$irrigated <- FALSE
	
##################################  LEGUMES DATA  ##############################
	
	r2 <- carobiner::read.excel(f, sheet ="All legumes DTC")
	d2 <- data.frame(planting_date=as.character(r2$Year),
	                 country=r2$Country,
	                 location=r2$Location,
	                 site="Domboshawa Training Centre",
	                 treatment=as.character(r2$Tmnt.),
	                 crop=tolower(r2$Crop),
	                 rep=as.integer(r2$Rep),
	                 dmy_total=r2$`Biomass yield (kg/ha)`,
	                 yield_part="grain",
	                 yield=r2$`Grain yield (kg/ha)`,
	                 latitude="-17.60527",
	                 longitude="31.13669",
	                 trial_id="2")
	d2$latitude<- as.numeric(d2$latitude)
	d2$longitude<- as.numeric(d2$longitude)
	d2$on_farm <- FALSE
	d2$is_survey <- FALSE
	d2$irrigated <- FALSE
	d2$crop<- gsub("groundnuts|grountnuts","groundnut",d2$crop)
	
	d<-rbind(d,d2)

# all scripts must end like this
	carobiner::write_files(path, dset,d)
}


