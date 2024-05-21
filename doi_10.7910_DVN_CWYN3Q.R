# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
This dataset presents the data used in the study undertaken in the semi-arid area 
of Kiteto District in the Babati Region to address the challenge of soil plow layer
compaction mainly associated with continued use of tractor mounted plow discs and cattle trampling.
Trials were arranged in a mother-baby set up during the 2018/2019 cropping season whereby baby trials
allow wide exposure that enables appropriate socio-economic study conditions. The mother factorial 
experiment was arranged in a split-plot design with two tillage treatments: Conventional farmer practice
i.e. conventional tillage which involves tractor-mounted plow (CT) and rip tillage (RT) and two improved maize 
varieties (commercial maize variety & DT maize Variety) thus giving a total of four treatment combinations.
"

#### Identifiers
	uri <- "doi:10.7910/DVN/CWYN3Q"
	group <- "conservation_agriculture"

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)

##### dataset level metadata 
	dset <- data.frame(
		# change the major and minor versions if you see a warning
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institutions = "Tanzania Agricultural Research Institute",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication = NA,
		project = NA,
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		data_type = "experiment",
		# treatment_vars has semi-colon separated variable names that represent the
		# treatments if the data is from an experiment. E.g. "N_fertilizer;P_fertilizer;K_fertilizer"
		treatment_vars = NA, 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-05-21"
	)
	
##### PROCESS data records

# read data 

	f <- ff[basename(ff) == "Bulk Density at Planting_use of Tractor Mounted trial 2019 Kiperesa.xlsx"]
	f1<- ff[basename(ff) =="Grain yield 2019 Kiperesa.xlsx"]
	#r <- read.csv(f)
	r <- carobiner::read.excel(f, sheet="Bulk Density at planting")
	r1<-carobiner::read.excel(f1, sheet="Grain yield Kiperesa 2019")

## process file(s)

## select the variables of interest and assign them to the correct name
	d0 <- data.frame(
		yield=NA,
		variety=r$Variety,
		treatment=r$`Tillage Method`,
		rep=r$Replication
		# etc
	)

	d1 <- data.frame(
	  variety=r1$Variety,
	  treatment=r1$`Tillage method`,
	  rep=r1$REP,
	  yield=r1$`Maize grain yield, kg/ha`)
	  
	d<- rbind(d0,d1)
	d$rep <- as.integer(d$rep)
#### about the data #####
## (TRUE/FALSE)
	d$on_farm <- TRUE
	d$planting_date <- "2018"
	d$harvest_date  <-"2019"


##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "Tanzania"
	d$site <- "Manyara"
	d$adm1 <- "Kiteto"
	d$elevation <- NA
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <-36.29271 
	d$latitude <- -5.24399

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "maize"

	#what plant part does yield refer to?
	d$yield_part <- "grain"
  d$trial_id="1"
	
# all scripts must end like this
	carobiner::write_files(path, dset, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

