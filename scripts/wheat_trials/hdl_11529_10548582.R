# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [The experiment was initiated in 2008 and concluded in 2018 to evaluate the performance of durum wheat (Triticum durum L.) under conventionally tilled (CTB) and permanent beds (PB) under two sowing irrigation practices and five nitrogen (N) fertilization treatments in northwestern Mexico. It was located at the Norman E. Borlaug Experiment Station (CENEB) near Ciudad Obregón, Sonora, Mexico (lat. 27°22010″N, long. 109°55051″E, 38 masl) and had a randomized complete block design for four environments (ENV) that combined tillage and sowing irrigation practice: CTB with wet and dry sowing and PB with wet and dry sowing. The PB treatments had been under conservation agriculture for over ten years previously to the experiment. Plots were defined by N fertilizer management, with three replicates. Plots were 3 m wide (4 beds of 0.75 m width) and 10 m long, a space of 30 m2. The CTB were tilled after each crop with a disk harrow to 20 cm depth and new beds were formed. The PB were only reshaped every year in the furrow without disturbing the soil on the bed. In wet sowing, 100-120 mm irrigation was applied two-to-three weeks before sowing; in dry sowing, the field was irrigated one or two days after sowing, which provided higher soil moisture content during germination than wet sowing. Four auxiliary irrigations of 80-100 mm were applied to all plots each cycle. The N fertilizer treatments consisted of a control treatment with no N fertilizer and five treatments with different doses and divisions between first and second fertilization applied as urea. The basal N application was done on the same day as the pre-sowing irrigation, applying the fertilizer in the furrow and incorporating it through irrigation. The N application at first node was completed immediately prior to the first auxiliary irrigation. Nitrogen was applied either once (basal) or split between pre-sowing and first node (split). The data set contains daily weather data for the weather station closest to the experimental site for 2008-2018 (reference evapotranspiration, precipitation, minimum and maximum temperature), yield data (grain yield, biomass yield and straw yield for durum wheat), grain quality data (test weight and thousand kernel weight), and plant physiological data (plant stand, days from flowering to maturity, NDVI) for 2009-2018, grain and straw N data for three years, soil temperature for two years and soil moisture for one year. (2021-07-09)]

"

	uri <- "doi:https://hdl.handle.net/11529/10548582"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "wheat_trials"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=NA,
		project=NA,
		uri=uri,
		data_citation="Provider: CIMMYT Research Data & Software Repository Network,
Content: text/plain; charset=utf-8
TY  - DATA
T1  - Durum wheat performance (10 years of data) and grain quality (three years of data) with two tillage and two sowing irrigation practices under five nitrogen fertilizer treatments in northwestern Mexico
AU  - Verhulst, Nele
AU  - Grahmann, Kathrin
AU  - Honsdorf, Nora
AU  - Govaerts, Bram
DO  - hdl:11529/10548582
ET  - V1
KW  - conservation agriculture
KW  - Yaqui Valley
KW  - nitrogen
KW  - permanent beds
KW  - nitrogen use efficiency
KW  - plant stand
KW  - wet sowing
KW  - dry sowing
LA  - English
PY  - 2021
RI  - null
SE  - 2021-07-09 13:28:01.106
UR  - https://hdl.handle.net/11529/10548582
PB  - CIMMYT Research Data & Software Repository Network",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "Verhulst, Nele; Grahmann, Kathrin; Honsdorf, Nora; Govaerts, Bram, 2021, Durum wheat performance (10 years of data) and grain quality (three years of data) with two tillage and two sowing irrigation practices under five nitrogen fertilizer treatments in northwestern Mexico, https://hdl.handle.net/11529/10548582, CIMMYT Research Data & Software Repository Network V1",
		data_institutions = "CIMMYT",
   		data_type="on-farm experiment",
		carob_contributor="Blessing Dzuda" 
	)

## download and read data 

	path <- ("C:/Users/user/Documents/MY WORKING DIRECTORY/carob")
	uri
	group <- "wheat_trials"
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)
	dset$license <- carobiner::get_license(js)
  path

  f <- "C:/Users/user/Documents/MY WORKING DIRECTORY/carob/data/raw/wheat_trials"
 
  library(readxl)
  DAT_PUB_214DrySow <- read_excel("data/raw/wheat_trials/doi_hdl.handle.net_11529_10548582/DAT-PUB-214DrySow.xlsx", 
                                  +     sheet = "Wheat")
  View(DAT_PUB_214DrySow)
	d <- DAT_PUB_214DrySow
	
	

	
## process file(s)

	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- TRUE
## the treatment code	
	d$rep <- d$REP
	d$irrigated <- d$IRR
	d$N_fertiliser <- d$FERT
	d$harvest_date <- d$Year
	d$yield <- d$`YIELD 12%`
	d$biomass_total <- d$BIOMASS
	d$residue_yield <- d$STRAW
	d$grain_weight <- d$TKW
	d$plant_density <- d$`PLANTS/m² Emerg`
	d$flowering <- d$`DAYS FLOR-MAD`
	
	
	
	
##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "Mexico"
	d$site <- "Norman E. Borlaug Experiment Station"
	d$adm1 <- "Sonora"
	d$adm2 <- "Ciudad Obregón"
	d$elevation <- 40
	
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	carobiner::geocode
	d$longitude <- 27.483274
	d$latitude <- -109.932804

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	carobiner::fix_name
	d$crop <- "wheat"
	d$variety <- "durum"

##### Yield #####
	
	#what plant part does yield refer to?
	d$yield_part <- "grain"
	
	
	d <- d[, c("country","adm1","adm2","site","latitude","longitude","elevation","harvest_date","crop","variety","yield_part","rep","irrigated","N_fertiliser","plant_density","grain_weight","flowering","yield","biomass_total","residue_yield")]
	
	
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)

